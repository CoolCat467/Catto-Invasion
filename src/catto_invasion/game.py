"""Graphical Catto Game."""

# Programmed by CoolCat467

# Copyright (C) 2024-2025  CoolCat467
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

from __future__ import annotations

__title__ = "Catto Invasion"
__author__ = "CoolCat467"
__license__ = "GNU General Public License Version 3"
__version__ = "0.0.0"

import contextlib
import platform
import sys
import traceback
from os import path
from pathlib import Path
from typing import (
    TYPE_CHECKING,
    Any,
    Final,
    NamedTuple,
    TypeAlias,
    TypedDict,
    TypeVar,
)

import pygame
import trio
from libcomponent.async_clock import Clock
from libcomponent.component import (
    Component,
    ComponentManager,
    Event,
    ExternalRaiseManager,
)
from pygame.color import Color
from pygame.locals import K_ESCAPE, KEYUP, QUIT, RESIZABLE, WINDOWRESIZED
from pygame.rect import Rect

from catto_invasion import objects, sprite, xdg_path
from catto_invasion.hyphenate import hyphenate_word
from catto_invasion.objects import Button, OutlinedText
from catto_invasion.sound import SoundData, play_sound as base_play_sound
from catto_invasion.statemachine import AsyncState
from catto_invasion.vector import Vector2

if sys.version_info < (3, 11):
    import tomli as tomllib
    from exceptiongroup import ExceptionGroup
else:
    import tomllib

if TYPE_CHECKING:
    from collections.abc import (
        Awaitable,
        Callable,
        Generator,
    )


SCREEN_SIZE = (640, 480)

FPS: Final = 48
VSYNC = True


SOUND_LOOKUP: Final = {
    "delete_piece": "pop.mp3",
    "piece_move": "slide.mp3",
    "piece_update": "ding.mp3",
    "button_click": "select.mp3",
    "tick": "tick.mp3",
}
SOUND_DATA: Final = {
    "delete_piece": SoundData(
        volume=50,
    ),
}

BLACK: Final = (0, 0, 0)
BLUE: Final = (15, 15, 255)
GREEN: Final = (0, 255, 0)
CYAN: Final = (0, 255, 255)
RED: Final = (255, 0, 0)
MAGENTA: Final = (255, 0, 255)
YELLOW: Final = (255, 255, 0)
WHITE: Final = (255, 255, 255)

CONFIG_PATH: Final = xdg_path.config_path(__title__)
DATA_PATH: Final = xdg_path.data_path(__title__)

T = TypeVar("T")

DATA_FOLDER: Final = Path(__file__).parent / "data"

IS_WINDOWS: Final = platform.system() == "Windows"

FONT_FILE = DATA_FOLDER / "unifont-15.1.05.otf"

Pos: TypeAlias = tuple[int, int]


def play_sound(
    sound_name: str,
) -> tuple[pygame.mixer.Sound, int | float]:
    """Play sound effect."""
    sound_filename = SOUND_LOOKUP.get(sound_name)
    if sound_filename is None:
        raise RuntimeError(f"Error: Sound with ID `{sound_name}` not found.")
    sound_data = SOUND_DATA.get(sound_name, SoundData())

    return base_play_sound(
        DATA_FOLDER / sound_filename,
        sound_data,
    )


class PygameVideoResize(TypedDict):
    """PygameVideoResize event data."""

    size: tuple[int, int]
    w: int
    h: int


class WindowResizeAutoMove(Component):
    """`window_resize_automove` component.

    Automatically reposition sprites when video area is resized.
    """

    __slots__ = ("last_size",)

    def __init__(self) -> None:
        """Initialize window_resize_automove component."""
        super().__init__("window_resize_automove")

        self.last_size = Vector2.from_iter(SCREEN_SIZE)

    def bind_handlers(self) -> None:
        """Register PygameVideoResize handler."""
        self.register_handler("PygameVideoResize", self.handle_resize)

    async def handle_resize(self, event: Event[PygameVideoResize]) -> None:
        """Handle Resize Event."""
        old = self.last_size
        self.last_size = Vector2.from_iter(event.data["size"])
        delta = self.last_size - old

        sprite_: sprite.Sprite = self.get_component("sprite")
        sprite_.location += delta // 2
        sprite_.dirty = 1
        await trio.lowlevel.checkpoint()


class WiggleData(NamedTuple):
    """Wiggle data."""

    wiggle: Vector2 | tuple[int, int]
    wiggle_time: float


class TalkData(NamedTuple):
    """Talk data."""

    text: str
    typewriter_delay: float = 0.01


class TextBox(objects.OutlinedText):
    """TextBox object."""

    __slots__ = ("sound",)

    def __init__(self) -> None:
        """Initialize textbox."""
        super().__init__(
            "textbox",
            pygame.font.Font(FONT_FILE, 32),
        )

        self.add_component(WindowResizeAutoMove())

        self.color = WHITE
        self.inside = (18, 18, 18)
        self.outline = GREEN
        self.border_radius = 4
        self.border_width = 2

        self.text = ""

        self.sound = pygame.mixer.Sound(DATA_FOLDER / "talky.wav")

    def bind_handlers(self) -> None:
        """Register event handlers."""
        super().bind_handlers()

        self.register_handlers(
            {
                "text_clear": self.text_clear,
                "text_add": self.text_add,
                "text_set": self.text_set,
            },
        )

    async def text_clear(self, event: Event[None]) -> None:
        """Clear text and set to not be visible."""
        self.text = ""
        self.visible = False
        await trio.lowlevel.checkpoint()

    async def text_add(self, event: Event[tuple[str, bool]]) -> None:
        """Add event data to current text, play talky talky sound, and make visible."""
        text, play_sound = event.data
        self.text += text
        self.visible = True
        await trio.lowlevel.checkpoint()
        self.location = Vector2(SCREEN_SIZE[0] // 2, SCREEN_SIZE[1] // 2 + 150)
        if play_sound:
            self.sound.play()

    async def text_set(self, event: Event[str]) -> None:
        """Set text to event data and make visible."""
        self.text = event.data
        self.visible = True
        self.location = Vector2(SCREEN_SIZE[0] // 2, SCREEN_SIZE[1] // 2 + 150)
        await trio.lowlevel.checkpoint()


def text_with_delays(text: str) -> Generator[tuple[str, int], None, None]:
    """Yield characters and delay types to wait.

    0 is short, 1 is normal, 2 is long.
    """
    short_wait = set(" ")
    long_wait = set(",.?!")

    words = text.split(" ")
    for idx, word in enumerate(words):
        parts = hyphenate_word(word)
        for part in parts:
            for char in part:
                if char in short_wait:
                    yield char, 0
                elif char in long_wait:
                    yield char, 2
                else:
                    yield char, 1
        if idx < (len(words) - 1):
            yield " ", 0


class Speaker(sprite.Sprite):
    """Speaker object."""

    __slots__ = ()

    def __init__(self, name: str) -> None:
        """Initialize Speaker."""
        super().__init__(name)

        self.image = pygame.transform.scale(
            pygame.image.load(DATA_FOLDER / f"{name}.png"),
            (128, 128),
        )

        self.add_component(sprite.DragClickEventComponent())
        self.add_component(WindowResizeAutoMove())

        self.location = (64, 64)  # Vector2(*SCREEN_SIZE) // 2

    def bind_handlers(self) -> None:
        """Register event handlers."""
        super().bind_handlers()

        self.register_handlers(
            {
                f"{self.name}_wiggle": self.wiggle,
                f"{self.name}_set_visible": self.set_visible,
                f"{self.name}_talk": self.talk,
                f"{self.name}_set_location": self.set_location,
                "click": self.click,
            },
        )

    async def click(
        self,
        event: Event[sprite.PygameMouseButtonEventData],
    ) -> None:
        """Handle click event."""
        if not self.is_topmost(event.data["pos"]):
            return
        await self.raise_event(Event("speaker_clicked", self.name, 1))

    async def wiggle(self, event: Event[WiggleData]) -> None:
        """Handle wiggle event."""
        ##        self.location = Vector2(*SCREEN_SIZE) // 2
        self.location += event.data.wiggle
        self.dirty = 1
        await trio.sleep(event.data.wiggle_time)
        self.location -= event.data.wiggle
        self.dirty = 1

    async def set_visible(self, event: Event[bool]) -> None:
        """Handle set visible event."""
        self.visible = event.data
        await self.raise_event(
            Event("visibility_changed", (self.name, self.visible), 1),
        )
        await trio.lowlevel.checkpoint()

    async def talk(self, event: Event[str]) -> None:
        """Handle talk event."""
        await self.raise_event(Event("talk", (self.name, event.data), 1))

    async def set_location(self, event: Event[Vector2]) -> None:
        """Handle set location event."""
        self.location = event.data
        self.dirty = 1


class FPSCounter(objects.Text):
    """FPS counter."""

    __slots__ = ()

    def __init__(self) -> None:
        """Initialize FPS counter."""
        font = pygame.font.Font(
            trio.Path(path.dirname(__file__), "data", "VeraSerif.ttf"),
            28,
        )
        super().__init__("fps", font)

    async def on_tick(self, event: Event[sprite.TickEventData]) -> None:
        """Update text."""
        self.text = f"FPS: {event.data.fps:.0f}"
        self.location = Vector2.from_iter(self.image.get_size()) // 2 + (5, 5)
        self.visible = True
        await trio.lowlevel.checkpoint()

    def bind_handlers(self) -> None:
        """Register tick event handler."""
        super().bind_handlers()
        self.register_handlers(
            {
                "tick": self.on_tick,
            },
        )


class HaltState(AsyncState["CattoClient"]):
    """Halt state to set state to None so running becomes False."""

    __slots__ = ()

    def __init__(self) -> None:
        """Initialize Hault State."""
        super().__init__("Halt")

    async def check_conditions(self) -> None:
        """Set active state to None."""
        assert self.machine is not None
        await self.machine.set_state(None)


class GameState(AsyncState["CattoClient"]):
    """Catto Game Asynchronous State base class."""

    __slots__ = ("id", "manager")

    def __init__(self, name: str) -> None:
        """Initialize Game State."""
        super().__init__(name)

        self.id: int = 0
        self.manager = ComponentManager(self.name)

    def add_actions(self) -> None:
        """Add internal component manager to statemachine's component manager."""
        assert self.machine is not None
        self.machine.manager.add_component(self.manager)

    def group_add(self, new_sprite: sprite.Sprite) -> None:
        """Add new sprite to statemachine's group."""
        assert self.machine is not None
        group = self.machine.get_group(self.id)
        assert group is not None, "Expected group from new group id"
        group.add(new_sprite)
        self.manager.add_component(new_sprite)

    async def exit_actions(self) -> None:
        """Remove group and unbind all components."""
        assert self.machine is not None
        self.machine.remove_group(self.id)
        self.manager.unbind_components()
        self.id = 0
        await trio.lowlevel.checkpoint()

    def change_state(
        self,
        new_state: str | None,
    ) -> Callable[[Event[Any]], Awaitable[None]]:
        """Return an async function that will change state to `new_state`."""

        async def set_state(*args: object, **kwargs: object) -> None:
            play_sound("button_click")
            await self.machine.set_state(new_state)

        return set_state


class InitializeState(AsyncState["CattoClient"]):
    """Initialize Catto State."""

    __slots__ = ()

    def __init__(self) -> None:
        """Initialize the Initialize State."""
        super().__init__("initialize")

    async def check_conditions(self) -> str:
        """Go to title."""
        await trio.lowlevel.checkpoint()
        return "title"


class KwargOutlineText(OutlinedText):
    """Outlined Text with attributes settable via keyword arguments."""

    __slots__ = ()

    def __init__(
        self,
        name: str,
        font: pygame.font.Font,
        **kwargs: object,
    ) -> None:
        """Initialize attributes via keyword arguments."""
        super().__init__(name, font)

        for key, value in kwargs.items():
            setattr(self, key, value)

        self.add_component(WindowResizeAutoMove())


class KwargButton(Button):
    """Button with attributes settable via keyword arguments."""

    __slots__ = ()

    def __init__(
        self,
        name: str,
        font: pygame.font.Font,
        **kwargs: object,
    ) -> None:
        """Initialize attributes via keyword arguments."""
        super().__init__(name, font)

        for key, value in kwargs.items():
            setattr(self, key, value)

        self.add_component(WindowResizeAutoMove())


class TitleState(GameState):
    """Game Title State."""

    __slots__ = ()

    def __init__(self) -> None:
        """Initialize Title State."""
        super().__init__("title")

    async def entry_actions(self) -> None:
        """Add buttons."""
        assert self.machine is not None
        await trio.lowlevel.checkpoint()
        self.id = self.machine.new_group("title")

        button_font = pygame.font.Font(
            trio.Path(path.dirname(__file__), "data", "VeraSerif.ttf"),
            28,
        )
        title_font = pygame.font.Font(
            trio.Path(path.dirname(__file__), "data", "VeraSerif.ttf"),
            56,
        )

        title_text = KwargOutlineText(
            "title_text",
            title_font,
            visible=True,
            color=Color(0, 0, 0),
            outline=(255, 0, 0),
            border_width=4,
            text=__title__.upper(),
        )
        title_text.location = (SCREEN_SIZE[0] // 2, title_text.rect.h)
        self.group_add(title_text)

        hosting_button = KwargButton(
            "hosting_button",
            button_font,
            visible=False,
            color=Color(0, 0, 0),
            text="Host Networked Game",
            location=[x // 2 for x in SCREEN_SIZE],
            handle_click=self.change_state("play_hosting"),
        )
        self.group_add(hosting_button)

        join_button = KwargButton(
            "join_button",
            button_font,
            visible=False,
            color=Color(0, 0, 0),
            text="Join Networked Game",
            location=hosting_button.location
            + Vector2(
                0,
                hosting_button.rect.h + 10,
            ),
            handle_click=self.change_state("play_joining"),
        )
        self.group_add(join_button)

        internal_button = KwargButton(
            "internal_hosting",
            button_font,
            visible=True,
            color=Color(0, 0, 0),
            text="Singleplayer Game",
            location=hosting_button.location
            - Vector2(
                0,
                hosting_button.rect.h + 10,
            ),
            handle_click=self.change_state("play"),
        )
        self.group_add(internal_button)

        await self.machine.raise_event(Event("init", None))


##    async def check_conditions(self) -> str:
##        return "play_hosting"  # "play_hosting" # "play_joining"


class Record(NamedTuple):
    """Database record object."""

    position: str
    events: dict[str, str]


class Stage:
    """Stage class, keep track of active character positions."""

    __slots__ = (
        "order",
        "positions",
    )

    def __init__(self) -> None:
        """Initialize stage."""
        self.order: list[str] = []
        self.positions: list[Vector2] = []

    @property
    def active_count(self) -> int:
        """Active character count."""
        return len(self.order)

    def reorder(self) -> None:
        """Reorder active character positions."""
        self.positions.clear()
        for index in range(self.active_count):
            current = Vector2(index, 0)
            self.positions.append(current)

    def get_positions(self) -> zip[tuple[str, Vector2]]:
        """Return zip object of character names and locations."""
        return zip(self.order, self.positions, strict=True)

    def character_enter(self, name: str) -> bool:
        """Character enters stage handler. Return if updated."""
        if name not in self.order:
            self.order.append(name)
            self.reorder()
            return True
        return False

    def character_leave(self, name: str) -> bool:
        """Character leaves stage handler. Return if updated."""
        if name in self.order:
            self.order.remove(name)
            self.reorder()
            return True
        return False


class PlayState(GameState):
    """Game Play State."""

    __slots__ = (
        "db",
        "position",
        "stage",
        "talking",
        "talking_needs_cancel",
        "visited",
    )

    def __init__(self) -> None:
        """Initialize Title State."""
        super().__init__("play")

        self.position = "start"
        self.visited: dict[str, set[str]] = {}
        self.talking = trio.Lock()
        self.talking_needs_cancel = False
        self.stage = Stage()

        with open(DATA_FOLDER / "database.toml", "rb") as fp:
            self.db = tomllib.load(fp)

    def register_handlers(self) -> None:
        """Register event handlers."""
        self.manager.register_handlers(
            {
                "load_speakers": self.handle_load_speakers,
                "goto": self.handle_goto,
                "talk": self.handle_talk,
                "speaker_clicked": self.handle_speaker_clicked,
                "visibility_changed": self.handle_visibility_changed,
            },
        )

    def add_actions(self) -> None:
        """Register handlers."""
        super().add_actions()
        self.register_handlers()

    async def handle_load_speakers(self, event: Event[list[str]]) -> None:
        """Handle load speaker(s) event."""
        speakers = event.data
        for speaker_name in speakers:
            if not self.manager.component_exists(speaker_name):
                self.group_add(Speaker(speaker_name))

    async def handle_goto(self, event: Event[str]) -> None:
        """Handle goto new position event."""
        new_state = event.data
        self.visited.setdefault(self.position, set())
        self.visited[self.position].add(new_state)
        print(f"{self.visited = }")
        self.position = new_state
        await self.handle_position()

    async def handle_record(self, record: dict[str, Any]) -> None:
        """Handle database record."""
        ##print(f"[handle_record] {record = }")
        events = record.get("events", {})
        for event_name, event_data in events.items():
            event = Event(event_name, event_data)
            ##print(f"[handle_record] {event = }")
            await self.manager.raise_event(event)
        conditionals = record.get("conditionals", {})
        for condition in conditionals:
            print(f"{condition = }")

    async def handle_position(self) -> None:
        """Handle position change."""
        ##print(f"[handle_position] {self.position = }")
        record = self.db[self.position]
        await self.handle_record(record)

    async def entry_actions(self) -> None:
        """Add GameBoard and raise init event."""
        assert self.machine is not None
        if self.id == 0:
            self.id = self.machine.new_group("play")

        self.group_add(FPSCounter())
        self.group_add(TextBox())

        await self.handle_position()

    async def update_character_positions(self) -> None:
        """Update character positions."""
        ##        print("[update_character_positions]")
        ##screen_width = SCREEN_SIZE[0]
        ##padding = screen_width // 6
        ##working_area = screen_width - (padding * 2)
        ##spacing = working_area // max(self.stage.active_count, 1)
        ##y = SCREEN_SIZE[1] // 2
        ##async with trio.open_nursery() as nursery:
        ##    for character, raw_position in self.stage.get_positions():
        ##        position = (raw_position * spacing) + (padding, y)
        ##        print(f'{character} {position = }')
        ##        await self.manager.raise_event_in_nursery(
        ##            Event(f"{character}_set_location", position),
        ##            nursery,
        ##        )
        screen_width = SCREEN_SIZE[0]
        ##        center = Vector2.from_iter(SCREEN_SIZE) // 2
        padding = screen_width // 6
        working_width = screen_width - (padding * 2)

        ##        half_width = working_width // 2
        ##        d = 30

        ##radius = (((half_width * half_width) // d) + d) // 2
        ##print(f"{radius = }")
        ##circle_center = center + Vector2(0, radius) - Vector2(0, d)
        ##theta = math.atan2(half_width, radius - d)
        ##print(f"{math.degrees(theta) = }")

        ##        angle = theta / (max(self.stage.active_count, 1))

        ##right_start = (math.pi - theta) / 2

        ##pygame.draw.circle(screen, WHITE, circle_center, radius, width=1)
        ##pygame.draw.line(screen, WHITE, circle_center, center)

        ##        c = max(self.stage.active_count, 1) + 1
        ##        for i in range(c+1):
        ##            end = center - Vector2.from_radians(math.pi * (i / c), 100)
        ##            pygame.draw.line(screen, RED, center, end, 3)

        ##        count = max(self.stage.active_count, 1) + 1
        spacing = working_width // max(self.stage.active_count, 1)
        y = SCREEN_SIZE[1] // 2
        ##        radius = 778
        ##        ##        print(f'{circle_center - center = }')
        ##        ##        print(f'{circle_center = }')
        ##        ##        print(f'{center + Vector2(x=0, y=748) = }')
        ##        circle_center = center + Vector2(x=0, y=748)
        ##        ##        print(f'{theta = }')
        ##        theta = 0.2786527663350684

        async with trio.open_nursery() as nursery:
            for character, raw_position in self.stage.get_positions():
                ##                r = theta * ((raw_position.x + 1) / (count))  # + right_start
                ##                position = circle_center - Vector2.from_radians(r, radius)
                ##                ##print(f'{circle_center = }')
                ##                ##print(f'{math.degrees(r) = }')
                ##                ##position = Vector2.from_radians(r, radius) + circle_center
                ##                ##position = Vector2.from_radians(r, radius)
                ##                ##position = Vector2(position.x, -position.y)
                ##                ##pygame.draw.line(screen, RED, circle_center, position, 3)
                ##                ##pygame.draw.line(screen, WHITE, circle_center, position)
                ##                print(f"{character} {position = }")
                position = (raw_position * spacing) + (padding, y)
                await self.manager.raise_event_in_nursery(
                    Event(f"{character}_set_location", position.rounded()),
                    nursery,
                )

    async def handle_visibility_changed(
        self,
        event: Event[tuple[str, bool]],
    ) -> None:
        """Handle visibility changed event."""
        speaker, visible = event.data
        update = False
        if visible:
            update = self.stage.character_enter(speaker)
        else:
            update = self.stage.character_leave(speaker)
        if update:
            await self.update_character_positions()

    async def handle_talk(self, event: Event[tuple[str, str]]) -> None:
        """Handle talk event."""
        speaker, words = event.data

        ##        if self.talking.locked():
        ##            self.talking_needs_cancel = True
        async with self.talking:
            if self.stage.character_enter(speaker):
                await self.update_character_positions()

            self.talking_needs_cancel = False
            await self.manager.raise_event(Event("text_clear", None))

            await self.manager.raise_event(
                Event(f"{speaker}_set_visible", True),
            )
            degrees = 0
            for char, delay_type in text_with_delays(words):
                distance = {0: 0, 1: 5, 2: 10}[delay_type]

                degrees = (degrees + 65) % 360
                await self.manager.raise_event(
                    Event("text_add", (char, delay_type != 0)),
                )
                wiggle = Vector2.from_degrees(degrees, distance)
                wiggle_delay = {0: 0.1, 1: 0.1, 2: 0.3}[delay_type]
                await self.manager.raise_event(
                    Event(
                        f"{speaker}_wiggle",
                        WiggleData(
                            wiggle,
                            wiggle_delay,
                        ),
                    ),
                )
                if self.talking_needs_cancel:
                    self.talking_needs_cancel = False
                    await self.manager.raise_event(Event("text_set", words))
                    break

    async def handle_speaker_clicked(self, event: Event[str]) -> None:
        """Handle speaker clicked event."""
        speaker = event.data
        if self.talking.locked():
            print("[speaker_clicked] Fast forward text")
            self.talking_needs_cancel = True
            return
        record = self.db[self.position].get("click", {}).get(speaker)
        if not record:
            print(
                f"No click handlers registered for {speaker!r} clicked in state {self.position!r}",
            )
            print("Add one with the following:\n")
            print(f"[{self.position}.click.{speaker}.events]")
            print("goto = 'not_implemented'\n")
            return
        await self.handle_record(record)


class CattoClient(sprite.GroupProcessor):
    """Catto Game Client."""

    __slots__ = ("manager",)

    def __init__(self, manager: ComponentManager) -> None:
        """Initialize Catto Client."""
        super().__init__()
        self.manager = manager

        self.add_states(
            (
                HaltState(),
                InitializeState(),
                TitleState(),
                PlayState(),
            ),
        )

    @property
    def running(self) -> bool:
        """Boolean of if state machine is running."""
        return self.active_state is not None

    async def raise_event(self, event: Event[Any]) -> None:
        """Raise component event in all groups."""
        await self.manager.raise_event(event)


async def async_run() -> None:
    """Handle main event loop."""
    # Set up globals
    global SCREEN_SIZE
    # global screen

    # Set up the screen
    screen = pygame.display.set_mode(SCREEN_SIZE, RESIZABLE, 32, vsync=VSYNC)
    pygame.display.set_caption(f"{__title__} v{__version__}")
    pygame.key.set_repeat(1000, 30)
    screen.fill((0xFF, 0xFF, 0xFF))

    async with trio.open_nursery() as main_nursery:
        event_manager = ExternalRaiseManager(
            __title__.lower(),
            main_nursery,  # "client"
        )
        client = CattoClient(event_manager)

        background = pygame.image.load(
            path.join(path.dirname(__file__), "data", "background.png"),
        ).convert()
        client.clear(screen, background)

        client.set_timing_threshold(1000 / 80)

        await client.set_state("initialize")

        # clock = pygame.time.Clock()
        clock = Clock()

        resized_window = False
        while client.running:
            async with trio.open_nursery() as event_nursery:
                for event in pygame.event.get():
                    if event.type == QUIT:
                        await client.set_state("Halt")
                    elif event.type == KEYUP and event.key == K_ESCAPE:
                        pygame.event.post(pygame.event.Event(QUIT))
                    elif event.type == WINDOWRESIZED:
                        SCREEN_SIZE = (event.x, event.y)
                        resized_window = True
                        client.clear(
                            screen,
                            pygame.transform.scale(background, SCREEN_SIZE),
                        )
                    sprite_event = sprite.convert_pygame_event(event)
                    # print(sprite_event)
                    event_nursery.start_soon(
                        event_manager.raise_event,
                        sprite_event,
                    )
                event_nursery.start_soon(client.think)
                event_nursery.start_soon(clock.tick, FPS)

            await client.raise_event(
                Event(
                    "tick",
                    sprite.TickEventData(
                        time_passed=clock.get_time()
                        / 1e9,  # nanoseconds -> seconds
                        fps=clock.get_fps(),
                    ),
                ),
            )

            if resized_window:
                resized_window = False
                screen.fill((0xFF, 0xFF, 0xFF))
                rects = [Rect((0, 0), SCREEN_SIZE)]
                client.repaint_rect(rects[0])
                rects.extend(client.draw(screen))
            else:
                rects = client.draw(screen)
            pygame.display.update(rects)
    client.clear_groups()


def run() -> None:
    """Start asynchronous run."""
    try:
        trio.run(async_run, strict_exception_groups=True)
    except ExceptionGroup:
        traceback.print_exc()


def cli_run() -> None:
    """Start game."""
    print(f"{__title__} v{__version__}\nProgrammed by {__author__}.\n")

    # If we're not imported as a module, run.
    # Make sure the game will display correctly on high DPI monitors on Windows.

    if IS_WINDOWS:
        from ctypes import windll  # type: ignore[attr-defined,unused-ignore]

        with contextlib.suppress(AttributeError):
            windll.user32.SetProcessDPIAware()
        del windll

    try:
        pygame.init()
        run()
    finally:
        pygame.quit()


if __name__ == "__main__":
    cli_run()
