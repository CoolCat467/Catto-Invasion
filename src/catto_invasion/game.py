"""Graphical Catto Game."""

# Programmed by CoolCat467

# Copyright (C) 2024  CoolCat467
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
from os import path
from pathlib import Path
from typing import TYPE_CHECKING, Any, Final, NamedTuple, TypeAlias, TypeVar

import pygame
import trio
from pygame.color import Color
from pygame.locals import K_ESCAPE, KEYUP, QUIT, RESIZABLE, WINDOWRESIZED
from pygame.rect import Rect

from catto_invasion import objects, sprite
from catto_invasion.async_clock import Clock
from catto_invasion.component import (
    ComponentManager,
    Event,
    ExternalRaiseManager,
)
from catto_invasion.objects import Button, OutlinedText
from catto_invasion.statemachine import AsyncState
from catto_invasion.vector import Vector2

if sys.version_info < (3, 11):
    import tomli as tomllib
else:
    import tomllib

if TYPE_CHECKING:
    from collections.abc import (
        Awaitable,
        Callable,
    )

    from pygame.surface import Surface

SCREEN_SIZE = (640, 480)

FPS: Final = 48
VSYNC = True

BLACK: Final = (0, 0, 0)
BLUE: Final = (15, 15, 255)
GREEN: Final = (0, 255, 0)
CYAN: Final = (0, 255, 255)
RED: Final = (255, 0, 0)
MAGENTA: Final = (255, 0, 255)
YELLOW: Final = (255, 255, 0)
WHITE: Final = (255, 255, 255)


T = TypeVar("T")

DATA_FOLDER: Final = Path(__file__).parent / "data"

IS_WINDOWS: Final = platform.system() == "Windows"

Pos: TypeAlias = tuple[int, int]


def render_text(
    font_name: str,
    font_size: int,
    text: str,
    color: tuple[int, int, int],
) -> Surface:
    """Render text with a given font at font_size with the text in the color of color."""
    # Load the font at the size of font_size
    font = pygame.font.Font(font_name, font_size)
    # Using the loaded font, render the text in the color of color
    return font.render(text, False, color)


class WiggleData(NamedTuple):
    wiggle: tuple[int, int]
    wiggle_time: float


class TalkData(NamedTuple):
    text: str
    typewriter_delay: int = 0.01


class TextBox(objects.OutlinedText):
    __slots__ = ("sound",)

    def __init__(self) -> None:
        super().__init__(
            "textbox",
            pygame.font.Font(DATA_FOLDER / "VeraSerif.ttf", 32),
        )

        self.color = WHITE
        self.inside = (18, 18, 18)
        self.outline = GREEN
        self.border_radius = 4
        self.border_width = 2

        self.text = ""

        self.sound = pygame.mixer.Sound(DATA_FOLDER / "talky.wav")

    def bind_handlers(self) -> None:
        super().bind_handlers()

        self.register_handlers(
            {
                "text_clear": self.text_clear,
                "text_add": self.text_add,
                "text_set": self.text_set,
            },
        )

    async def text_clear(self, event: Event[None]) -> None:
        self.text = ""
        self.visible = False
        await trio.lowlevel.checkpoint()

    async def text_add(self, event: Event[str]) -> None:
        self.text += event.data
        self.visible = True
        await trio.lowlevel.checkpoint()
        self.location = Vector2(SCREEN_SIZE[0] // 2, SCREEN_SIZE[1] // 2 + 150)
        self.sound.play()

    async def text_set(self, event: Event[str]) -> None:
        self.text = event.data
        self.visible = True
        self.location = Vector2(SCREEN_SIZE[0] // 2, SCREEN_SIZE[1] // 2 + 150)
        await trio.lowlevel.checkpoint()


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
        self.location = Vector2(*SCREEN_SIZE) // 2

    def bind_handlers(self) -> None:
        """Register event handlers."""
        super().bind_handlers()

        self.register_handlers(
            {
                f"{self.name}_wiggle": self.wiggle,
                f"{self.name}_set_visible": self.set_visible,
                f"{self.name}_talk": self.talk,
                "click": self.click,
                "init": self.initialize,
            },
        )

    async def initialize(self, event: Event[None]) -> None:
        """Initialize textbox."""
        group = self.groups()[-1]

        text_box = TextBox()
        self.add_component(text_box)

        group.add(text_box)

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
        self.location = Vector2(*SCREEN_SIZE) // 2
        self.location += event.data.wiggle
        self.dirty = 1
        await trio.sleep(event.data.wiggle_time)
        self.location -= event.data.wiggle
        self.dirty = 1

    async def set_visible(self, event: Event[bool]) -> None:
        """Handle set visible event."""
        self.visible = event.data
        if not self.visible:
            await self.raise_event(Event("text_clear", None))
        await trio.lowlevel.checkpoint()

    async def talk(self, event: Event[str]) -> None:
        """Handle talk event."""
        await self.raise_event(Event(f"{self.name}_set_visible", True))
        await self.raise_event(Event("text_clear", None))
        degrees = 0
        for char in event.data:
            degrees = (degrees + 65) % 360
            await self.raise_event(Event("text_add", char))
            wiggle = Vector2.from_degrees(degrees, 5)
            await self.raise_event(
                Event(
                    f"{self.name}_wiggle",
                    WiggleData(
                        wiggle,
                        0.1,  # event.data.typewriter_delay
                    ),
                ),
            )
            ##            await trio.sleep(event.data.typewriter_delay)
            await trio.sleep(0.01)


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

    def change_state(
        self,
        new_state: str | None,
    ) -> Callable[[Event[Any]], Awaitable[None]]:
        """Return an async function that will change state to `new_state`."""

        async def set_state(*args: object, **kwargs: object) -> None:
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


class TitleState(GameState):
    """Game Title State."""

    __slots__ = ()

    def __init__(self) -> None:
        """Initialize Title State."""
        super().__init__("title")

    async def entry_actions(self) -> None:
        """Add buttons."""
        assert self.machine is not None
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


class PlayState(GameState):
    """Game Play State."""

    __slots__ = ("position", "db")

    def __init__(self) -> None:
        """Initialize Title State."""
        super().__init__("play")

        with open(DATA_FOLDER / "database.toml", "rb") as fp:
            self.db = tomllib.load(fp)

    def register_handlers(self) -> None:
        """Register event handlers."""
        self.manager.register_handlers(
            {
                "speaker_clicked": self.handle_speaker_clicked,
            },
        )

    def add_actions(self) -> None:
        """Register handlers."""
        super().add_actions()
        self.register_handlers()

    async def entry_actions(self) -> None:
        """Add GameBoard and raise init event."""
        assert self.machine is not None
        if self.id == 0:
            self.id = self.machine.new_group("play")

        # self.group_add(())

        self.group_add(Speaker("cat_babushka"))
        self.group_add(Speaker("cat_supreme"))
        self.group_add(Speaker("cat_officer"))
        self.group_add(Speaker("mr_floppy"))

        self.group_add(FPSCounter())

        self.position = "start"

        await self.machine.raise_event(Event("init", None))

        record = self.db[self.position]
        self.position = record["position"]
        for event_name, event_data in record["events"].items():
            await self.machine.raise_event(Event(event_name, event_data))

    async def handle_speaker_clicked(self, event: Event[str]) -> None:
        """Handle speacker clicked event."""
        record = self.db[self.position][event.data]
        self.position = record["position"]
        for event_name, event_data in record["events"].items():
            await self.machine.raise_event(Event(event_name, event_data))


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
                ##                PlayHostingState(),
                ##                PlayInternalHostingState(),
                ##                PlayJoiningState(),
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
    global SCREEN_SIZE, screen

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
    trio.run(async_run, strict_exception_groups=True)


def cli_run() -> None:
    """Start game."""
    print(f"{__title__} v{__version__}\nProgrammed by {__author__}.\n")

    # If we're not imported as a module, run.
    # Make sure the game will display correctly on high DPI monitors on Windows.

    if IS_WINDOWS:
        from ctypes import windll  # type: ignore

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