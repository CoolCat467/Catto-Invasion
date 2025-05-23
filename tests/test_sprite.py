from __future__ import annotations

from typing import cast

import pytest
import trio
from libcomponent.component import Event
from pygame.mask import Mask
from pygame.rect import Rect
from pygame.surface import Surface

from catto_invasion.sprite import (
    AnimationComponent,
    DragClickEventComponent,
    GroupProcessor,
    ImageComponent,
    MovementComponent,
    OutlineComponent,
    Sprite,
    TargetingComponent,
    TickEventData,
)
from catto_invasion.vector import Vector2


@pytest.fixture
def sprite() -> Sprite:
    return Sprite("test_sprite")


@pytest.fixture
def image_component(sprite: Sprite) -> ImageComponent:
    sprite.add_component(ImageComponent())
    return sprite.get_component("image")


@pytest.fixture
def animation_component(image_component: ImageComponent) -> AnimationComponent:
    image_component.add_component(AnimationComponent())
    return image_component.get_component("animation")


@pytest.fixture
def outline_component(image_component: ImageComponent) -> OutlineComponent:
    return image_component.get_component("outline")


@pytest.fixture
def movement_component(sprite: Sprite) -> MovementComponent:
    sprite.add_component(MovementComponent())
    return sprite.get_component("movement")


@pytest.fixture
def targeting_component(
    movement_component: MovementComponent,
) -> TargetingComponent:
    sprite = movement_component.manager
    sprite.add_component(TargetingComponent())
    return sprite.get_component("targeting")


@pytest.fixture
def drag_click_event_component() -> DragClickEventComponent:
    return DragClickEventComponent()


@pytest.fixture
def group_processor() -> GroupProcessor:
    return GroupProcessor()


def test_sprite_init(sprite: Sprite) -> None:
    assert sprite.name == "test_sprite"
    assert not sprite.visible
    assert sprite.rect == Rect(0, 0, 0, 0)


def test_sprite_location(sprite: Sprite) -> None:
    sprite.location = (10, 20)
    assert sprite.rect.center == (10, 20)


def test_sprite_repr(sprite: Sprite) -> None:
    assert repr(sprite) == "<Sprite Sprite ('sprite',)>"


def test_sprite_image(sprite: Sprite) -> None:
    sprite.dirty = 0
    assert sprite.image is None
    assert not sprite.dirty
    sprite.image = Surface((10, 10))
    assert isinstance(sprite.image, Surface)
    assert sprite.dirty
    assert sprite.rect.size == (10, 10)


def test_sprite_image_set_none(sprite: Sprite) -> None:
    sprite.dirty = 0
    assert sprite.image is None
    assert not sprite.dirty
    sprite.image = None
    assert sprite.dirty


def test_sprite_image_no_set_location_change(sprite: Sprite) -> None:
    sprite.update_location_on_resize = False
    sprite.location = (100, 100)
    sprite.image = Surface((50, 25))
    assert sprite.location == (125, 112)


def test_sprite_image_set_location_change(sprite: Sprite) -> None:
    sprite.update_location_on_resize = True
    sprite.location = (100, 100)
    sprite.image = Surface((50, 25))
    assert sprite.location == (100, 100)


def test_sprite_selected_invisible(sprite: Sprite) -> None:
    assert not sprite.visible
    sprite.rect.size = (100, 100)
    assert not sprite.is_selected((20, 20))


def test_sprite_selected(sprite: Sprite) -> None:
    sprite.visible = True
    sprite.rect.size = (100, 100)
    assert sprite.is_selected((20, 20))


def test_image_component_init(image_component: ImageComponent) -> None:
    assert image_component.mask_threshold == 127


def test_image_component_add_image(image_component: ImageComponent) -> None:
    image = Surface((10, 10))
    image_component.add_image("test_image", image)
    assert "test_image" in image_component.list_images()


def test_image_component_add_images(image_component: ImageComponent) -> None:
    image = Surface((10, 10))
    image_component.add_images({"test_image": image})
    assert "test_image" in image_component.list_images()


def test_image_component_get_image_fail(
    image_component: ImageComponent,
) -> None:
    with pytest.raises(
        ValueError,
        match='^No image saved for identifier "test_image"$',
    ):
        image_component.get_image("test_image")


def test_image_component_get_mask_fail(
    image_component: ImageComponent,
) -> None:
    with pytest.raises(
        ValueError,
        match='^No mask saved for identifier "test_mask"$',
    ):
        image_component.get_mask("test_mask")


def test_image_component_get_mask_success(
    image_component: ImageComponent,
) -> None:
    image = Surface((10, 10))
    image_component.add_image("test_image", image)
    assert isinstance(image_component.get_mask("test_image"), Mask)


def test_image_component_add_image_and_mask_invalid_image(
    image_component: ImageComponent,
) -> None:
    with pytest.raises(
        ValueError,
        match="^Expected surface to be a valid identifier$",
    ):
        image_component.add_image_and_mask("test_image", None, None)  # type: ignore[arg-type]
    with pytest.raises(
        ValueError,
        match="^Expected surface to be a valid identifier$",
    ):
        image_component.add_image_and_mask("test_image", "copy_from", None)  # type: ignore[arg-type]


def test_image_component_add_image_and_mask_invalid_mask(
    image_component: ImageComponent,
) -> None:
    image = Surface((1, 1))
    with pytest.raises(
        ValueError,
        match="^Expected mask to be a valid identifier$",
    ):
        image_component.add_image_and_mask("test_image", image, None)  # type: ignore[arg-type]
    with pytest.raises(
        ValueError,
        match="^Expected mask to be a valid identifier$",
    ):
        image_component.add_image_and_mask("test_image", image, "copy_from")  # type: ignore[arg-type]


def test_image_component_get_image(image_component: ImageComponent) -> None:
    image = Surface((1, 1))
    image_component.add_image("test_image", image)
    assert image_component.get_image("test_image") is image


def test_image_component_add_image_duplication(
    image_component: ImageComponent,
) -> None:
    image = Surface((1, 1))
    image_component.add_image("test_image", image)
    image_component.add_image("duplicate", "test_image")
    assert image_component.get_image("duplicate") is image


def test_image_component_get_duplicate_mask(
    image_component: ImageComponent,
) -> None:
    image = Surface((1, 1))
    image_component.add_image("test_image", image)
    image_component.add_image("duplicate", "test_image")
    assert isinstance(image_component.get_mask("duplicate"), Mask)


def test_image_component_set_image_affects_sprite(
    image_component: ImageComponent,
) -> None:
    image = Surface((1, 1))
    sprite = cast("Sprite", image_component.manager.get_component("sprite"))
    image_component.add_image("test_image", image)
    assert sprite.image is None
    image_component.set_image("test_image")
    assert sprite.image is image
    image_component.set_image("test_image")
    assert sprite.image is image


def test_movement_component_init(
    movement_component: MovementComponent,
) -> None:
    assert movement_component.heading == Vector2(0, 0)
    assert movement_component.speed == 0


def test_movement_component_point_toward(
    movement_component: MovementComponent,
) -> None:
    movement_component.point_toward((10, 20))
    assert (
        movement_component.heading
        == Vector2.from_points((0, 0), (10, 20)).normalized()
    )


def test_movement_component_move_heading_time(
    movement_component: MovementComponent,
) -> None:
    movement_component.speed = 5
    movement_component.move_heading_time(1)
    assert movement_component.heading * 5 == movement_component.heading


def test_targeting_component_init(
    targeting_component: TargetingComponent,
) -> None:
    assert targeting_component.destination == Vector2(0, 0)
    assert targeting_component.event_raise_name == "reached_destination"


def test_targeting_component_update_heading(
    targeting_component: TargetingComponent,
) -> None:
    targeting_component.destination = Vector2(10, 20)
    targeting_component.update_heading()
    assert targeting_component.to_destination() == Vector2.from_points(
        (0, 0),
        (10, 20),
    )


@pytest.mark.trio
async def test_targeting_component_move_destination_time(
    targeting_component: TargetingComponent,
) -> None:
    movement_component = targeting_component.get_component("movement")
    movement_component.speed = 1
    targeting_component.destination = Vector2(10, 20)
    current_distance = targeting_component.to_destination().magnitude()
    await targeting_component.move_destination_time(1)
    assert targeting_component.to_destination().magnitude() < current_distance


def test_drag_click_event_component_init(
    drag_click_event_component: DragClickEventComponent,
) -> None:
    assert drag_click_event_component.pressed == {}


def test_group_processor_init(group_processor: GroupProcessor) -> None:
    assert group_processor.groups == {}
    assert group_processor.group_names == {}
    assert group_processor.new_gid == 0


def test_group_processor_new_group(group_processor: GroupProcessor) -> None:
    gid = group_processor.new_group("test_group")
    assert gid in group_processor.groups
    assert "test_group" in group_processor.group_names


@pytest.mark.trio
async def test_animation_component_tick(
    animation_component: AnimationComponent,
) -> None:
    async with trio.open_nursery() as nursery:
        nursery.start_soon(
            animation_component.tick,
            Event("tick", TickEventData(time_passed=1, fps=60)),
        )
        await trio.lowlevel.checkpoint()
        # Assert that the animation component has updated correctly
