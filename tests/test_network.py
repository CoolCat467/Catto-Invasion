from __future__ import annotations

from typing import TYPE_CHECKING

import pytest
import trio
import trio.testing

from catto_invasion.component import Event
from catto_invasion.network import (
    NetworkComponent,
    NetworkEventComponent,
    NetworkTimeoutError,
    Server,
)

if TYPE_CHECKING:
    from collections.abc import Callable, Generator

pytest_plugins = ("pytest_trio",)


@pytest.mark.trio
async def client_connect(port: int, stop_server: Callable[[], None]) -> None:
    await trio.sleep(0.05)
    # manager = ComponentManager("manager")

    client = NetworkEventComponent("client")
    # manager.add_component(client)

    await client.connect("127.0.0.1", port)

    client.register_network_write_event("echo_event", 0)
    client.register_read_network_event(1, "reposted_event")

    event = Event(
        "echo_event",
        bytearray("I will give my cat food to bob", "utf-8"),
        3,
    )

    # await client.raise_event(event)
    await client.write_event(event)
    print(f"{await client.read_event() = }")

    await client.close()
    stop_server()


@pytest.mark.trio
async def run_async() -> None:
    class TestServer(Server):
        async def handler(self, stream: trio.SocketStream) -> None:
            client = NetworkEventComponent.from_stream("client", stream=stream)

            client.register_read_network_event(0, "repost_event")
            client.register_network_write_event("repost_event", 1)

            await client.write_event(await client.read_event())
            await stream.aclose()

    server = TestServer("server")
    port = 3004
    async with trio.open_nursery() as nursery:
        nursery.start_soon(server.serve, port)
        nursery.start_soon(client_connect, port, server.stop_serving)
        nursery.start_soon(client_connect, port, server.stop_serving)


@pytest.fixture
async def network_component() -> Generator[NetworkComponent, None, None]:
    async with NetworkComponent("TestComponent") as component:
        yield component


@pytest.fixture
def memory_stream_oneway_network_components() -> (
    Generator[tuple[NetworkEventComponent, NetworkEventComponent], None, None]
):
    send, recv = trio.testing.memory_stream_one_way_pair()
    send_component = NetworkEventComponent.from_stream("Send", stream=send)
    recv_component = NetworkEventComponent.from_stream("Recv", stream=recv)
    yield (send_component, recv_component)
    send.close()
    recv.close()


@pytest.fixture
async def network_event_component() -> (
    Generator[NetworkEventComponent, None, None]
):
    async with NetworkEventComponent("TestEventComponent") as component:
        yield component


@pytest.mark.trio
async def test_network_component_connect(
    memory_stream_oneway_network_components: tuple[
        NetworkEventComponent,
        NetworkEventComponent,
    ],
) -> None:
    send_component, recv_component = memory_stream_oneway_network_components
    assert not send_component.not_connected
    assert not recv_component.not_connected


@pytest.mark.trio
async def test_not_connected_access(
    network_component: NetworkComponent,
) -> None:
    with pytest.raises(RuntimeError, match="^Stream not connected!$"):
        _ = network_component.stream


@pytest.mark.trio
async def test_network_component_read_write(
    memory_stream_oneway_network_components: tuple[
        NetworkEventComponent,
        NetworkEventComponent,
    ],
) -> None:
    send_component, recv_component = memory_stream_oneway_network_components
    data_to_write = b"Hello, World!"
    await send_component.write(data_to_write)
    data_read = await recv_component.read(len(data_to_write))
    assert data_read == data_to_write


@pytest.mark.trio
async def test_network_event_component_register_write_event(
    network_event_component: NetworkEventComponent,
) -> None:
    network_event_component.register_network_write_event("TestEvent", 1)
    assert (
        "TestEvent" in network_event_component._write_event_name_to_packet_id
    )


@pytest.mark.trio
async def test_network_event_component_register_write_event_failure(
    network_event_component: NetworkEventComponent,
) -> None:
    network_event_component.register_network_write_event("TestEvent", 1)
    with pytest.raises(
        ValueError,
        match="^'TestEvent' event already registered!$",
    ):
        network_event_component.register_network_write_event("TestEvent", 1)


@pytest.mark.trio
async def test_network_event_component_register_read_event_failure(
    network_event_component: NetworkEventComponent,
) -> None:
    network_event_component.register_read_network_event(1, "TestEvent")
    with pytest.raises(ValueError, match="^Packet ID 1 already registered!$"):
        network_event_component.register_read_network_event(1, "JeraldEvent")
    with pytest.raises(
        ValueError,
        match="^'TestEvent' events are also being received from server with packet id 1,",
    ):
        network_event_component.register_network_write_event("TestEvent", 1)


@pytest.mark.trio
async def test_network_event_component_write_read_event(
    memory_stream_oneway_network_components: tuple[
        NetworkEventComponent,
        NetworkEventComponent,
    ],
) -> None:
    send_component, recv_component = memory_stream_oneway_network_components
    send_component.register_network_write_event("SendTestEvent", 1)
    recv_component.register_read_network_event(1, "RecvTestEvent")
    event_data = b"Event Data"
    event = Event("SendTestEvent", event_data)
    await send_component.write_event(event)
    await trio.lowlevel.checkpoint()
    received_event = await recv_component.read_event()
    assert received_event.name == "RecvTestEvent"
    assert received_event.data == event_data


def test_network_timeout_error() -> None:
    with pytest.raises(NetworkTimeoutError, match="^Test Timeout Error$"):
        raise NetworkTimeoutError("Test Timeout Error")


@pytest.mark.trio
async def test_network_component_connect_error(
    memory_stream_oneway_network_components: tuple[
        NetworkEventComponent,
        NetworkEventComponent,
    ],
) -> None:
    send_component, recv_component = memory_stream_oneway_network_components
    with pytest.raises(RuntimeError, match="^Already connected!$"):
        await send_component.connect("localhost", 80)
    with pytest.raises(RuntimeError, match="^Already connected!$"):
        await recv_component.connect("localhost", 80)


@pytest.mark.trio
async def test_network_component_read_failure_timeout(
    memory_stream_oneway_network_components: tuple[
        NetworkEventComponent,
        NetworkEventComponent,
    ],
) -> None:
    send_component, recv_component = memory_stream_oneway_network_components
    await send_component.write(b"")
    recv_component.timeout = 0.05
    with pytest.raises(
        NetworkTimeoutError,
        match="^Server did not respond with any information",
    ):
        await recv_component.read(1)


@pytest.mark.trio
async def test_network_component_read_partial_response_failure(
    memory_stream_oneway_network_components: tuple[
        NetworkEventComponent,
        NetworkEventComponent,
    ],
) -> None:
    send_component, recv_component = memory_stream_oneway_network_components
    await send_component.write(b"cat")
    recv_component.timeout = 0.05
    with pytest.raises(OSError, match="^Server stopped responding "):
        await recv_component.read(4)
