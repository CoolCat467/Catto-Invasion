"""XDG Path - Handle persistent save data."""

# Programmed by CoolCat467

from __future__ import annotations

# XDG Path - Handle persistent save data
# Copyright (C) 2025  CoolCat467
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

__title__ = "XDG Path"
__author__ = "CoolCat467"
__version__ = "0.0.0"
__license__ = "GNU General Public License Version 3"


from os import getenv, path
from typing import Final

import trio

HOME: Final = trio.Path(getenv("HOME", path.expanduser("~")))
XDG_DATA_HOME: Final = trio.Path(
    getenv("XDG_DATA_HOME", HOME / ".local" / "share"),
)
XDG_CONFIG_HOME: Final = trio.Path(getenv("XDG_CONFIG_HOME", HOME / ".config"))


def application_folder_name(title: str) -> str:
    """Return application folder name from title."""
    return title.lower().replace(" ", "-").replace("-", "_")


def config_path(title: str) -> trio.Path:
    """Return XDG configuration path from application title."""
    return XDG_CONFIG_HOME / application_folder_name(title)


def data_path(title: str) -> trio.Path:
    """Return XDG data path from application title."""
    return XDG_DATA_HOME / application_folder_name(title)
