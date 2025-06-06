# Catto-Invasion
Catto Invasion Visual Novel

[![CI](https://github.com/CoolCat467/Catto-Invasion/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/CoolCat467/Catto-Invasion/actions/workflows/ci.yml)
<!-- BADGIE TIME -->

[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit)](https://github.com/pre-commit/pre-commit)
[![code style: black](https://img.shields.io/badge/code_style-black-000000.svg)](https://github.com/psf/black)
[![Ruff](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/astral-sh/ruff/main/assets/badge/v2.json)](https://github.com/astral-sh/ruff)

<!-- END BADGIE TIME -->

## Installation
- To install the newest version from the internet:
```console
pip install git+https://github.com/CoolCat467/Catto-Invasion.git
```

To install local copy:
- Clone this repo
- Go to folder repo is cloned in
```console
pip install -e .
```

## Run
```console
catto_invasion_game
```

## How to play
Click the character icons. If you click a character while any character
is still talking, it will skip doing typewriter style printing and just
display the entire text box immediately so you don't have to wait for
them to finish talking. Story database can be set up to have different
things happen if you click different characters.

## Extending
All story data lives in ./src/catto_invasion/data/database.toml


### Links
* Source Code - https://github.com/CoolCat467/Catto-Invasion.git
* Issues      - https://github.com/CoolCat467/Catto-Invasion/issues

### License
-------
Code and documentation are available according to the GNU General Public License v3.0 (see [LICENSE](https://github.com/CoolCat467/Catto-Invasion/blob/HEAD/LICENSE)).
