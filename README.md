# POOM SDK
This repository contains everything required to produce a DOOM-like using PICO8, featuring:
* Complex geometry (slanted walls, stairs, doors...)
* Textured floors & walls (inc. transparent walls, animated triggers)
* Lightning (blinking sectors, light triggers)
* 8 sided sprites for monsters & props
* Triggers (opening/closing doors)
* Keys
* Multiple weapons (bullets & projectiles)

Level building and monster logic uses standard DOOM concepts & editors and is fully modable.

>note: the toolkit is NOT a 100% complete DOOM port - many features are not supported (and never will be).

>note: this documentation is using [Slade3](https://slade.mancubus.net/)

>note: the game does not require an official DOOM WAD

# Credits

Zdoom Wiki (outstanding content)

# Getting Started
## Pre-Requisites

* [PICO8](ttps://www.lexaloffle.com/pico-8.php) to run game
* [Python 3.6+](https://www.python.org/) to pack levels into pico8 carts
* [Slade 3](https://slade.mancubus.net/) to edit levels
* [ZBSP](https://zdoom.org/files/utils/zdbsp/zdbsp-1.19.zip) to produce UDMF files from levels

## Run Poom
1. Fork this repo
2. Open a Python command prompt at repo location, run:
```
compile.pex .. poom
```
The command generate a game the sample level included in SDK.
3. Launch game:
```
pico8 -home <path to repo> poom.p8
```

# Make a Game!
## Level Building

The toolkit supports file-based DOOM archive only.

Archive structure:
* :file_folder: maps/ contains levels WAD files
* :file_folder: graphics/ contains image assets (inc. menu)
* :page_facing_up: PLAYPAL game palette ramp
* :page_facing_up: PAINPAL pain palette ramp
* :page_facing_up: DECORATE define actors & behaviors
* :page_facing_up: TEXTURES defines floor & wall textures

### My First Level

Open poom-sdk level folder:
![docs/open_dir.png]

Open the maps folder from Slade:
![docs/open_maps.png]

Create a new map (say E1M3) using UDMF format.
> maps must be named ExMx
![docs/map_type.png]

> make sure to set the base archive to poom
![docs/map_base_archive.png]

From the map editor, select the "Draw Shape" option (defaults to rectangle), create a new room:
![docs/create_room.png]

Select all four walls and assign "middle" texture:
![docs/set_middle_texture.png]

Switch to sector mode and assign floor and ceiling textures:
![docs/sector_textures.png]

Check level using 3d view:
![docs/3d_view.png]

Save level under: _repo location_/maps

Slade3 will display the settings window to get ZBSP compiler path. This is a good time to fill it with your own path:
![docs/zbsp_settings.png]

Switch to "Things" mode to set player's starting location. Right click inside the room:
![docs/create_thing.png]

Edit thing type (should not be necessary as first thing created by Slade is player's location):
[!docs/player_settings.png]

Select thing ID 1 (e.g. POOM guy!):
[!docs/select_poomguy.png]

### Texturing
All wall textures must be stored inside a single image. The tileset can be up to 1024x128 pixels.

The toolkit automatically converts tileset into unique 8x8 tiles. 
> The tileset cannot contain more than 128 unique tiles.

Example tileset:
![graphics/TILES.png]

Resulting tiles:
![docs/tiles.png]


### Palettes

The game support 2 palettes:
* Game palette - used to shade textures & things
* Pain palette - used to fade to red screen when player gets hit

#### Game Palette

Must be a 16x16 image using *only* colors from row 0 of pain palette.

Ex:
![graphics/PLAYPAL.png]

#### Pain Palette

Must be a 16x16 image - can use any PICO8 colors (inc. secret colors).

Ex:
![graphics/PAINPAL.png]

Row 0 is the default game palette.

Tip: use @Kometbomb [Fade Generator](http://kometbomb.net/pico8/fadegen.html) to produce pain palette!

### Sector Specials

The following sector behaviors are supported:

| ID | Type | Description |
|----|:----:|:----------:|
| 65 | Light Flicker | Random light flicker |

### Line Specials

The following triggers are supported:
| ID | Type | Description |
|----|:----:|:----------:|
| 202 | Generic Door | Lower/raise sector ceiling. See: [Generic Door](https://zdoom.org/wiki/Generic_Door) |
| 64 | Platform Up/Down/Stay | Lower/raise sector floor. See: [Platform Up/Down/Stay](https://zdoom.org/wiki/Plat_UpWaitDownStay) |
| 112 | Light Change | Set sector light. See: [Light Change To Value](https://zdoom.org/wiki/Light_ChangeToValue)

### Animated Triggers

## Monsters & Props

### Standard Classes














