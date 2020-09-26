# POOM SDK
This repository contains everything required to produce a DOOM-like using PICO8, featuring:
* Complex geometry (slanted walls, stairs, doors, elevators...)
* Textured floors & walls (inc. animated triggers & textures)
* Lightning (blinking sectors, light triggers)
* 8 sided sprites for monsters & props
* Data driven monsters & props
* Triggers (opening/closing doors, elevators, light...)
* Keys
* Multiple weapons (bullets & projectiles)

Level building and monster logic uses standard DOOM concepts & editors and is fully modable.

>note: the toolkit is NOT a 100% complete DOOM port - many features are not supported (and never will be).

>note: this documentation is using [Slade3](https://slade.mancubus.net/)

>note: the game does not require an official DOOM WAD

# Credits

Original assets - Id Software (Microsoft)

ZDoom Wiki (outstanding content folks!)

# Getting Started
## Pre-Requisites

* [PICO8](ttps://www.lexaloffle.com/pico-8.php) to run game
* [Python 3.6+](https://www.python.org/) to pack levels into pico8 carts
* [Slade 3](https://slade.mancubus.net/) to edit levels
* [ZBSP](https://zdoom.org/files/utils/zdbsp/zdbsp-1.19.zip) to produce UDMF files from levels

## Install Toolchain
1. Clone repo
2. Open a Python command prompt at repo location
3. (optional) Create a Python virtual env:
    ```shell
    python -m venv env
    sandbox/script/activate
    ```
4. Install WAD compiler:
    ```shell
    pip install tools/wad_reader-<latest version>.tar.gz
    ```
5. Validate install
    ```shell
    python -m wad_reader --help
    ```
    Expected output:
    ```shell
    usage: wad_reader.py [-h] --pico-home PICO_HOME --carts-path CARTS_PATH --mod-name MOD_NAME [--map MAP]

    optional arguments:
      -h, --help            show this help message and exit
      --pico-home PICO_HOME
                            full path to PICO8 folder
      --carts-path CARTS_PATH
                            path to carts folder where game is exported
      --mod-name MOD_NAME   game cart name (ex: poom)
      --map MAP             map name to compile (ex: E1M1)
      --compress            Enable compression (default: false)  
    ```

## Compile & Run Poom
1. Open a Python command prompt at repo location (e.g. where DECORATE file is)
2. (optional) Enable Python virtual env
3. Generate a PICO8 multi-cart game with a sample level in carts folder:

```shell
python -m wad_reader --pico-home <path to PICO8> --carts-path <path to carts folder> --mod-name <mod name> --map E1M1
```
Example:
```shell
cd poom-sdk
python -m wad_reader --pico-home d:\pico-8_0.2.0 --carts-path carts --mod-name poom --map E1M1
```

4. Launch game:
```shell
pico8 -home <path to repo> poom.p8
```

# Controls

## Player 1 keys

⬇️⬆️: back/forward

⬅️➡️: turn left/right

🅾️ + ⬅️➡️: strafe

❎: fire

## Player 1 + 2 keys

> use keyconfig to map second player keys

WASD or local variant: back/forward/strafe

⬅️➡️: turn left/right

❎: fire

## Weapon Selection

pause: open weapon selection wheel

pause: regular pico-8 pause menu

⬇️⬆️⬅️➡️: select weapon

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
* :page_facing_up: ZMAPINFO defines map options & sequence

### My First Level

Open poom-sdk level folder:

![Open Directory](docs/open_dir.png)

Open the maps folder from Slade:

![Open Maps](docs/open_maps.png)

Create a new map (say E1M3) using UDMF format.
> maps must be named ExMx

![Map Type](docs/map_type.png)

Set the base archive to POOM (to use main game resources)

![Base Archive](docs/map_base_archive.png)

From the map editor, select the "Draw Shape" option (defaults to rectangle), create a new room:

![Create Room](docs/create_room.png)

Select all four walls and assign "middle" texture:

![Texture](docs/set_middle_texture.png)

Switch to sector mode and assign floor and ceiling textures:

![Sector Textures](docs/sector_textures.png)

Check level using 3d view:

![3d view](docs/3d_view.png)

Save level under: _repo location_/maps

Slade3 will display the settings window to get ZBSP compiler path. This is a good time to fill it with your own path:

![ZBSP Settings](docs/zbsp_settings.png)

Switch to "Things" mode to set player's starting location. Right click inside the room:

![Create Thing](docs/create_thing.png)

Edit thing type (should not be necessary as first thing created by Slade is player's location):

![Player Settings](docs/player_settings.png)

Select thing ID 1 (e.g. POOM guy!):

![Select POOMGuy](docs/select_poomguy.png)

## Sector Specials

The following sector "special" behaviors are supported:

| ID | Type | Description |
|----|:----:|:-----------:|
| 65 | Light Flicker | Random light flicker |
| 69 | 10 Damage | 10 HP every 15 ticks |
| 71 | 5 Damage | 5 HP every 15 ticks |
| 80 | 20 Damage | 20 HP every 15 ticks |
| 84 | 5 Damage + Scroll East | 5 HP every 15 ticks + floor texture scrolling |
| 115 | Instadeath | Kills any actor touching sector floor |

## Line Specials

The following triggers are supported:
| ID | Type | Description |
|----|:----:|:----------:|
| 202 | Generic Door | Lower/raise sector ceiling. See: [Generic Door](https://zdoom.org/wiki/Generic_Door) |
| 64 | Platform Up/Down/Stay | Lower/raise sector floor. See: [Platform Up/Down/Stay](https://zdoom.org/wiki/Plat_UpWaitDownStay) |
| 112 | Light Change | Set sector light. See: [Light Change To Value](https://zdoom.org/wiki/Light_ChangeToValue)

### Animated Triggers

# Graphics

## Flats
All wall & floors textures (aka "flats") must be stored as single image. The tileset can be up to 1024x128 pixels.

The toolkit automatically converts tileset into unique 8x8 tiles. 
> The tileset cannot contain more than 127 unique tiles.

Example tileset:

![Tileset](graphics/TILES.png)

Resulting PICO8 tiles:

![In Game Tiles](docs/tiles.png)

### Creating New Textures

Edit the tileset picture using your favorite paint program. Make sure to use *only* colors from the game palette, save file.

From Slade, launch the texture editor:

![Texture Editor](docs/open_texture.png)

Create a new texture:

![New Texture](docs/new_texture.png)

Specify texture name (to be used in editor) and size (in pixels):

![Texture Settings](docs/texture_params.png)

Attach a patch (e.g. source image) to the texture:

> make sure to use the full path option to locate the tileset

![Patch Selection](docs/patch_selection.png)

Move the texture area over the correct patch location:

![Patch](docs/adjust_patch.png)

> Adjust texture scale to 0.5/0.5 to match in-game scale.

Save.

The texture is now available in level editor!

### Texture Switches

Line triggers (ex: open door button) will automatically switch between textures named xxx_ON/xxx_OFF

## Palettes

The game support 2 palettes:
* Game palette - used to shade textures & things
* Pain palette - used to fade to red screen when player gets hit

### Game Palette

Must be a 16x16 image using *only* colors from row 0 of pain palette.

Default palette:

![Game palette](docs/playpal.png)

### Pain Palette

Must be a 16x16 image - can use any PICO8 colors (inc. secret colors).

Default palette:

![Pain palette](docs/painpal.png)

Column 0 is the game palette.

> Tip: use @Kometbomb [Fade Generator](http://kometbomb.net/pico8/fadegen.html) to produce pain palette!


## Game Title

Game title is packaged from graphics/m_title.png image.

![Title Image](graphics/M_TITLE.png)

> Image must be 128x32

> Image must use standard PICO8 palette

# Maps

The ZMAPINFO file contains the list of map metadata (name, music...).

The file syntax is:
```
map <maplump> <nice name> { properties }
```
where supported properties are:
| property | Type | Description |
|----------|:----:|:-----------:|
|levelnum  | integer | level sequence. Only required to mark first level |
|next | string | lump name of next level. Use ```"endgame"``` to move to game over screen|
|music | integer | (optional) background music identifier. See [Music & Sound]() section |

Example:
```C
map E1M1 "Hangar"
{
	levelnum = 1
	next = "E1M2"
	music = 0
}

map E1M2 "Docks"
{
  // optional
	levelnum = 2
	next = "endgame"
	music = 12
}
```

# Monsters & Props

The DECORATE file describes everything the player will find in the game (monsters, keys, medkits, props...). Each entry is an **actor**. An actor on the map is a **thing** (e.g. a thing always references an actor).

## Syntax
```C
actor classname [ : parentclassname] [doomednum]
{
  properties
  flags
  states
}
```

Example: light pole
```C
actor Column 2028
{
 Radius 16
 Height 30
 +SOLID
 States
 {
	Spawn:
	 LIGH A -1
	 Stop
 }
}
```

* classname

  The name this new actor is referenced by in the game. While ZDoom will accept a much larger range of values for the name, this should for best compatibility be a valid identifier (alphanumeric plus underscores, but not starting with a digit).
* parentclassname

  The name of a parent class this new actor inherits its attributes from (optional). If none is specified, the parent class is Actor.
  
* doomednum

  Editor number for this actor (optional). This is the number used to distinguish the actor from other things in map. If the actor is intended to be placed in a map editor, it should have an editor number. The actual number value is generally arbitrary but should avoid conflicting with already used numbers.

An actor definition consists of properties, flags and state definitions. In the state definitions you can call Action functions.

Actor properties and flags define the general behavior of an actor.

States define the various sprite animations of an actor.

Action functions (also known as "code pointers") cause the actor to perform some particular action when the frame that calls them is shown. They form the basis of almost all enemy and weapon behavior in the game. Instead of using one of the special action functions you can also use almost any action special that is available in ACS.

Comments are supported. Both types of C-style comments (// to end of line, and /* to */) are allowed. While not part of the specification, certain editing tools, such as Doom Builder and SLADE 3, make use of specific comments for special purposes, these usually start with //$.

## Standard Classes

The following actors are built-in parent class names:

### key
```
actor key {
    amount 1
    maxamount 1
    radius 20
  }
```
### ammo
```
actor ammo {
    radius 20
  }
```
### health
```
actor health {
    maxamount 200
    radius 20
  }
```
### armor
```
actor ammo {
    maxamount 200
    radius 20
  }
```
### weapon
```
actor weapon {
    radius 20
    amount 0
    maxamount 1
  }
```

### player
```
actor player {
    radius 32
    armor 100
    health 100
    speed 3
    +SHOOTABLE
    +SOLID
  }
```
### monster
```
actor monster {
    radius 20
    armor 0
    health 50
    +SHOOTABLE
    +SOLID
  }
```
### projectile
```
actor projectile {
    damage 1
    speed 5
    +NOGRAVITY
    +MISSILE
  }
```

## Actor Properties

The following properties can be set in an actor's DECORATE {} block definition. They control various characteristics of your actor, from appearance to behavior or the actor's physical properties. To use, simply specify the property and any associated values on its own line within the actor's definition and outside the state block. 

Supported properties:

### Health _value_
Defines the health a monster or any other shootable item starts with.

Default is 1000.

### Armor _amount_
The amount of armor that this item gives.

### Height _value_
Defines the height of this actor.

Default is 16.

> using very small values can provoke glitches in collision detection.

### Radius _value_
Defines the radius of this actor.

Default is 20.

> Notes:
Using very small values can provoke glitches in collision detection.
The total diameter is double this value, so an actor with a radius of 64 is actually 128 units in length and width, making it impossible for them to navigate through a corridor that's 128 units wide, for example.

### Weapon.SlotNumber _value_
Default slot for this weapon. 

### Inventory.Amount _value_
Sets the amount of inventory items given by this item. Mostly used for item types that give larger quantities.

### Inventory.MaxAmount _value_
Sets the maximum amount the player can carry of this item.

### Damage value

For a projectile defines the damage it inflicts upon impact. The formula is random(1,8) * damage.

Default is 0.

### Speed _value_
Defines how fast an actor moves. For projectiles this is the distance it moves per tic (1/35 seconds). For monsters it defines the size of one step done in A_Chase. 

Default is 0.

### Weapon.AmmoGive _amount_

The amount of primary ammo you receive from this weapon.

### Weapon.AmmoUse amount
The amount of primary ammo the weapon uses per shot.

### Inventory.Icon _sprite_
Defines the icon this item uses when displayed in the HUD or status bar.

_sprite_ is the Pico-8 character code (as given by ord())

### AttackSound _sound_
Defines the sound the actor makes when attacking.

_sound_ is the sfx id from the user supplied music cart.

### Inventory.PickupSound _sound_
Defines the sound that is played when a player picks up this item.

_sound_ is the sfx id from the user supplied music cart.

### DeathSound _sound_
Defines the sound the actor makes when dying or when a projectile explodes. For non-projectiles the death sound must be explicitly played with A_Scream.

_sound_ is the sfx id from the user supplied music cart.

### MeleeRange value
Specifies the maximum distance to a target where a melee attack inflicts damage. Distance is calculated from the attacker's center to the target's center.

Default is 64.

### MaxTargetRange _value_
A monster with this property set will not attack its target unless it is within the specified range. This is used by A_Chase and similar functions.

### Weapon.AmmoType _type_
The type of primary ammo the weapon uses. This must be a valid ammo type.

### Player.StartItem _classname_ [_amount_]
Adds an item to player's start inventory. First weapon added is the weapon selected at start.

> The initial startitem list is never inherited and must be specified in full for each player class.

## Actor Flags

Flags control various characteristics of your actor, varying from appearance to physical properties. To use, simply specify the flag within the actor's DECORATE definition and outside the state block. You can set or clear a flag as follows:

+FLAGNAME sets a flag
-FLAGNAME clears a flag

Supported flags:

### SOLID

Set when the object should be solid (blocking). The size of the blocking is defined using the height and radius properties.
> Automatically given by the Monster combo

### SHOOTABLE

Object can be damaged. If health goes below 0 it enters its death state.
> Automatically given by the Monster combo

### ISMONSTER

Actor is classed as a monster.
> Automatically given by the Monster combo

### MISSILE
Actor is a projectile. Actors with this flag set will enter their death state when hitting a solid and constantly move at their speed value (without the need of any actor functions) 

Actors with this flag will also be able to go through impassable linedefs.

> Automatically given by the Projectile combo


pack_flag(actor, 'solid') | pack_flag(actor, 'shootable')<<1 | pack_flag(actor, 'missile')<<2 | pack_flag(actor, 'ismonster')<<3

# Music & Sound

Music and sound is totally modable.
Sound is stored in:
```
carts/music.p8
```
This file can be modified as needed. music+sfx sections will be merged with main gamecart at compile time.

## Reserved Sounds

The following sound effects ID are reserved for game engine use:
- 62: blocked button/door sound
- 63: door & platform open/close


















