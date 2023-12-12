<h1 align="center">Haskell Raycaster</h1>
<p align="center">
    <img width="400" height="225" src="assets/docs/raycaster.gif">
</p>

## About the Haskell Raycaster

[![CodeFactor](https://www.codefactor.io/repository/github/joshuacrotts/raycaster-haskell/badge)](https://www.codefactor.io/repository/github/joshuacrotts/raycaster-haskell) ![GitHub contributors](https://img.shields.io/github/contributors/joshuacrotts/raycaster-haskell) ![GitHub commit activity](https://img.shields.io/github/commit-activity/m/joshuacrotts/raycaster-haskell) ![GitHub repo size](https://img.shields.io/github/repo-size/joshuacrotts/Raycaster) [![GitHub issues open](https://img.shields.io/github/issues/joshuacrotts/raycaster-haskell)]()
[![GitHub issues closed](https://img.shields.io/github/issues-closed-raw/joshuacrotts/Raycaster)]()

This is a (very minimalistic) raycasting engine written in Haskell. As is tradition with these types of engines, we provide (in the left-hand side of the screen) a top-down/overhead perspective of the raycasting, which shows the walls and player position, as well as each ray as it collides with the walls. To the right, we cast the rays into the third dimension. 

## Dependencies

This program uses `stack`, `cabal`, and the `SDL2` bindings. 

## Rebuilding Raycaster

Clone the repository to your computer and run `stack build`, followed by `stack run`. This should,in theory, correctly build and execute the project, but if not, you may need to manually install the SDL2 bindings for your operating system. We have not tried this project on Windows, nor do we believe thta it would work correctly on Windows. 

### Installing SDL2

**MacOS**: For MacOS, run the following commands in your terminal to get the appropriate development files for SDL:

1. <code>brew install SDL2</code>
2. <code>brew install SDL2_image</code>
3. <code>brew install SDL2_ttf</code>
4. <code>brew install SDL2_mixer</code>

**Linux (Ubuntu/Debian)**: The process is similar to the former, with the exception of having to install SDL files to your system in Linux. Run the following commands in your terminal:

1. <code>sudo apt-get install libsdl2-dev</code>
2. <code>sudo apt-get install libsdl2-image-dev</code>
3. <code>sudo apt-get install libsdl2-ttf-dev</code>
4. <code>sudo apt-get install libsdl2-mixer-dev</code>

**Linux (Arch-Based Distros)**: Run the following commands after cloning the repo to install SDL2:

1. <code>sudo pacman -S sdl2</code>
2. <code>sudo pacman -S sdl2_image</code>
3. <code>sudo pacman -S sdl2_ttf</code>
4. <code>sudo pacman -S sdl2_mixer</code>

## Reporting Bugs

See the Issues Tab.

## Version History

The **master** branch encompasses all changes.