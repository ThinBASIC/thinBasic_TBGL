# thinBASIC TBGL module

This repository contains code of thinBasic module, which is in continuous development for 14 years.

The first public version of the module was released on September 21st, 2005 by Petr Schreiber as an experimental way to add OpenGL support to thinBasic programming language.

Since then, the scope has grown bigger, adding support for basic texture loading, custom model format, scene-entity system and much more - including 2D sprite system kindly contributed by Michael Hartlef, along with useful 2D primitive functions.

TBGL module is part of the standard thinBASIC installation and the development is led by Petr Schreiber, who would like to continue the development here, on the official thinBasic GitHub.

## Why is this at GitHub?

Exposing the code will make the further development more visible and it will allow community to join it, in case of interest.

## Disclaimer

- the quality of the code is proportional to the length of the development. Many places will give you a good laugh, many will bring back memories (does anybody still remember sublevel6 openGL tutorials?)

- the project is coded using PowerBASIC compiler, however, the future of this compiler is unknown and might be replaced by different technology in the future

## Core principles

TBGL is not intented to be a 1:1 OpenGL wrapper, but a higher level graphic library to be used for visualizations or simple game concepts.

The ultimate goal would be to purify the design as much as possible to be OpenGL independent - this will allow to port it to more modern backend in the future, such as Vulkan.

TBGL design prioritizes ease of use over raw speed efficiency.

## Compiling the module
In order to compile the module, you will need:
- PowerBASIC for Windows, v10.04
- WinAPI headers [from José Roca](https://forum.powerbasic.com/forum/jose-s-corner/downloads/61213-windows-api-headers-iii-v-1-07)

## Collaboration guidelines

> Contributions are welcome, however, before investing time to the project, please keep in mind thinBasic TBGL project is developed using compiler, which is no longer actively maintained and [Petr Schreiber](https://github.com/petrSchreiber), while the leader of the module project, might not be able to react immediately.

Adding new functionality should be always consulted ahead via GitHub issue.

Pull request should be placed from branch on forked repo with a single commit and linked with gitHub Issue.

### New feature
- branch name `feat_<description>`
- commit message `feat: <description>`

### Bugfix
- branch name `fix_<description>`
- commit message `fix: <description>`

### Refactoring
- branch name `refactor_<description>`
- commit message `refactor: <description>`

### Minor chores (typos, ...)
- branch name `chore_<description>`
- commit message `chore: <description>`

### Documentation adjustment
- branch name `doc_<description>`
- commit message `doc: <description>`

## Acknowledgement

I would like to thank Eros Olmi for creating thinBASIC and letting me join the adventure.

I would like to thank José Roca for providing wonderful OpenGL & WinAPI headers.

I would like to thank Bob Zale, may he rest in peace, for creating practical, straightforward compiler.

I would like to thank Mike Hartlef for cool Sprite2D and 2D primitives for TBGL.

I would like to thank PowerBASIC community which helped me to solve many challenges along the way.


*Petr Schreiber, Dec 24, 2018*
