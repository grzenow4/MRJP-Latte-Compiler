# MRJP-Latte-Compiler

## Description
You can read detailed description in either [polish](Latte.md) or [english](Latte-en.md).

## Implemented extensions
- arrays
- structs
- objects - attributes, methods, single inheritance (without virtual methods yet)

## Usage
To build and run the project execute
```bash
make
./latc_x86_64 foo/bar/baz.lat
./foo/bar/baz
```

## External libraries
- [BNFC](https://bnfc.digitalgrammars.com/)

## Project directory structure
- [lib](lib) - contains [runtime.c](lib/runtime.c)
- [src](src) - project's root directory
    - [Latte](src/Latte/) - automatically generated with BNFC, based on [grammar](src/Latte.cf)
    - [Backend](src/Backend/) - contains files responsible for x86 code generation
    - [Frontend](src/Frontend/) - contains [TypeChecker](src/Frontend/TypeChecker.hs) for the Latte language
    - [Utils](src/Utils/) - contains the utility functions for both [Backend](src/Utils/Backend.hs) and [Frontend](src/Utils/Frontend.hs)
    - [Main](src/Main.hs) - main file that compiles latte programs
    - [Makefile](src/Makefile) - builds the project, it is used by the main [Makefile](Makefile)

## Examples
You can run the compiler on example programs in [good](good/) and [extensions](extensions/) directories using bash [script](script.sh):
```bash
make
./script.sh      -- compile all the example files
./script.sh exec -- execute all binaries and compare the outputs
```
