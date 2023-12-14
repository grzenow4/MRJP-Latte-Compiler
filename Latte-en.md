# Latte Compiler

## Stages

The project consists of three stages:
1. Front-end: syntactic and semantic analysis.
2. LLVM or x86 back-end.
3. (optional) Extensions.

Your lab supervisor can set slightly different deadline, in particular require project presentation in person. Nonetheless, all projects must be submitted to moodle.

## Scoring

At most 34 points in total, counting towards final course mark. To pass the lab and qualify to sit the exam, it is required to submit satisfactory solutions for all stages and get a total of at least 20 points (including points for the [Instant compiler](https://github.com/grzenow4/MRJP-Instant-Compiler)).

Points are awarded for:
1. front-end (4)
2. back-end for LLVM (8) or (exclusive) x86 (10)
3. LLVM: use registers+phi instead of alloca - additional 2p
4. register allocation for x86 - additional (up to) 5p
5. optimisations - up to 10p
    - LCSE/GCSE - 3/5p
    - loop optimisation (inductive vars + strength reduction) 2p
    - function inlining 1-3p
6. Extension (x/y denotes points for LLVM/x86 respectively); a more precise specification of extensions is contained in the [language description](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2023/Latte/description.html).
Late submissions
    - arrays (1)
    - structures (2)
    - objects (attributes, methods, inheritance without method override) - (3/4) extra
    - virtual methods - (3/4) extra (in total 8/10 can be awarded for objects with virtual methods)
    - garbage collection (2)
    - Rust-like memory management (2) (instead of garbage collection)

Structures/objects also denote nested structures/objects and access to fields/methods in the form of `foo.bar.baz`.

To earn points for arrays and structures/objects, arrays of structures and arrays as elements of structures are required.

**Note**: the provided point values are maximum values; the supervisor may assign a lower number of points depending on the quality of the solution and generated code. We expect the code to be generated in accordance with the principles of art learned in class (or better).

## Technical requirements

1. The project must be submitted as a packed TAR archive (.tar.gz, .tgz, tar.bz2 or .tbz).
2. The project root must contain at least:
    - Text file [README](README.md) describing how to compile and run the project, used tools and libraries, implemented extensions, structure of the project, pointers to more detailed documentation.
    - [Makefile](Makefile) allowing to build the program.
    - A directory [src](src/) containing only source files of the project (possibly including [Latte.cf](src/Latte.cf), makefiles etc.); auxiliary files such as libraries, etc. should be placed in other directories.
3. The project must be buildable on lab computers by running `make` in the project root.
4. All necessary libraries (except standard library of the programming language used) must be described in [README](README.md).
5. After the build, project root must contain an executable file `latc` (may be a shell script calling other programs).
6. The compiler must accept all test programs from the directory [good](good/) and reject (with appropriate error messages) all programs from the directory [bad](bad/). For the extensions, the compiler must accept all programs from the respective subdirectories in [extension](extension/). Execution of a correct test program must give output exactly the same as in the corresponding .output file (given input in the corresponding .input file, if any).
7. For an accepted program, the compiler must output OK (`"OK\n"`) and exit with code `0`.
8. For a rejected program, the first line of stderr must be ERROR (`"ERROR\n"`). Further lines should contain appropriate error messages. The compiler must then exit with a non-zero exit code.
9. Solutions requiring the use of non-standard software should be coordinated with the lab supervisors.

### LLVM backend

1. After running `make` the project root should contain executable `latc_llvm`.
2. Running `latc_llvm foo/bar/baz.lat` for a correct program `baz.lat` should create files `baz.ll` (readable LLVM code) and executable `baz.bc` in the directory `foo/bar`.
3. Helper functions (`printInt` etc.) should be placed in the file `runtime.bc` in the [lib](lib/) directory (with sources in `runtime.ll`).

### x86 backend

1. After running `make` the project root should contain executable `latc_ARCH` where `ARCH` is `x86` or `x86_64`.
2. Running `latc_ARCH foo/bar/baz.lat` for a correct program `baz.lat` should create files `baz.s` (assembly) and executtable `baz` in the directory `foo/bar`.
3. Helper functions (`printInt` etc.) should be placed in the file `runtime.o` in the [lib](lib/) directory (with sources in `runtime.c`).
