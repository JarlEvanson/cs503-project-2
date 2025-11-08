# Project 2 - Lox

## Project Description

The project provides an implementation of a Lox interpreter.

## Organization

The code is organized into a single directory (`src/main/scala/slox`) with a
very similar file structure as given by the `Crafting Interpreters Part II`
book.

The test runner is located at `src/test/scala/test.scala` and runs integration
tests located in `src/test/resources`. The integration tests run a lox file and
compare the output to its corresponding `.output` file.

The tests located in `src/test/resources/crafting-interpreters/` are direct
ports of the official `jlox` test suite.

The other tests located in `src/test/resources/` are tests made to validate the
functionality of the interpreter.

## Build Process

### Prerequisites

In order to run `slox` and its tests, the following programs must be installed:
`sbt 1.11.6` and `scala 3.7.3`.

To best way to install these programs is to use `nix develop` with Nix flakes
installed.

### Running and Testing

To run the REPL, run `stb run`
To run a file, run `sbt "run file"`
To run the tests, run `sbt test`

For faster and perhaps clearer testing, run the command `sbt`. This will create
a server that enables fast compilation with `compile` and fast execution
with `run`. One can run a file using the server by calling `run file`,
otherwise an invocation of `run` starts the scripting interface.
