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

## Build Process

### Prerequisites

In order to run `slox` and its tests, the following programs must be installed:
`sbt 1.11.6` and `scala 3.7.3`.

To best way to install these programs is to use `nix develop` with Nix
installed.

### Running and Testing

To run `slox`, run the command `sbt`. This will bring up a `sbt` server that
enables fast compilation with the `compile` command and fast execution with the
`run` command.

To run the REPL, run `stb run`
To run a file, run `sbt "run file"
