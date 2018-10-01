[![Travis](https://travis-ci.com/Matthewar/ASV.svg?branch=feature%2Fparser%2Frefactor)](https://travis-ci.com/Matthewar/ASV)
[![Coveralls](https://coveralls.io/repos/github/Matthewar/ASV/badge.svg?branch=feature%2Fparser%2Frefactor)](https://coveralls.io/github/Matthewar/ASV?branch=feature%2Fparser%2Frefactor)

# ASV Simulates VHDL
Read the [documentation](docs/README.md) before contributing, or for further details about the project.

See the issues for potential contributions to be made.
For first time contributors, there may be issues with the 'good first issue' label to indicate these are more approachable.

If you want to report a bug, please see the [bug reporting section](#reporting-bugs).

For a quick start go to [Running a Simulation](#creating-and-running-a-simulation).

## Other Pages
- [Documentation](docs/README.md)
- [Haddock Documentation](https://matthewar.github.io/ASV/haddock)
- [Haddock Coverage Summary](https://matthewar.github.io/ASV/haddock-cov/Report.md)
- Code Coverage
   - [Basic Summary](https://matthewar.github.io/ASV/tests/Coverage.md)
   - [Test List](https://matthewar.github.io/ASV/tests/List.md)
   - [Coveralls](https://coveralls.io/github/Matthewar/ASV)

## Introduction
This project is a VHDL simulator, whose initial aim is to implement the entirety of the 1076-1987 VHDL specification.
Currently a subset of the language has been implemented, see the documentation for details on features.
Issues may have more up to date progress on feature implementation.

## Prerequisites
The only tool required to use this software is [Stack](https://github.com/commercialhaskell/stack).
This tool will install all dependencies as required when building the tool.

### Ubuntu/Debian
To get `stack`, simply run the command:
```
sudo apt-get install haskell-stack
```

## Stack Commands
Installing and using the tool is done through the `stack` tool.

- To build: `stack build`
- To test: `stack test`
- To generate documentation: `stack haddock`
- To run tool: `stack exec asv`

### Creating and Running a Simulation
- Build the tool: `stack build`
- Put HDL files to be parsed into a directory (`<work-dir>`)
   - NOTE: Automatic file finding is not supported, any package/entity/architecture of name `name` should be in file `<work-dir>/NAME.vhd`
   - Multiple architectures for a single entity are also not supported, entity and single linked architecture should be in the same file
- Create IEEE package directory (`<ieee-dir`>)
   - IEEE files are not fully parsable by the tool because of aliases and aggregates, so an empty directory is sufficient
- Run tool through stack
   - Stack puts compiled binary (from tool build stage) into a hidden folder, so run through command
   - `stack exec asv -- --top <top-file> --build-dir <build-dir> --work-dir <work-dir> --ieee-dir <ieee-dir>`
      - `<top-file>`: Name of top level entity/architecture to compile (case insensitive)
      - `<work-dir>`, `<ieee-dir>`: As specified earlier
      - `<build-dir>`: Directory to place simulation files (directory should already exist and preferably be empty)
- Compile/Run simulation
   - The tool creates a stack project for the simulation (in auto-generated Haskell)
   - It currently does not automatically run it
   - Go to `<build-dir>`
   - Build simulation with `stack build`
   - Run simulation with `stack exec VHDL-Sim-exe -- --max-time <max-time>`
      - `<max-time>`: Maximum time to run the simulation, format `[0-9]+(fs|ps|ns|us|ms|sec|hr)`

## Documentation
Documentation [here](docs/README.md)

See also the [Haddock documentation](matthewar.github.io/ASV/haddock).

## Similar Projects
Most VHDL simulators are closed source products, this project aims to provide an open source tool that anyone can use.

This was largely inspired by [GHDL](https://github.com/ghdl/ghdl), another open source simulator.
GHDL implements a large portion of the VHDL specification however it is written mostly in Ada, an old and not widely used language which could discourage contributors.
An aim of this project is provide code that can be understood and modified easily.

## Reporting Bugs
To report a bug, please use the issues section of this repository.
The following information is useful for replicating the bug, in order to test it:
- Version of the tool (what commit you are currently running on)
   - If you are not on an up-to-date version of the tool, please update and try again before reporting an issue
- Command run that triggered the bug
- Output of the tool (any error or debug messages that have occurred)
- (If the bug occurred when parsing a design) Any files used by the simulator IE contents of library directories

## Licensing
While this repository is private, there is no license.

Once published either the MIT or GPLv3 license will apply.
