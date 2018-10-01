# Documentation
## VHDL
Currently only one version being implemented.
Aim to have full implementation of 1076-1987 before starting on others.

## Simulation
The generated simulation is in the form of a Haskell package.
Documentation of the simulation output can be found [here](haskell/Simulation-Output.md).

## Haskell Implementation Minutia
- [Data types](haskell/types.md)
- [File Location](haskell/File-Finding.md)
- [Type conversions](haskell/TypeConversion.md)
- [Subprogram Declarations](haskell/Subprograms.md) _Not yet implemented_
- [Object Declarations](haskell/Objects.md)

## Automated Documentation
Much of the code has been commented in order to automatically generate documentation.
This is done using the [Haddock](https://github.com/haskell/haddock) tool and may be useful when contributing to the code.

To generate this automated documentation, run the command `stack haddock`.
Alternatively, this is automatically run and published for the master branch, and the generated documentation can be found [here](matthewar.github.io/ASV).

## Tool
- [Errors](haskell/Errors.md)
