# VHDL-Sim
## Prerequisites
Just need `stack`, it should install all dependencies.

### Ubuntu/Debian
To get `stack`, simply run the command:
```
sudo apt-get install haskell-stack
```

## Stack Commands
To build: `stack build`

To test: `stack test`

To generate documentation: `stack haddock`

To run tool: `stack exec VHDL-Sim-exe`

### Creating and Running a Simulation
- Build the tool: `stack build`
- Put HDL files to be parsed into a directory (`<work-dir>`)
   - NOTE: Automatic file finding is not supported, any package/entity/architecture of name `name` should be in file `<work-dir>/NAME.vhd`
   - Multiple architectures for a single entity are also not supported, entity and single linked architecture should be in the same file
- Create IEEE package directory (`<ieee-dir`>)
   - IEEE files are not fully parsable by the tool because of aliases and aggregates, so an empty directory is sufficient
- Run tool through stack
   - Stack puts compiled binary (from tool build stage) into a hidden folder, so run through command
   - `stack exec VHDL-Sim-exe -- --top <top-file> --build-dir <build-dir> --work-dir <work-dir> --ieee-dir <ieee-dir>`
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
