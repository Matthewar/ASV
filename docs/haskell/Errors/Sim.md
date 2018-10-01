# Simulation Output Generator Error
Errors that occur when performing conversion of the netlist to the simulation output.
The errors are contained in the [sim error](../../src/Sim/Types/Error.hs) file.
They are held in the type `SimOutputError`.

## Constant is missing a value
Within a package, constants can have deferred values:
- constant name is declared in the package header (primary unit)
- constant value is declare in package body (secondary unit)
Constant values must be evaluated before converting to a simulation.
If not, an error will be thrown.

### Constructor
`SimErr_ConstantNoValue`

### Data Contained
- `String`: Constant name

### Error Message
"no value was specified for the deferred constant \<name\>"

## Cannot find a design entity
Cannot find the file specified in the host file system.

### Constructor
`SimErr_NoEntityOrArchWithTopModuleName`

### Data Contained
- `NetlistName`: Name of entity that cannot be found (library and unit)

### Error Message
"cannot find an entity or architecture with the name \<library\>.\<unit name\>"

## Incomplete top level generics
Top level ports and generics are not linked, therefore generic values must use their defaults.
If a default is not provided this is an error.

### Constructor
`SimErr_TopGenericsWithoutDefaultValues`

### Data Contained
N/A

### Error Message
"the top level module generics must have default values provided"
