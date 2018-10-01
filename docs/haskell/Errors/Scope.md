# Scope Converter Error
Errors that occur when performing scope interpretation and conversion to netlist data.
The errors are contained in the netlist (parser) [scope types](../../src/Parser/Netlist/Types/Scope.hs) file.
They are held in the type `ScopeConverterError`.
All these errors are wrapped with the position where the error occurred in the file.
Note that all these errors have an associated position printed after their error message.

## Invalid Library
Libraries used in the design must have a specified directory to be valid.
Cannot refer to a library in the design if there is not a valid connected library to find design entities in.

**NOTE**: Currently only two libraries supported (`WORK` and `IEEE`)

### Constructor
`ScopeConverterError_InvalidLibrary`

### Data Contained
- `String`: Invalid library name

### Error Message
"invalid library: \<library name\>"

## Invalid Operator
When importing operators, the operator name is specified in a string literal, e.g. `"and"`.
This doesn't get tokenised to an operator because it is a string literal, so a non-existent operator could be named in the string.
If an import statement tries to import an operator that doesn't exist, this error occurs.

### Constructor
`ScopeConverterError_InvalidOperator`

### Data Contained
- `String`: "Operator" named in the string literal

### Error Message
"invalid operator in use clause: \<operator string\>"

## Library Not in Scope
If an import statement uses a library name in an import statement without first declaring it will use the library with a library clause.
Library name referred to when trying to import a design entity from within a library or a declaration from a design entity in a library.

### Constructor
`ScopeConverterError_LibNoScope`

### Data Contained
- `String`: Library name

### Error Message
"the library is not in the current scope: \<library name\>"

## Invalid Declaration in Package
The declaration that appears in the import statement does not exist in the package you are trying to import it from.

### Constructor
`ScopeConverterError_InvalidDeclare`

### Data Contained
- `String`: Declaration name
- `NetlistName`: Library and package name attempting to import from

### Error Message
"the declare: "\<declaration name\>" cannot be found in the package: "\<package name\>""

## Invalid Operator Declaration in Package
The operator declaration that appears in the import statement does not exist in the package you are trying to import it from.
This will occur the package contains no overloaded declarations of that operator function.

### Constructor
`ScopeConverterError_InvalidOpDeclare`

### Data Contained
- `Operator`: Operator attempting to import
- `NetlistName`: Library and package name attempting to import from

### Error Message
"the operator declare: "\<operator name\>" can not be found in the package: "\<package name\>""

## Cyclic Dependency
Cyclic dependency between two modules.
This can be directly imported, IE when an imported entity imports the entity that imported it.
Alternatively it can be somewhere in the chain of dependencies.

### Constructor
`ScopeConverterError_CyclicDependency`

### Data Contained
- `NetlistName`: Name of library and design entity that is already in the dependency chain
- `[NetlistName]`: List of library and design entity names that make up the dependency chain

### Error Message
"a cyclic dependency was detected, \<first package\> tried to import \<lowest import package\>. But the dependency chain prevents this: \<dependency chain\>"
