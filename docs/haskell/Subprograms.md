# Subprogram Declarations
These become functions in Haskell.
Names must involve types because subprograms in VHDL can be overloaded.

## Procedures
### Name
`<haskell_procedure_name> ::= procedure'<PROCEDURE_NAME>'<INPUT_TYPES>`
- `<PROCEDURE_NAME>`: Procedure name (upper case)
- `<INPUT_TYPES>`: List of input types (upper case) separated by the `'` character
   - IE: `<INPUT_TYPES> ::= <INPUT_TYPE>'<INPUT_TYPES>`

### Type

## Functions
### Name
```
<haskell_function_name> ::= function'<designator>'in'<INPUT_TYPES>'out'<OUTPUT_TYPE>
<designator> ::= op'<OP_STRING>
               | iden'<FUNCTION_NAME>
```
- `<OP_STRING>` is described below in the "Operator Map" section
   - String representation of operators
- `<FUNCTION_NAME>`: Identifier function name (upper case)
- `<INPUT_TYPES>`: List of input types (upper case) separated by the `'` character
   - IE: `<INPUT_TYPES> ::= <INPUT_TYPE>'_'<INPUT_TYPES>`
- `<OUTPUT_TYPE>`: Output type name (upper case)

### Type
Relatively simple since functions are just that
`<haskell_function_name> :: <INPUT_TYPES> -> Control'Stack <OUTPUT_TYPE>`
- Note for `<INPUT_TYPES>` that the list is separated by the `->` function symbol

NOTE: How deal with signals

### Operator Map
The following operators correspond to the following strings to create function designators
| Operator Symbol | Haskell String Representation |
| --------------- | ----------------------------- |
| `and`           | `AND`                         |
| `or`            | `OR`                          |
| `nand`          | `NAND`                        |
| `nor`           | `NOR`                         |
| `xor`           | `XOR`                         |
| `=`             | `EQUAL`                       |
| `/=`            | `NEQUAL`                      |
| `<`             | `LESSTHAN`                    |
| `<=`            | `LESSTHANOREQUAL`             |
| `>`             | `GREATERTHAN`                 |
| `>=`            | `GREATERTHANOREQUAL`          |
| `+`             | `PLUS`                        |
| `-`             | `MINUS`                       |
| `&`             | `CONCAT`                      |
| `*`             | `MULT`                        |
| `/`             | `DIV`                         |
| `mod`           | `MOD`                         |
| `rem`           | `REM`                         |
| `**`            | `EXP`                         |
| `abs`           | `ABS`                         |
| `not`           | `NOT`                         |
