# Syntax Elements
The specification of the language includes a large number of statements which can appear in files.
It is unlikely they will all be implemented initially, instead a subset will be prioritised.

**NOTE**: this document specifies "Implemented" as whether the AST element can be parsed by the Netlist Converter.

## Abstract Syntax Tree
The abstract syntax tree is complex and recursive.
The structure of the tree can be understood from the [node types file](../../../src/Parser/Happy/Types.hs).

## Expressions
Expressions require special processing, to resolve types along with keeping the intent behind the ordering of _short-circuit_ operations.
These will be implemented completely because they are required for most interactions between signals and variables.

### Expression Components
| Expression | Implemented | Sanity Test | Extensive Test |
| :--------- | :---------: | :---------: | :------------: |
| Expression_And  | [ ] | [ ] | [ ] |
| Expression_Or   | [ ] | [ ] | [ ] |
| Expression_Xor  | [ ] | [ ] | [ ] |
| Expression_Nand | [ ] | [ ] | [ ] |
| Expression_Nor  | [ ] | [ ] | [ ] |
| Relation        | [ ] | [ ] | [ ] |

## Statements
The follow section shows the planned implemented statements of the language.
If a statement is not implemented this means any time it appears in the AST a "Non-Implemented" error should be thrown.

### Prioritised
| Statement | Justification | Implemented | Sanity Test | Extensive Test |
| :-------- | :------------ | :---------: | :---------: | :------------: |

### Extensions
| Statement | Justification |
| :-------- | :------------ |

## Attributes
Attributes are qualities of variables and signals that show details about the underlying type and the state of the signal.
Some are required for this task, a major example of this is _EVENT_, which is necessary for clocked signals.

### Prioritised
| Attribute | Justification | Implemented | Sanity Test | Extensive Test |
| :-------- | :------------ | :---------: | :---------: | :------------: |
### Extensions
| Attribute | Justification |
| :-------- | :------------ |
