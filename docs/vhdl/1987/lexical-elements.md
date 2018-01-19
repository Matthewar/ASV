# Lexical Elements
There are a number of different lexical elements, which fall into the categories of the basic token.

```haskell
-- |Token Type
data Token = Keyword ReservedWord -- Reserved keywords, case insensitive
           | Operator OperatorType -- Symbolic operators
           | Identifier String -- Non-reserved names, case insensitive
           | Literal LitType -- Literal types (values)
           | EOF -- Used by parser to denote EOF (not a VHDL lexical element)
```

## Keywords
There are a limited number of reserved words (or keywords) that need to be implemented.
Keywords are case insensitive.

### Sanity Tests
There are two tests for each keyword (inputted on their own):
* Completely lower case
* Completely upper case

### Extensive Testing
TODO: Not yet mapped out

### Implementation
| Keyword | Sanity Test | Extensive Test |
| :------ | :---------: | :------------: |
| `Abs`           | [x] | [ ] |
| `Access`        | [x] | [ ] |
| `After`         | [x] | [ ] |
| `Alias`         | [x] | [ ] |
| `All`           | [x] | [ ] |
| `And`           | [x] | [ ] |
| `Architecture`  | [x] | [ ] |
| `Array`         | [x] | [ ] |
| `Assert`        | [x] | [ ] |
| `Attribute`     | [x] | [ ] |
| `Begin`         | [x] | [ ] |
| `Block`         | [x] | [ ] |
| `Body`          | [x] | [ ] |
| `Buffer`        | [x] | [ ] |
| `Bus`           | [x] | [ ] |
| `Case`          | [x] | [ ] |
| `Component`     | [x] | [ ] |
| `Configuration` | [x] | [ ] |
| `Constant`      | [x] | [ ] |
| `Disconnect`    | [x] | [ ] |
| `Downto`        | [x] | [ ] |
| `Else`          | [x] | [ ] |
| `Elsif`         | [x] | [ ] |
| `End`           | [x] | [ ] |
| `Entity`        | [x] | [ ] |
| `Exit`          | [x] | [ ] |
| `File`          | [x] | [ ] |
| `For`           | [x] | [ ] |
| `Function`      | [x] | [ ] |
| `Generate`      | [x] | [ ] |
| `Generic`       | [x] | [ ] |
| `Guarded`       | [x] | [ ] |
| `If`            | [x] | [ ] |
| `In`            | [x] | [ ] |
| `Inout`         | [x] | [ ] |
| `Is`            | [x] | [ ] |
| `Label`         | [x] | [ ] |
| `Library`       | [x] | [ ] |
| `Linkage`       | [x] | [ ] |
| `Loop`          | [x] | [ ] |
| `Map`           | [x] | [ ] |
| `Mod`           | [x] | [ ] |
| `Nand`          | [x] | [ ] |
| `New`           | [x] | [ ] |
| `Next`          | [x] | [ ] |
| `Nor`           | [x] | [ ] |
| `Not`           | [x] | [ ] |
| `Null`          | [x] | [ ] |
| `Of`            | [x] | [ ] |
| `On`            | [x] | [ ] |
| `Open`          | [x] | [ ] |
| `Or`            | [x] | [ ] |
| `Others`        | [x] | [ ] |
| `Out`           | [x] | [ ] |
| `Package`       | [x] | [ ] |
| `Port`          | [x] | [ ] |
| `Procedure`     | [x] | [ ] |
| `Process`       | [x] | [ ] |
| `Range`         | [x] | [ ] |
| `Record`        | [x] | [ ] |
| `Register`      | [x] | [ ] |
| `Rem`           | [x] | [ ] |
| `Report`        | [x] | [ ] |
| `Return`        | [x] | [ ] |
| `Select`        | [x] | [ ] |
| `Severity`      | [x] | [ ] |
| `Signal`        | [x] | [ ] |
| `Subtype`       | [x] | [ ] |
| `Then`          | [x] | [ ] |
| `To`            | [x] | [ ] |
| `Transport`     | [x] | [ ] |
| `Type`          | [x] | [ ] |
| `Units`         | [x] | [ ] |
| `Until`         | [x] | [ ] |
| `Use`           | [x] | [ ] |
| `Variable`      | [x] | [ ] |
| `Wait`          | [x] | [ ] |
| `When`          | [x] | [ ] |
| `While`         | [x] | [ ] |
| `With`          | [x] | [ ] |
| `Xor`           | [x] | [ ] |

## Operator
There are a limited number of operators that need to be implemented.

### Sanity Tests
There is a single test for each operator.
This test consists of the operator on its own in the input string.

### Extensive Testing
TODO: Not yet mapped out

### Implementation
| Operator | Sanity Test | Extensive Test |
| :------- | :---------: | :------------: |
| `=>` | [x] | [ ] |
| `**` | [x] | [ ] |
| `:=` | [x] | [ ] |
| `/=` | [x] | [ ] |
| `>=` | [x] | [ ] |
| `<=` | [x] | [ ] |
| `<>` | [x] | [ ] |
| `&`  | [x] | [ ] |
| `'`  | [x] | [ ] |
| `(`  | [x] | [ ] |
| `)`  | [x] | [ ] |
| `*`  | [x] | [ ] |
| `+`  | [x] | [ ] |
| `,`  | [x] | [ ] |
| `-`  | [x] | [ ] |
| `.`  | [x] | [ ] |
| `/`  | [x] | [ ] |
| `:`  | [x] | [ ] |
| `;`  | [x] | [ ] |
| `<`  | [x] | [ ] |
| `=`  | [x] | [ ] |
| `>`  | [x] | [ ] |
| `|`  | [x] | [ ] |

## Identifier
Identifiers are a combination of letters and numbers that represent names.
An identifier matches the regex pattern `[a-zA-Z](_?[a-zA-Z0-9])*`.

### Sanity Tests
There is a randomised test for sanity.
It creates a random valid identifier within some length range.
This random identifier is input alone into the lexer, 100 times.

### Extensive Testing
TODO: Not yet mapped out

### Implementation
* Sanity Test: [x]
* Extensive Test: [ ]

## Literals
There are a number of different literals that exist in VHDL:
* Based value
* Decimal value
* Bit string
* String
* Character

These are implemented as:
```haskell
data LitType = Univ_Int Int64 -- Universal integer
             | Univ_Real Double -- Universal floating point value
             | BitStr LiteralBase ByteString -- Bit string literal
             | Str String -- String literal
             | Character Char -- Character literal
```
Note that the `Universal` values are converted from the actual literal types:
* Based value
* Decimal value

### Sanity Tests
There are randomised tests for several different cases.
These tests consist of the randomly generated literals on their own as an input.
Each test case is run 100 times with different inputs.

Each random generator test is as follows:
* Decimal tests
    * Random:
        * Integer values without an exponent: `abs :: Int`
        * Real values without an exponent: `abs :: Double`
        * Integer values with an exponent: `[0-9](_?[0-9])*[eE][+-]?[0-9](_?[0-9])*`
        * Real values with an exponent: `[0-9](_?[0-9])*.[0-9](_?[0-9])*[eE][+-]?[0-9](_?[0-9])*`
        * Integer zero with an exponent: `0[Ee][+-]?[0-9]+`
        * Real zero with an exponent: `0.0[Ee][+-]?[0-9]+`
    * Unit Tests:
        * Integer zero without an exponent: `0`
        * Real zero without an exponent: `0.0`
* Bit string tests:
    * Random:
        * With " containers: `([Bb]"[01](_?[01])*"|[Oo]"[0-7](_?[0-7])*"|[Xx]"[0-9a-fA-F](_?[0-9a-fA-F])*")`
        * With % containers: `([Bb]%[01](_?[01])*%|[Oo]%[0-7](_?[0-7])*%|[Xx]%[0-9a-fA-F](_?[0-9a-fA-F])*%)`
* String tests
    * Random:
        * Random length string of all accepted characters, with " containers
        * Random length string of all accepted characters, with % containers
    * Unit Tests:
        * Empty string with " containers: `""`
        * Empty string with % containers: `%%`
* Character tests:
    * Random:
        * Any random character with ' containers
* Identifier tests:
    * Random:
        * General form: `[a-zA-Z][a-zA-Z0-9_]*`
* Based literal tests:
    * TODO: Not yet implemented

### Extensive Testing
TODO: Not yet mapped out

### Implementation
| Literal Type | Sanity Test | Extensive Test |
| :----------- | :---------: | :------------: |
| Based        | [ ] | [ ] |
| Decimal      | [x] | [ ] |
| Bit string   | [x] | [ ] |
| String       | [x] | [ ] |
| Character    | [x] | [ ] |
