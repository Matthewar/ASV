# Haskell Type Implementation
There are a number of VHDL types that needed to be stored in Haskell.

| VHDL Type | Description | Haskell Type |
| --------- | ----------- | ------------ |
| Universal Integer | Any integral value e.g. integer, natural | `Int64` |
| Universal Real | Any floating point value e.g. real | `Double` |
| Bit String Literal | A string of bits e.g. std\_logic\_vector | `BitString` |
| String | A string (contained in " or %) | `String` |
| Character | A character (only certain ones are supported in VHDL) | `Char` |

## Implementation Details
- Bit string only contains characters '0' and '1'
   - Contains Haskell `ByteString.Lazy.Char8` type
   - See `BitString` type in [token internal types file](../src/Parser/Types/Token/Internal.hs) and constructor in [token types file](../src/Parser/Types/Token.hs)
- Universal integer and real values are contained in the type `AbstractLiteral` in the [token types file](../src/Parser/Types/Token.hs)

## Reading an abstract literal
See [lexical elements parsing file](../src/Parser/Combinators/Lex.hs).

When parsing an abstract value:
- Units are parsed as a string
- Decimals are parsed a string
- Exponent is parsed as an integer

To attempt to get maximum precision, the parser carries out pre-shifting of the units and decimals using the exponent before converting the components to a value:
- Pre-shifting
   - If exponent positive:
      - Shifts decimals into units and adds trailing zeroes if necessary
   - If exponent negative:
      - Shifts any units into decimals, does not add following zeroes
      - Calculates new negative exponent
- If exponent and decimal components are `1.0` and `""` respectively
   - Converts value to integer
- Otherwise:
   - Combines units and decimals
   - Converts combined string to `Double`
   - Evaluates exponent value (base ^ exponent)
   - Adds values
**NOTE**: Currently only occurs for decimal literals (base 10)

This means that only values of +/- 2^53 are guaranteed to parse to an integer
- 53 bits is the size of the `Double` mantissa
