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

According to [1076-1987](https://ieeexplore.ieee.org/document/26487/):
- Any abstract literal containing a decimal point is a `Universal_Real` value
- Otherwise the literal is a `Universal_Integer`
- Exponent doesn't affect how the value is parsed
