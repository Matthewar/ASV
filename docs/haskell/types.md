# Haskell Type Implementation
There are a number of VHDL types that needed to be stored in Haskell.

| VHDL Type | Description | Haskell Type |
| --------- | ----------- | ------------ |
| Universal Integer | Any integral value e.g. integer, natural | `Int64` |
| Universal Real | Any floating point value e.g. real | `Double` |
| Bit String Literal | A string of bits e.g. std\_logic\_vector | `ByteString` and enumerate for base |
| String | A string (contained in " or %) | `String` |
| Character | A character (only certain ones are supported in VHDL) | `Char` |

Excerpt from [token types file](/src/Parser/TokenTypes.hs):
```
-- |Literal token types
data LitType =
   -- |Universal Integer
   -- Uses 'Int64' so bounded by 64 bits
   Univ_Int Int64
   -- |Universal Real
   -- Uses 'Double' so bounded by implementation of this
   | Univ_Real Double
   -- |Bit string literal
   | BitStr LiteralBase ByteString
   -- |String literal
   | Str String
   -- |Character literal
   | Character Char
   deriving (Eq,Show)
```
