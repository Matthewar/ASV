module Parser.ErrorTypes
( 
   ParserError(..)
   , WrappedParserError
   , printParserError
)
where

import Data.Char as Char
import Data.Int (Int64)
import Data.Function ((&))

import Parser.Alex.BaseTypes (AlexPosn(..))
import Parser.PositionWrapper

-- |Errors that the parser and lexer can output
-- All errors contain the position where the error occurred.
data ParserError
   -- |Used when an unknown error has occurred in the lexer
   = GenericLexError
   -- |Based literal lexer error, invalid base value
   -- Contains base value, entire literal
   | LexErr_BasedLiteral_InvalidBaseValue Int String
   -- |Based literal lexer error, invalid base chararacter
   -- Contains base character, entire literal
   | LexErr_BasedLiteral_InvalidBaseChar Char String
   -- |Based literal lexer error, invalid based value
   -- This could be incorrect underscore formatting, or invalid characters found, or both
   -- Contains entire literal
   | LexErr_BasedLiteral_InvalidValue String
-- ??   | LexErr_BasedLiteral_Generic String
   -- |Decimal literal lexer error, incorrect formatting of decimal
   -- This could be empty section e.g. ".3" instead of "0.3"
   -- Also includes invalid underscore formatting
   -- Contains entire literal
-- ?? Should include incorrect characters and wrong exponent characters?
   | LexErr_DecimalLiteral_InvalidFormat String
   -- |Universal integer type lexer error, out of bounds value
   -- This occurs when a number is larger than the Haskell Int64 type
   -- Contains entire literal (based or decimal)
   | LexErr_UniversalInt_OutOfBounds String
   -- |Universal real type lexer error, out of bounds value
   -- This occurs when a number is larger than the Haskell Double type
   -- Contains entire literal (based or decimal)
   | LexErr_UniversalReal_OutOfBounds String
   -- |Bit string literal lexer error, invalid string
   -- This could be incorrect underscore formatting, invalid characters found, or both
   -- Contains entire literal
   | LexErr_BitStrLiteral_InvalidStr String
   -- |Bit string literal lexer error, invalid base character
   -- Contains base character, entire literal
   | LexErr_BitStrLiteral_InvalidBase Char String
   -- |Bit string literal lexer error, empty string
   -- Contains entire literal
   | LexErr_BitStrLiteral_EmptyStr String
   -- |Used when an unknown error has occurred in the parser
   | GenericParseError
   deriving (Eq)
-- ?? NonMatchingIdentifierError ReservedWord String String

instance (Show ParserError) where
   show (GenericLexError) =
      "Some lexer error occurred "
   show (LexErr_BasedLiteral_InvalidBaseValue base str) =
      "Lexer found invalid base value "
      ++ (show base)
      ++ " in based literal "
      ++ str
   show (LexErr_BasedLiteral_InvalidBaseChar base str) =
      "Lexer found invalid base character "
      ++ [base]
      ++ " in based literal "
      ++ str
   show (LexErr_BasedLiteral_InvalidValue str) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing based literal \""
      ++ str
      ++ "\""
--   show (LexErr_BasedLiteral_Generic str) =
--      "Some lexer error occurred lexing bit string "
--      ++ str
   show (LexErr_DecimalLiteral_InvalidFormat str) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing decimal literal \""
      ++ str
      ++ "\""
   show (LexErr_UniversalInt_OutOfBounds str) =
      "Lexer found out of bounds integer value "
      ++ str
      ++ " note that integer range is "
      ++ (show $ (minBound :: Int64))
      ++ " to "
      ++ (show $ (maxBound :: Int64))
   show (LexErr_UniversalReal_OutOfBounds str) =
      let maxVal = getFloatBound (0.0 :: Double)
      in "Lexer found out of bounds real value "
         ++ str
         ++ " note that integer range is "
         ++ maxVal
         ++ " to -"
         ++ maxVal
   show (LexErr_BitStrLiteral_InvalidStr str) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing bit string literal \""
      ++ str
      ++ "\""
   show (LexErr_BitStrLiteral_InvalidBase base str) =
      "Lexer found invalid base character "
      ++ [base]
      ++ " in bit string literal "
      ++ str
   show (LexErr_BitStrLiteral_EmptyStr str) =
      "Lexer found empty bit string "
      ++ str
   --show (GenericParseError) =
   --   "Some parser error occurred "
   show (GenericParseError) =
      "Some parser error occurred"

-- | Find largest (and by extension smallest) possible value of double
getFloatBound :: RealFloat a => a -> String
getFloatBound val =
   let radix = floatRadix val
       maxExp = snd $ floatRange val
       numBitsSig = floatDigits val
       mantissa = (radix ^ numBitsSig) - 1
       exp = maxExp - numBitsSig
   in encodeFloat mantissa exp
      & show

type WrappedParserError = PosnWrapper ParserError

-- |Print parser error
-- Extracts position of error and error type to produce message.
printParserError err =
   let position = getPos err
       contents = unPos err
   in show contents ++ getLineAndColErrStr position

-- | Get the string containing the line and column string
getLineAndColErrStr :: AlexPosn -> String
getLineAndColErrStr (AlexPn _ line column) =
   " at line "
   ++ (show line)
   ++ ", column "
   ++ (show column)
