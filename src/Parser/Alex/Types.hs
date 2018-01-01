-- Adapted from alex default wrapper file
module Parser.Alex.Types where

import Data.Word (Word8)
import Control.Applicative as App (Applicative (..))
import Data.Int (Int64)
import Data.Function ((&))

import Parser.TokenTypes

type Byte = Word8

-- | Input type
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

-- | Token positions
-- Address (absolute position), line number, column number
data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

-- | Current state of lexer
data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode
    }

newtype Alex a = Alex { unAlex :: AlexState -> Either LexerError (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left err -> Left err
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left err -> Left err
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left err -> Left err
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

-- | Type for alex actions
type AlexAction result = AlexInput -> Int -> Alex result

-- |Errors that the lexer can output
-- All errors contain the position where the error occurred.
data LexerError
   -- |Used when an unknown error has occurred in the lexer
   = GenericLexError AlexPosn
   -- |Based literal lexer error, invalid base value
   -- Contains base value, entire literal, position
   | LexErr_BasedLiteral_InvalidBaseValue Int String AlexPosn
   -- |Based literal lexer error, invalid base chararacter
   -- Contains base character, entire literal, position
   | LexErr_BasedLiteral_InvalidBaseChar Char String AlexPosn
   -- |Based literal lexer error, invalid based value
   -- This could be incorrect underscore formatting, or invalid characters found, or both
   -- Contains entire literal, position
   | LexErr_BasedLiteral_InvalidValue String AlexPosn
-- ??   | LexErr_BasedLiteral_Generic String AlexPosn
   -- |Decimal literal lexer error, incorrect formatting of decimal
   -- This could be empty section e.g. ".3" instead of "0.3"
   -- Also includes invalid underscore formatting
   -- Contains entire literal, position
-- ?? Should include incorrect characters and wrong exponent characters?
   | LexErr_DecimalLiteral_InvalidFormat String AlexPosn
   -- |Universal integer type lexer error, out of bounds value
   -- This occurs when a number is larger than the Haskell Int64 type
   -- Contains entire literal (based or decimal), position
   | LexErr_UniversalInt_OutOfBounds String AlexPosn
   -- |Universal real type lexer error, out of bounds value
   -- This occurs when a number is larger than the Haskell Double type
   -- Contains entire literal (based or decimal), position
   | LexErr_UniversalReal_OutOfBounds String AlexPosn
   -- |Bit string literal lexer error, invalid string
   -- This could be incorrect underscore formatting, invalid characters found, or both
   -- Contains entire literal, position
   | LexErr_BitStrLiteral_InvalidStr String AlexPosn
   -- |Bit string literal lexer error, invalid base character
   -- Contains base character, entire literal, position
   | LexErr_BitStrLiteral_InvalidBase Char String AlexPosn
   -- |Bit string literal lexer error, empty string
   -- Contains entire literal, position
   | LexErr_BitStrLiteral_EmptyStr String AlexPosn
   deriving (Eq)
-- ?? NonMatchingIdentifierError ReservedWord String String AlexPosn

instance (Show LexerError) where
   show (GenericLexError pos) =
      "Some lexer error occurred "
      ++ getLineAndColErrStr pos
   show (LexErr_BasedLiteral_InvalidBaseValue base str pos) =
      "Lexer found invalid base value "
      ++ (show base)
      ++ " in based literal "
      ++ str
      ++ (getLineAndColErrStr pos)
   show (LexErr_BasedLiteral_InvalidBaseChar base str pos) =
      "Lexer found invalid base character "
      ++ [base]
      ++ " in based literal "
      ++ str
      ++ (getLineAndColErrStr pos)
   show (LexErr_BasedLiteral_InvalidValue str pos) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing based literal \""
      ++ str
      ++ "\""
      ++ (getLineAndColErrStr pos)
--   show (LexErr_BasedLiteral_Generic str pos) =
--      "Some lexer error occurred lexing bit string "
--      ++ str
--      ++ (getLineAndColErrStr pos)
   show (LexErr_DecimalLiteral_InvalidFormat str pos) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing decimal literal \""
      ++ str
      ++ "\""
      ++ (getLineAndColErrStr pos)
   show (LexErr_UniversalInt_OutOfBounds str pos) =
      "Lexer found out of bounds integer value "
      ++ str
      ++ (getLineAndColErrStr pos)
      ++ " note that integer range is "
      ++ (show $ (minBound :: Int64))
      ++ " to "
      ++ (show $ (maxBound :: Int64))
   show (LexErr_UniversalReal_OutOfBounds str pos) =
      let maxVal = getFloatBound (0.0 :: Double)
      in "Lexer found out of bounds real value "
         ++ str
         ++ (getLineAndColErrStr pos)
         ++ " note that integer range is "
         ++ maxVal
         ++ " to -"
         ++ maxVal
   show (LexErr_BitStrLiteral_InvalidStr str pos) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing bit string literal \""
      ++ str
      ++ "\""
      ++ (getLineAndColErrStr pos)
   show (LexErr_BitStrLiteral_InvalidBase base str pos) =
      "Lexer found invalid base character "
      ++ [base]
      ++ " in bit string literal "
      ++ str
      ++ (getLineAndColErrStr pos)
   show (LexErr_BitStrLiteral_EmptyStr str pos) =
      "Lexer found empty bit string "
      ++ str
      ++ (getLineAndColErrStr pos)

-- | Get the string containing the line and column string
getLineAndColErrStr :: AlexPosn -> String
getLineAndColErrStr (AlexPn _ line column) =
   " at line "
   ++ (show line)
   ++ ", column "
   ++ (show column)

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
