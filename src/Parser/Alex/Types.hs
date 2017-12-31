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

-- | Errors associated with literal lexer
data LiteralLexErrorType = BasedLexError BasedLexErrorType
                         | DecimalLexError DecimalLexErrorType
                         | UniversalLexError UnivLexErrorType
                         deriving (Eq)

-- | Errors associated with based literals in the lexer
data BasedLexErrorType = InvalidBase Int String
--                       | InvalidValue
                       | GenericBasedLiteralError String
                       deriving (Eq)

-- | Errors associated with decimal literals in the lexer
data DecimalLexErrorType = InvalidDecimalFormat String
                         deriving (Eq)

-- | Errors associated with literals interpreted as universal type in the lexer
data UnivLexErrorType = OutOfBoundsInt String
                      | OutOfBoundsReal String
                      deriving (Eq)

-- | Errors that the lexer can output
data LexerError = GenericLexError AlexPosn
                 | LiteralLexError LiteralLexErrorType AlexPosn
--                 | NonMatchingIdentifierError ReservedWord String String AlexPosn
                 deriving (Eq)

instance (Show LexerError) where
   show (GenericLexError pos) =
      "Some lexer error occurred "
      ++ getLineAndColErrStr pos
   show (LiteralLexError (BasedLexError (GenericBasedLiteralError str)) pos) =
      "Some lexer error occurred lexing bit string "
      ++ str
      ++ (getLineAndColErrStr pos)
   show (LiteralLexError (BasedLexError (InvalidBase base str)) pos) =
      "Lexer found invalid base "
      ++ (show base)
      ++ " in literal "
      ++ str
      ++ (getLineAndColErrStr pos)
   show (LiteralLexError (UniversalLexError (OutOfBoundsInt valStr)) pos) =
      "Lexer found out of bounds integer value "
      ++ valStr
      ++ (getLineAndColErrStr pos)
      ++ " note that integer range is "
      ++ (show $ (minBound :: Int64))
      ++ " to "
      ++ (show $ (maxBound :: Int64))
   show (LiteralLexError (UniversalLexError (OutOfBoundsReal valStr)) pos) =
      let maxVal = getFloatBound (0.0 :: Double)
      in "Lexer found out of bounds real value "
         ++ valStr
         ++ (getLineAndColErrStr pos)
         ++ " note that integer range is "
         ++ maxVal
         ++ " to -"
         ++ maxVal

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
