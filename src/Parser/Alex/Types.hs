-- Adapted from alex default wrapper file
module Parser.Alex.Types where

import Data.Word (Word8)
import Control.Applicative as App (Applicative (..))

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

newtype Alex a = Alex { unAlex :: AlexState -> Either ParserError (AlexState, a) }

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

data ParserError = GenericLexError AlexPosn
                 | GenericBasedLiteralError String AlexPosn
                 | InvalidBaseBasedLiteralError Double String AlexPosn
                 | NonMatchingIdentifierError ReservedWord String String AlexPosn
                 deriving (Eq)

instance (Show ParserError) where
   show (GenericLexError pos) =
      "Some lexer error occurred at line "
      ++ getLineAndColErrStr pos
   show (GenericBasedLiteralError str pos) =
      "Some lexer error occurred lexing bit string "
      ++ str
      ++ (getLineAndColErrStr pos)
   show (InvalidBaseBasedLiteralError base str pos) =
      "Lexer found invalid base "
      ++ (show base)
      ++ " in literal "
      ++ str
      ++ (getLineAndColErrStr pos)

-- | Get the string containing the line and column string
getLineAndColErrStr :: AlexPosn -> String
getLineAndColErrStr (AlexPn _ line column) =
   " at line "
   ++ (show line)
   ++ ", column "
   ++ (show column)
