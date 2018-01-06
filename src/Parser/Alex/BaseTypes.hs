-- Adapted from Alex default wrapper file
module Parser.Alex.BaseTypes where

import Data.Word (Word8)

type Byte = Word8

-- |Input type
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

-- |Token positions
-- Address (absolute position), line number, column number
data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

-- |Current state of lexer
data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int        -- the current startcode
    }
