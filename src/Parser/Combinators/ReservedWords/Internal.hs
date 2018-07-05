{-|
   Module      : Parser.Combinators.ReservedWords.Internal
   Description : Internal combinators for the "reserved word" lexical elements of VHDL.
|-}
module Parser.Combinators.ReservedWords.Internal
   ( caseInsensitiveChar
   , keywordParser
   ) where

import Control.Applicative
import Data.Char
         ( toUpper
         , toLower
         )
import Text.Parsec (notFollowedBy)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
         ( char
         , oneOf
         )

-- |Parse a character, ignoring case
-- Parses 'Char' input (upper or lower case)
-- Returns upper case character parsed
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar chr =
   char (toUpper chr)
   <|> (char (toLower chr) *> return (toUpper chr))

-- |Parses a keyword, ignoring case
-- Parses 'String' input (upper or lower case)
-- Returns '()'
keywordParser :: String -> Parser ()
keywordParser keyword = mapM caseInsensitiveChar keyword *> (notFollowedBy $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
