{-|
   Module      : Parser.Combinators.Lex.Internal
   Description : Internal combinators for the lexical elements of VHDL.
|-}
module Parser.Combinators.Lex.Internal
   ( graphicCharacter
   , basicGraphicCharacter
   , basicCharacter
   , upperCaseLetter
   , specialCharacter
   , spaceCharacter
   , formatEffector
   , lowerCaseLetter
   , otherSpecialCharacter
   ) where

import Control.Applicative
import Text.Parsec.String (Parser)
import Text.Parsec.Char
         ( digit
         , oneOf
         , char
         , space
         )

-- |Parse a graphic character
-- Returns parsed character
-- @
--    graphic_character ::=
--       basic_graphic_character | lower_case_letter | other_special_character
-- @
graphicCharacter :: Parser Char
graphicCharacter =
   basicGraphicCharacter
   <|> lowerCaseLetter
   <|> otherSpecialCharacter

-- |Parse a basic graphic character
-- Returns parsed character
-- @
--    basic_graphic_character ::=
--       upper_case_letter | digit | special_character | space_character
-- @
basicGraphicCharacter :: Parser Char
basicGraphicCharacter =
   upperCaseLetter
   <|> digit
   <|> specialCharacter
   <|> spaceCharacter

-- |Parse a basic character
-- Returns parsed character
-- @
--    basic_character ::=
--       basic_graphic_character | format_effector
-- @
basicCharacter :: Parser Char
basicCharacter =
   basicGraphicCharacter
   <|> formatEffector

-- |Parse an upper case letter
-- Returns parsed character
-- @
--    upper case letters
--       A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
-- @
upperCaseLetter :: Parser Char
upperCaseLetter = oneOf ['A'..'Z']

-- |Parse a special character
-- Returns parsed character
-- @
--    special characters
--       " # & ' ( ) * + , - . / : ; < = > _ |
-- @
specialCharacter :: Parser Char
specialCharacter = oneOf
   [ '"'
   , '#'
   , '&'
   , '\''
   , '('
   , ')'
   , '*'
   , '+'
   , ','
   , '-'
   , '.'
   , '/'
   , ':'
   , ';'
   , '<'
   , '='
   , '>'
   , '_'
   , '|'
   ]

-- |Parse the space character
-- Returns the space character
spaceCharacter :: Parser Char
spaceCharacter = char ' '

-- |Parse a format effector
-- Returns parsed character
formatEffector :: Parser Char
formatEffector = space

-- |Parse a lower case letter
-- Returns parsed character
-- @
--    upper case letters
--       a b c d e f g h i j k l m n o p q r s t u v w x y z
-- @
lowerCaseLetter :: Parser Char
lowerCaseLetter = oneOf ['a'..'z']

-- |Parse an other special character
-- Returns parsed character
-- @
--    other special characters
--       ! $ % @ ? [ \ ] ^ ` { } ~
-- @
otherSpecialCharacter :: Parser Char
otherSpecialCharacter = oneOf
   [ '!'
   , '$'
   , '%'
   , '@'
   , '?'
   , '['
   , '\\'
   , ']'
   , '^'
   , '`'
   , '{'
   , '}'
   , '~'
   ]
