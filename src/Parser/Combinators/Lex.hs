{-|
   Module      : Parser.Combinators.Lex
   Description : Combinators for the lexical elements of VHDL.
|-}
module Parser.Combinators.Lex 
   ( identifier
   , abstractLiteral
   , characterLiteral
   , stringLiteral
   , bitStringLiteral
   ) where

import Control.Applicative
import Numeric (readHex)
import Data.List (delete)
import Data.Int (Int64)
import Text.Parsec
         ( unexpected
         , try
         )
import Text.Parsec.String (Parser)
import Text.Parsec.Char
         ( oneOf
         , digit
         , space
         , char
         , octDigit
         , hexDigit
         , string
         , anyChar
         , endOfLine
         )
import Text.Parsec.Combinator (manyTill)

import Parser.Types.Token
         ( UpperString
         , mkUpperString
         , AbstractLiteral(..)
         , BitString
         , mkBitString
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



-- |Parses an identifier
-- Returns upper case string form of the identifier ('UpperString')
-- @
--    identifier ::=
--       letter { [ underliner ] letter_or_digit }
-- @
identifier :: Parser UpperString
identifier =
   makeIdentifier
   <$> letter
   <*> many
       ( optionalJoin
     <$> optional (char '_')
     <*> letterOrDigit
       )
   where optionalJoin :: Maybe Char -> Char -> String
         optionalJoin (Just _) other = ['_',other]
         optionalJoin Nothing other = [other]
         makeIdentifier :: Char -> [String] -> UpperString
         makeIdentifier fst rest =
            mkUpperString
            $ fst
            : concat rest

-- |Parses a letter or digit
-- Returns parsed character
-- @
--    letter_or_digit ::= letter | digit
-- @
letterOrDigit :: Parser Char
letterOrDigit =
   letter
   <|> digit

-- |Parses a letter
-- Returns parsed character
-- @
--    letter ::= upper_case_letter | lower_case_letter
-- @
letter :: Parser Char
letter =
   upperCaseLetter
   <|> lowerCaseLetter

-- |Parses an abstract literal
-- Returns the parsed 'AbstractLiteral' value
-- @
--    abstract_literal ::= decimal_literal | based_literal
--    decimal_literal ::= integer [ . integer ] [ exponent ]
--    based_literal ::= base # based_integer [ . based_integer ] # [ exponent ]
--    base ::= integer
--    based_integer ::= extended_digit { [ underline ] extended_digit }
--    extended_digit ::= digit | letter
-- @
-- NOTE:
-- - Underline is ignored
-- - Base determines valid extended_digit set
-- - extended_digit is case insensitive
abstractLiteral :: Parser AbstractLiteral
abstractLiteral =
   makeAbstractLiteral =<< integer
   where makeAbstractLiteral :: Integer -> Parser AbstractLiteral
         makeAbstractLiteral fstInt =
            ( basedLiteral fstInt '#'
          <|> basedLiteral fstInt ':'
          <|> decimalLiteral fstInt
            )
         basedLiteral :: Integer -> Char -> Parser AbstractLiteral
         basedLiteral base container =
            makeUniversalValue
            =<< ( (makeBasedLiteral base)
              <$> (char container *> many (basedInteger base))
              <*> optional (char '.' *> many (basedInteger base))
              <*> (char container *> optional exponent')
                )
         basedInteger :: Integer -> Parser Char
         basedInteger 2 = oneOf "01"
         basedInteger 3 = oneOf "012"
         basedInteger 4 = oneOf ['0'..'3']
         basedInteger 5 = oneOf ['0'..'4']
         basedInteger 6 = oneOf ['0'..'5']
         basedInteger 7 = oneOf ['0'..'6']
         basedInteger 8 = octDigit
         basedInteger 9 = oneOf ['0'..'8']
         basedInteger 10 = oneOf ['0'..'9']
         basedInteger 11 = oneOf $ ['0'..'9'] ++ ['a','A']
         basedInteger 12 = oneOf $ ['0'..'9'] ++ ['a','b'] ++ ['A','B']
         basedInteger 13 = oneOf $ ['0'..'9'] ++ ['a'..'c'] ++ ['A'..'C']
         basedInteger 14 = oneOf $ ['0'..'9'] ++ ['a'..'d'] ++ ['A'..'D']
         basedInteger 15 = oneOf $ ['0'..'9'] ++ ['a'..'e'] ++ ['A'..'E']
         basedInteger 16 = hexDigit
         basedInteger int = unexpected $ "based integer for base " ++ show int
         makeBasedLiteral :: Integer -> String -> Maybe String -> Maybe Integer -> Double
         makeBasedLiteral base units maybeDecimals maybeExponent =
            ( (convertBasedValue (fromInteger base) 0 (+1) (reverse units) 0.0)
            + case maybeDecimals of
               Just decimals -> convertBasedValue (fromInteger base) (-1) (\x -> x-1) decimals 0.0
               Nothing -> 0.0
            )
            * case maybeExponent of
               Just exponent -> (fromInteger base) ^^ exponent
               Nothing -> 1.0
         convertBasedValue :: Int -> Int -> (Int -> Int) -> String -> Double -> Double
         convertBasedValue base position modPos (chr:rest) currentVal =
            let baseMultiplier = (fromIntegral base) ^^ position
                hexVal = fst $ head $ readHex [chr]
                newVal = currentVal + (hexVal * baseMultiplier)
            in convertBasedValue base (modPos position) modPos rest newVal
         convertBasedValue _ _ _ [] finalVal = finalVal
         decimalLiteral :: Integer -> Parser AbstractLiteral
         decimalLiteral unitInt =
            makeUniversalValue
            =<< ( (makeDecimalLiteral unitInt)
              <$> optional (char '.' *> many digit)
              <*> optional exponent'
                )
         makeDecimalLiteral :: Integer -> Maybe String -> Maybe Integer -> Double
         makeDecimalLiteral units maybeDecimals maybeExponent =
            ( fromInteger units
            + case maybeDecimals of
               Just decimals -> convertBasedValue 10 (-1) (\x -> x-1) decimals 0.0
               Nothing -> 0.0
            )
            * case maybeExponent of
               Just exponent -> 10 ^^ exponent
               Nothing -> 1.0
         makeUniversalValue :: Double -> Parser AbstractLiteral
         makeUniversalValue value
            | isInfinite value = unexpected "Real (floating point) value out of Double bounds (infinite)"
            | floor value == ceiling value && ((floor value) > (toInteger $ (maxBound :: Int64)) || (floor value) < (toInteger $ (minBound :: Int64))) =
               unexpected "Integer value out of Int64 bounds"
            | floor value == ceiling value = return $ UniversalInteger $ floor value
            | otherwise = return $ UniversalReal value

-- |Parses an integer
-- Returns the 'Integer' value
-- @
--    integer ::= digit { [ underline ] digit }
-- @
integer :: Parser Integer
integer =
   makeInteger
   <$> digit
   <*> many
       ( optional (char '_')
      *> digit
       )
   where makeInteger :: Char -> String -> Integer
         makeInteger chr str = read $ chr : str

-- |Parses an exponent
-- Returns the 'Integer' exponent value
-- @
--    exponent ::= E [ + ] integer | E - integer
-- @
-- NOTE:
-- - Case insensitive E
exponent' :: Parser Integer
exponent' =
   makeExponent
   <$> (oneOf "Ee" *> optional (char '+' <|> char '-'))
   <*> integer
   where makeExponent :: Maybe Char -> Integer -> Integer
         makeExponent (Just '+') int = int
         makeExponent (Just '-') int = - int
         makeExponent Nothing int = int

-- |Parses a character literal
-- Returns the 'Char' literal (without the containing ''' characters)
-- @
--    character_literal ::= ' graphic_character '
-- @
characterLiteral :: Parser Char
characterLiteral =
   char '\''
   *> graphicCharacter
   <* char '\''

-- |Parses a string literal
-- Returns the 'String' literal (without the containing '"' characters)
-- @
--    string_literal ::= " { graphic_character } "
-- @
-- NOTE:
-- - Can use character '%' instead of '"'
-- - To use the container character within the string, must repeat the character
stringLiteral :: Parser String
stringLiteral =
   stringLiteral' '"'
   <|> stringLiteral' '%'
   where stringLiteral' :: Char -> Parser String
         stringLiteral' container =
            char container
            *> some (validGraphicChars container)
            <* char container
         validGraphicChars container =
            (string (replicate 2 container) *> return container)
            <|> upperCaseLetter
            <|> digit
            <|> spaceCharacter
            <|> lowerCaseLetter
            <|> oneOf
                  (delete container
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
                     , '!'
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
                  )

-- |Parses a bit string literal
-- Returns the parsed bit string literal
-- @
--    bit_string_literal ::= base_specifier " bit_value "
--    bit_value ::= extended_digit { [ underline ] extended_digit }
--    base_specifier ::= B | O | X
-- @
-- NOTE:
-- - Base specifier is case insensitive
-- - Can use character '%' instead of '"'
-- - Extended digit depends on base specifier
--
-- If base specifier == B:
-- @
--    extended_digit ::= 0 | 1
-- @
-- If base specifier == O:
-- @
--    extended_digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
-- @
-- If base specifier == X:
-- @
--    extended_digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | A | B | C | D | E | F
-- @
-- NOTE:
-- - Extended digit is case insensitive
bitStringLiteral :: Parser BitString
bitStringLiteral =
   bitStringLiteral' '"'
   <|> bitStringLiteral' '%'
   where bitStringLiteral' :: Char -> Parser BitString
         bitStringLiteral' container =
            binaryString container
            <|> octalString container
            <|> hexString container
         binaryString :: Char -> Parser BitString
         binaryString = baseString (oneOf "Bb") (oneOf "01") pure
         octalString :: Char -> Parser BitString
         octalString = baseString (oneOf "Oo") octDigit octDigitMap
         hexString :: Char -> Parser BitString
         hexString = baseString (oneOf "Xx") hexDigit hexDigitMap
         octDigitMap :: Char -> String
         octDigitMap '0' = "000"
         octDigitMap '1' = "001"
         octDigitMap '2' = "010"
         octDigitMap '3' = "011"
         octDigitMap '4' = "100"
         octDigitMap '5' = "101"
         octDigitMap '6' = "110"
         octDigitMap '7' = "111"
         hexDigitMap :: Char -> String
         hexDigitMap '0' = "0000"
         hexDigitMap '1' = "0001"
         hexDigitMap '2' = "0010"
         hexDigitMap '3' = "0011"
         hexDigitMap '4' = "0100"
         hexDigitMap '5' = "0101"
         hexDigitMap '6' = "0110"
         hexDigitMap '7' = "0111"
         hexDigitMap '8' = "1000"
         hexDigitMap '9' = "1001"
         hexDigitMap chr
            | elem chr "Aa" = "1010"
            | elem chr "Bb" = "0011"
            | elem chr "Cc" = "0100"
            | elem chr "Dd" = "0101"
            | elem chr "Ee" = "0110"
            | elem chr "Ff" = "0111"
         baseString :: Parser Char -> Parser Char -> (Char -> String) -> Char -> Parser BitString
         baseString baseChar extendedDigit digitToBinary container =
            (mkBitString . concat . (map digitToBinary))
            <$> ( baseChar
               *> char container
               *> ( (:)
                <$> extendedDigit
                <*> many (optional (char '_') *> extendedDigit)
                  )
               <* char container
                )

-- |Parses a comment
-- Returns '()'
-- - Comments start with the string "--"
-- - Comments continue to the end of a line
comment :: Parser ()
comment =
   string "--"
   *> manyTill anyChar (try endOfLine)
   *> return ()
