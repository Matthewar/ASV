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
   , comment
   ) where

import Control.Applicative
import Numeric (readInt)
import qualified Data.Map.Strict as MapS
import Data.List (delete)
import Data.Int (Int64)
import Data.Char (toUpper)
import Text.Parsec
         ( unexpected
         , try
         )
import Text.Parsec.String (Parser)
import Text.Parsec.Char
         ( oneOf
         , digit
         , char
         , octDigit
         , hexDigit
         , string
         , anyChar
         , endOfLine
         )
import Text.Parsec.Combinator
         ( manyTill
         , eof
         )
import Text.Parsec.Prim
         ( unexpected
         , (<?>)
         )

import Parser.Types.Token
         ( UpperString
         , mkUpperString
         , AbstractLiteral(..)
         , BitString
         , mkBitString
         )
import Parser.Combinators.Lex.Internal

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
   where makeAbstractLiteral :: String -> Parser AbstractLiteral
         makeAbstractLiteral fstInt =
            ( basedLiteral fstInt '#'
          <|> basedLiteral fstInt ':'
          <|> decimalLiteral fstInt
            )
         basedLiteral :: String -> Char -> Parser AbstractLiteral
         basedLiteral baseStr container =
            let base = read baseStr
            in (makeBasedLiteral $ fromInteger base)
               =<< (\a b c -> (a,b,c))
               <$> (char container *> basedInteger base)
               <*> optional (char '.' *> basedInteger base)
               <*> (char container *> optional exponent')
         basedInteger :: Integer -> Parser String
         basedInteger base =
            (:)
            <$> extendedDigit base
            <*> many (optional (char '_') *> extendedDigit base)
         digitMap :: Integer -> [Char]
         digitMap = (MapS.!) $ MapS.fromList
            [ (2,"01")
            , (3,"012")
            , (4,['0'..'3'])
            , (5,['0'..'4'])
            , (6,['0'..'5'])
            , (7,['0'..'6'])
            , (8,['0'..'7'])
            , (9,['0'..'8'])
            , (10,['0'..'9'])
            , (11,['0'..'9'] ++ ['a','A'])
            , (12,['0'..'9'] ++ ['a','b'] ++ ['A','B'])
            , (13,['0'..'9'] ++ ['a'..'c'] ++ ['A'..'C'])
            , (14,['0'..'9'] ++ ['a'..'d'] ++ ['A'..'D'])
            , (15,['0'..'9'] ++ ['a'..'e'] ++ ['A'..'E'])
            , (16,['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])
            ]
         charConvert :: Char -> Int
         charConvert '0' = 0
         charConvert '1' = 1
         charConvert '2' = 2
         charConvert '3' = 3
         charConvert '4' = 4
         charConvert '5' = 5
         charConvert '6' = 6
         charConvert '7' = 7
         charConvert '8' = 8
         charConvert '9' = 9
         charConvert 'A' = 10
         charConvert 'B' = 11
         charConvert 'C' = 12
         charConvert 'D' = 13
         charConvert 'E' = 14
         charConvert 'F' = 15
         extendedDigit :: Integer -> Parser Char
         extendedDigit int | int < 2 || int > 16 = unexpected ("based integer for base " ++ show int)
                                                   <?> "base between 2 and 16"
         extendedDigit int = toUpper <$> oneOf (digitMap int)
         makeBasedLiteral :: Integer -> (String,Maybe String,Maybe Integer) -> Parser AbstractLiteral
         makeBasedLiteral base = let validChars chr = elem chr $ digitMap base
                                     readFunc str = case (readInt (fromInteger base) validChars charConvert) str of [(val,"")] -> val
                                     makeReal units decimals exponent = makeUniversalReal $
                                                                        (*)
                                                                        (readFunc $ units ++ decimals)
                                                                        (fromInteger base ^^ (fromInteger exponent - length decimals))
                                     makeLiteral (units,Just decimals,Nothing) = makeReal units decimals 0
                                     makeLiteral (units,Just decimals,Just exponent) = makeReal units decimals exponent
                                     makeLiteral (units,Nothing,exponent) = makeUniversalIntegerFromBase readFunc units exponent
                                 in makeLiteral
         decimalLiteral :: String -> Parser AbstractLiteral
         decimalLiteral units =
            (makeDecimalLiteral units)
            =<< (\a b -> (a,b))
            <$> optional (char '.' *> integer)
            <*> optional exponent'
         makeDecimalLiteral :: String -> (Maybe String,Maybe Integer) -> Parser AbstractLiteral
         makeDecimalLiteral units (Just decimals,Just 0) = makeUniversalReal $ read $ units ++ "." ++ decimals
         makeDecimalLiteral units (Just decimals,Nothing) = makeUniversalReal $ read $ units ++ "." ++ decimals
         makeDecimalLiteral units (Just decimals,Just exponent) = makeUniversalReal $ read $ units ++ "." ++ decimals ++ "e" ++ show exponent
         makeDecimalLiteral units (Nothing,exponent) = makeUniversalIntegerFromBase read units exponent
         makeUniversalIntegerFromBase :: (String -> Integer) -> String -> Maybe Integer -> Parser AbstractLiteral
         makeUniversalIntegerFromBase readFunc units (Just exponent)
            | exponent >= 0 = makeUniversalInteger $ readFunc $ units ++ replicate (fromInteger exponent) '0'
            | otherwise = let shiftValue 0 finished = makeUniversalInteger $ readFunc $ reverse finished
                              shiftValue shift ('0':rest) = shiftValue (shift-1) rest
                              shiftValue shift valueStr = unexpected ( "floating point value "
                                                                     ++ ( (\(a,b) -> reverse b ++ reverse a)
                                                                        $ splitAt shift
                                                                        $ valueStr
                                                                        ++ if shift >= length valueStr
                                                                              then replicate (shift + 1 - length valueStr) '0'
                                                                              else ""
                                                                        )
                                                                     )
                                                          <?> "integer value"
                          in shiftValue (- (fromInteger exponent)) $ reverse units
         makeUniversalIntegerFromBase readFunc units Nothing = makeUniversalInteger $ readFunc units
         makeUniversalInteger :: Integer -> Parser AbstractLiteral
         makeUniversalInteger val
            | val > toInteger (maxBound :: Int64) = unexpected "integer value out of Int64 bounds"
                                                    <?> "universal integer value"
            | otherwise = return $ UniversalInteger $ fromInteger val
         makeUniversalReal :: Double -> Parser AbstractLiteral
         makeUniversalReal val
            | isInfinite val = unexpected "real (floating point) value out of Double bounds (infinite)"
                               <?> "universal real value"
            | otherwise = return $ UniversalReal val

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
            *> some (try $ validGraphicChars container)
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
   try (bitStringLiteral' '"')
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
            | elem chr "Bb" = "1011"
            | elem chr "Cc" = "1100"
            | elem chr "Dd" = "1101"
            | elem chr "Ee" = "1110"
            | elem chr "Ff" = "1111"
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
   try (string "--")
   *> manyTill anyChar ((endOfLine *> return ()) <|> eof)
   *> return ()
