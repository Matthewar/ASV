{-|
   Module      : Spec.Generators.LexElements
   Description : Generators of basic lexical elements of VHDL
-}
module Spec.Generators.LexElements 
   --( GenPair(..)
   --, combineGen
   ( genInteger
   , genExponent
   --, genBasedStr
   , genBitStr
   , genIdentifier
   --, genDecimal
   , genKeyword
   , genCaseInsensitiveWord
   --, genDelimiter
   , genComment
   ) where

import qualified Test.Tasty.QuickCheck as QC

import Control.Monad
import Data.Function ((&))
import qualified Data.Map.Strict as MapS
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
         ( toUpper
         , toLower
         )
import Data.Functor ((<&>))

import Types
         ( ExpectedOutput(..)
         , ParserExpectedOutput
         )

import Parser.Types.Token
         ( BitString
         , mkBitString
         )
--import Lexer.Types.Token
--         ( Token(..)
--         , LiteralBase(..)
--         , LitType(..)
--         , OperatorType(..)
--         )
--import Manager.Types.Error (ConverterError)

---- |Pair of lexer input and expected token
--data GenPair a = GenPair String a
--instance Show a => Show (GenPair a) where
--   show (GenPair str expected) =
--      "Input: \"" ++ str ++ "\"\n"
--      ++ "Expected Output: " ++ show expected
--
--combineGen :: (a -> b -> c) -> GenPair a -> GenPair b -> GenPair c
--combineGen combineFunc (GenPair lexInput1 list1) (GenPair lexInput2 list2) =
--   GenPair (lexInput1 ++ lexInput2) (combineFunc list1 list2)
--
-- |Generate a VHDL specific integer
-- Two arguments: minimum and maximum value
--
-- > integer ::= digit { [ underline ] digit }
-- Implemented as:
-- @
--    'genInteger' ::= digit { 'genUnderscoreDigit' }
--    'genUnderscoreDigit' ::= [ underline ] digit
-- @
genInteger :: Integer -> Integer -> QC.Gen (ParserExpectedOutput Integer)
genInteger minimum maximum =
   let genUnderscore char = QC.arbitrary
                            <&> \includeUnderscore ->
                                 if includeUnderscore
                                    then ['_',char]
                                    else [char]
       addUnderscores (fst:rest) = ([fst]:) <$> mapM genUnderscore rest
       addUnderscores [] = return []
       input output = concat <$> (addUnderscores $ show $ output)
       output val = input val <&> \input -> ExpectedOutput input val
   in output =<< QC.choose (minimum,maximum)

-- |Generate a VHDL specific exponent
-- > exponent ::= E [ + ] integer  | E - integer
-- Note: case insensitive \\'E\\'
-- Implemented as:
-- @
--    'genExponent' ::= exponent_marker [ exponent_sign ] 'genInteger'
--    exponent_marker ::= E | e
--    exponent_sign ::= + | -
-- @
genExponent :: QC.Gen (ParserExpectedOutput Integer)
genExponent = do
   expChar <- QC.elements "Ee"
   expSign <- QC.elements ["+","-",""]
   (ExpectedOutput expStr expVal) <- genInteger 0 200
   let expectedOutput = expVal & case expSign of
                                    "-" -> (-) 0
                                    _ -> id
       input = (expChar:expSign) ++ expStr
   return $ ExpectedOutput input expectedOutput

---- |Generate a VHDL specific based literal
---- @
----    based_literal ::=
----       base # based_integer [ . based_integer ] # [ exponent ]
----    base ::= integer
----    based_integer ::=
----       extended_digit { [ underline ] extended_digit }
----    extended_digit ::= digit | letter
---- @
---- Note: \\'#\\' container can instead be \\':\\'
---- Implemented as:
---- @
----    'genBasedStr' ::=
----       base based_container 'genExtendedBasedStr' based_container [ 'genExponent' ]
----    base ::= HASKELL( [2..16] )
----    based_container ::= # | :
----    'genExtendedBasedStr' ::= based_integer [ . based_integer ]
----    based_integer ::= REGEX( [0-9a-fA-F] )
---- @
---- Note: based_container must be same on both sides
----       Relevant base restricts range of regex (only full range for hex/base16)
--genBasedStr :: Char -> QC.Gen String
--genBasedStr container = do
--   base <- QC.elements [2..16]
--   let baseChars = show base
--       basedStrGen = genExtendedBasedStr base
--   optionalFractional <- QC.elements [True,False]
--   expStr <- genExponent
--   unitBasedStr <- basedStrGen
--   fullBasedStr <-
--         if optionalFractional then do
--            decBasedStr <- basedStrGen
--            return $ unitBasedStr ++ "." ++ decBasedStr
--         else
--            return unitBasedStr
--   return $ baseChars ++ [container] ++ fullBasedStr ++ [container] ++ expStr
--   where genExtendedBasedStr :: Int -> QC.Gen String
--         genExtendedBasedStr base = do
--            let allowedChars = charMap !! (base-2)
--            firstChar <- QC.elements allowedChars
--            lengthStr <- QC.elements [0..25]
--            otherChars <- replicateM lengthStr $ genUnderscoreExtendedChar allowedChars
--            return $ [firstChar] ++ (concat otherChars)
--         charMap =
--            [ "01"
--            , ['0'..'2']
--            , ['0'..'3']
--            , ['0'..'4']
--            , ['0'..'5']
--            , ['0'..'6']
--            , ['0'..'7']
--            , ['0'..'8']
--            , ['0'..'9']
--            , ['0'..'9'] ++ ['a'] ++ ['A']
--            , ['0'..'9'] ++ ['a'..'b'] ++ ['A'..'B']
--            , ['0'..'9'] ++ ['a'..'c'] ++ ['A'..'C']
--            , ['0'..'9'] ++ ['a'..'d'] ++ ['A'..'D']
--            , ['0'..'9'] ++ ['a'..'e'] ++ ['A'..'E']
--            , ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
--            ]

-- |Potential bit string bases
data Base =
   -- |Binary base bit string
   Base_B
   -- |Octal base bit string
   | Base_O
   -- |Hexadecimal base bit string
   | Base_X
   deriving (Eq,Ord)

instance QC.Arbitrary Base where
   arbitrary = QC.elements [Base_B,Base_O,Base_X]

-- |Generate a VHDL specific bit string literal
-- @
--    bit_string_literal ::= base_specifier " bit_value "
--    bit_value ::= extended_digit { [ underline ] extended_digit }
--    base_specified ::= B | O | X
-- @
-- Note: B, O, X are case insensitive.
--       \\'%\\' can be used as a container instead of \\'\\"\\'
-- Implemented as:
-- @
--    'genBitStr' ::= base_specifier base_container 'genExtendedStr' base_container
--    base_specifier ::= B | b | O | o | X | x
--    base_container ::= " | %
--    'genExtendedStr' ::= extended_digit { 'genUnderscoreExtendedChar' }
--    extended_digit ::= REGEX( [0-9a-fA-F] )
-- @
-- Note: Base containers must match
--       Relevant base restricts range of regex (only full range for hex/base16)
genBitStr :: QC.Gen (ParserExpectedOutput BitString)
genBitStr = do
   container <- QC.elements ['"','%']
   base <- QC.arbitrary
   baseChar <- QC.elements $ baseCharMap MapS.! base
   bitStr <- genExtendedStr base
   let expectedOutput = mkBitString $ concat $ map (convertToBitString base) bitStr
       parseInput = [baseChar,container] ++ bitStr ++ [container]
   return $ ExpectedOutput parseInput expectedOutput
   where baseCharMap = MapS.fromList
            [ (Base_B,"Bb")
            , (Base_O,"Oo")
            , (Base_X,"Xx")
            ]
         genExtendedStr :: Base -> QC.Gen String
         genExtendedStr base = do
            let allowedChars = charMap MapS.! base
            firstChar <- QC.elements allowedChars
            lengthStr <- QC.elements [0..100]
            otherChars <- replicateM lengthStr $ genUnderscoreExtendedChar allowedChars
            return $ [firstChar] ++ (concat otherChars)
         charMap = MapS.fromList
            [ (Base_B,"01")
            , (Base_O,['0'..'7'])
            , (Base_X,['0'..'9']++['A'..'F']++['a'..'f'])
            ]
         convertToBitString :: Base -> Char -> String
         convertToBitString _ '_' = ""
         convertToBitString Base_B '0' = "0"
         convertToBitString Base_B '1' = "1"
         convertToBitString Base_O '0' = "000"
         convertToBitString Base_O '1' = "001"
         convertToBitString Base_O '2' = "010"
         convertToBitString Base_O '3' = "011"
         convertToBitString Base_O '4' = "100"
         convertToBitString Base_O '5' = "101"
         convertToBitString Base_O '6' = "110"
         convertToBitString Base_O '7' = "111"
         convertToBitString Base_X '0' = "0000"
         convertToBitString Base_X '1' = "0001"
         convertToBitString Base_X '2' = "0010"
         convertToBitString Base_X '3' = "0011"
         convertToBitString Base_X '4' = "0100"
         convertToBitString Base_X '5' = "0101"
         convertToBitString Base_X '6' = "0110"
         convertToBitString Base_X '7' = "0111"
         convertToBitString Base_X '8' = "1000"
         convertToBitString Base_X '9' = "1001"
         convertToBitString Base_X chr
            | elem chr "Aa" = "1010"
            | elem chr "Bb" = "1011"
            | elem chr "Cc" = "1100"
            | elem chr "Dd" = "1101"
            | elem chr "Ee" = "1110"
            | elem chr "Ff" = "1111"

-- |Generate latter part of VHDL specific bit_value (or based_integer)
-- Part of VHDL bit string literal
-- @
--    bit_value ::= extended_digit { [ underline ] extended_digit }
--    based_integer ::=
--       extended_digit { [ underline ] extended_digit }
-- @
-- Implemented as:
-- @
--    'genUnderscoreExtendedChar' ::= [ _ ] extended_digit
--    extended_digit ::= REGEX( [0-9a-fA-F] )
-- @
-- Note: Relevant base restricts range of regex (only full range for hex/base16)
--       Regex range is actually passed into this function
genUnderscoreExtendedChar :: [Char] -> QC.Gen String
genUnderscoreExtendedChar allowedChars = do
   optionalUnderscore <- QC.elements [True,False]
   otherChar <- QC.elements allowedChars
   return $
      if optionalUnderscore then ['_',otherChar]
      else [otherChar]

-- |Generate a VHDL specific identifier
-- > identifier ::= letter { [ underline ] letter_or_digit }
-- Implemented as:
-- @
--    'genIdentifier' ::= letter { 'genUnderscoreLetterOrDigit' }
--    'genUnderscoreLetterOrDigit' ::= [ underline ] letter_or_digit
-- @
genIdentifier :: Int -> Int -> QC.Gen String
genIdentifier fromLength toLength = do
   letter <- QC.elements letters
   lengthStr <- QC.elements [fromLength..toLength]
   otherLetters <- replicateM lengthStr genUnderscoreLetterOrDigit
   let identifier = (letter:concat otherLetters)
   return identifier
   where letters = ['a'..'z'] ++ ['A'..'Z']
         letters_or_digits = ['0'..'9'] ++ letters
         genUnderscoreLetterOrDigit :: QC.Gen String
         genUnderscoreLetterOrDigit = do
            optionalUnderscore <- QC.elements [True,False]
            letter_or_digit <- QC.elements letters_or_digits
            return $
               if optionalUnderscore then ['_',letter_or_digit]
               else [letter_or_digit]

---- |Generate a VHDL specific decimal literal
---- Arguments: range of unit string, range of optional decimal part, whether exponent used
---- > decimal_literal ::= integer [ . integer ] [ exponent ]
--genDecimal :: (Int,Int) -> Maybe (Int,Int) -> Bool -> QC.Gen String
--genDecimal (unitsFrom,unitsTo) decimalRange includeExponent = do
--   intStr <- genInteger unitsFrom unitsTo
--   decStr <- case decimalRange of
--               Just (decsFrom,decsTo) -> do
--                  baseDecStr <- genInteger decsFrom decsTo
--                  return $ "." ++ baseDecStr
--               Nothing -> return ""
--   expStr <- if includeExponent then genExponent else return ""
--   return $ intStr ++ decStr ++ expStr

-- |Generate valid keyword
-- When passed valid keyword as 'String' input, generates randomised case form of it.
-- E.g. Passed "and", returns 'QC.Gen' "ANd" or 'QC.Gen' "aNd", etc.
genKeyword :: String -> QC.Gen String
genKeyword = genCaseInsensitiveWord

-- |Generates a case insensitive word
-- When passed a 'String' word, generates randomised case form of it.
genCaseInsensitiveWord :: String -> QC.Gen String
genCaseInsensitiveWord word = do
   let genBool = QC.elements [True,False]
   wordBools <- replicateM (length word) genBool
   let lexInput = map chooseCase $ zip wordBools word
   return lexInput
   where chooseCase (True,chr) = toUpper chr
         chooseCase (False,chr) = toLower chr

--genDelimiter :: QC.Gen (GenPair [Token])
--genDelimiter =
--   let standardDelimiter = do
--         op <- QC.elements
--            [ Arrow
--            , DoubleStar
--            , VarAssign
--            , Inequality
--            , GreaterThanOrEqual
--            , SignAssign
--            , Box
--            , Ampersand
--            , Apostrophe
--            , LeftParen
--            , RightParen
--            , Star
--            , Plus
--            , Comma
--            , Hyphen
--            , Period
--            , Slash
--            , Colon
--            , Semicolon
--            , LessThan
--            , Equal
--            , GreaterThan
--            , Bar
--            ]
--         return $ GenPair (show op) [Operator op]
--       spaceDelimiter = do
--         length <- QC.choose (1,5)
--         let spaces = QC.elements "\t\n "
--         lexInput <- replicateM length spaces
--         return $ GenPair lexInput []
--   in QC.oneof [standardDelimiter,spaceDelimiter]

genComment :: QC.Gen (ParserExpectedOutput String)
genComment = do
   randomString <- QC.arbitrary
   let splitStringOnNewline = span (`notElem` "\r\n") randomString
       (input,expectedOutput) =
         case splitStringOnNewline of
            (fst,snd@('\r':'\n':rest)) -> (fst++snd,rest)
            (fst,snd@('\r':rest)) -> (fst++"\r\n"++rest,rest)
            (fst,snd@('\n':rest)) -> (fst++snd,rest)
            (fst,[]) -> (fst,[])
   return $ ExpectedOutput ("--" ++ input) expectedOutput
