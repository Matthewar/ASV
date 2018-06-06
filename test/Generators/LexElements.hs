{-|
   Module      : Generators.LexElements
   Description : Generators of basic lexical elements of VHDL
-}
module Generators.LexElements 
   ( genInteger
   , genExponent
   , genBasedStr
   , genBitStr
   , genIdentifier
   , genDecimal
   , genKeyword
   ) where

import qualified Test.Tasty.QuickCheck as QC
import Data.Function ((&))
import qualified Data.Map.Strict as MapS
import Data.Char
         ( toLower
         , toUpper
         )
import Control.Monad

import Lexer.Types.Token
         ( ReservedWord(..)
         , LiteralBase(..)
         )

-- |Generate a VHDL specific integer
-- Two arguments: minimum and maximum additional length
-- Total length = randomly selected length within range + 1
--
-- > integer ::= digit { [ underline ] digit }
-- Implemented as:
-- @
--    'genInteger' ::= digit { 'genUnderscoreDigit' }
--    'genUnderscoreDigit' ::= [ underline ] digit
-- @
genInteger :: Int -> Int -> QC.Gen String
genInteger fromLength toLength = do
   firstInt <- QC.elements ['0'..'9']
   lengthStr <- QC.elements [fromLength..toLength]
   otherInts <- replicateM lengthStr genUnderscoreDigit
   return $ [firstInt] ++ (concat otherInts)
   where genUnderscoreDigit = do
            optionalUnderscore <- QC.elements [True,False]
            otherDigit <- QC.elements ['0'..'9']
            return $
               if optionalUnderscore then ['_',otherDigit]
               else [otherDigit]

-- |Generate a VHDL specific exponent
-- > exponent ::= E [ + ] integer  | E - integer
-- Note: case insensitive \\'E\\'
-- Implemented as:
-- @
--    'genExponent' ::= exponent_marker [ exponent_sign ] 'genInteger'
--    exponent_marker ::= E | e
--    exponent_sign ::= + | -
-- @
genExponent = do
   expChar <- QC.elements "Ee"
   expSign <- QC.elements ["+","-",""]
   expVal <- genInteger 0 1
   return $ (expChar:expSign) ++ expVal

-- |Generate a VHDL specific based literal
-- @
--    based_literal ::=
--       base # based_integer [ . based_integer ] # [ exponent ]
--    base ::= integer
--    based_integer ::=
--       extended_digit { [ underline ] extended_digit }
--    extended_digit ::= digit | letter
-- @
-- Note: \\'#\\' container can instead be \\':\\'
-- Implemented as:
-- @
--    'genBasedStr' ::=
--       base based_container 'genExtendedBasedStr' based_container [ 'genExponent' ]
--    base ::= HASKELL( [2..16] )
--    based_container ::= # | :
--    'genExtendedBasedStr' ::= based_integer [ . based_integer ]
--    based_integer ::= REGEX( [0-9a-fA-F] )
-- @
-- Note: based_container must be same on both sides
--       Relevant base restricts range of regex (only full range for hex/base16)
genBasedStr :: Char -> QC.Gen String
genBasedStr container = do
   base <- QC.elements [2..16]
   let baseChars = show base
       basedStrGen = genExtendedBasedStr base
   optionalFractional <- QC.elements [True,False]
   expStr <- genExponent
   unitBasedStr <- basedStrGen
   fullBasedStr <-
         if optionalFractional then do
            decBasedStr <- basedStrGen
            return $ unitBasedStr ++ "." ++ decBasedStr
         else
            return unitBasedStr
   return $ baseChars ++ [container] ++ fullBasedStr ++ [container] ++ expStr
   where genExtendedBasedStr :: Int -> QC.Gen String
         genExtendedBasedStr base = do
            let allowedChars = charMap !! (base-2)
            firstChar <- QC.elements allowedChars
            lengthStr <- QC.elements [0..25]
            otherChars <- replicateM lengthStr $ genUnderscoreExtendedChar allowedChars
            return $ [firstChar] ++ (concat otherChars)
         charMap =
            [ "01"
            , ['0'..'2']
            , ['0'..'3']
            , ['0'..'4']
            , ['0'..'5']
            , ['0'..'6']
            , ['0'..'7']
            , ['0'..'8']
            , ['0'..'9']
            , ['0'..'9'] ++ ['a'] ++ ['A']
            , ['0'..'9'] ++ ['a'..'b'] ++ ['A'..'B']
            , ['0'..'9'] ++ ['a'..'c'] ++ ['A'..'C']
            , ['0'..'9'] ++ ['a'..'d'] ++ ['A'..'D']
            , ['0'..'9'] ++ ['a'..'e'] ++ ['A'..'E']
            , ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
            ]

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
genBitStr :: Char -> QC.Gen String
genBitStr container = do
   base <- QC.elements [BinBased,OctBased,HexBased]
   baseChar <- QC.elements $ baseCharMap MapS.! base
   bitStr <- genExtendedStr base
   return $ [baseChar,container] ++ bitStr ++ [container]
   where baseCharMap =
            [ (BinBased,"Bb")
            , (OctBased,"Oo")
            , (HexBased,"Xx")
            ]
            & MapS.fromList
         genExtendedStr :: LiteralBase -> QC.Gen String
         genExtendedStr base = do
            let allowedChars = charMap MapS.! base
            firstChar <- QC.elements allowedChars
            lengthStr <- QC.elements [0..100]
            otherChars <- replicateM lengthStr $ genUnderscoreExtendedChar allowedChars
            return $ [firstChar] ++ (concat otherChars)
         charMap =
            [ (BinBased,"01")
            , (OctBased,['0'..'7'])
            , (HexBased,['0'..'9']++['A'..'F']++['a'..'f'])
            ]
            & MapS.fromList

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
   return (letter:concat otherLetters)
   where letters = ['a'..'z'] ++ ['A'..'Z']
         letters_or_digits = ['0'..'9'] ++ letters
         genUnderscoreLetterOrDigit :: QC.Gen String
         genUnderscoreLetterOrDigit = do
            optionalUnderscore <- QC.elements [True,False]
            letter_or_digit <- QC.elements letters_or_digits
            return $
               if optionalUnderscore then ['_',letter_or_digit]
               else [letter_or_digit]

-- |Generate a VHDL specific decimal literal
-- Arguments: range of unit string, range of optional decimal part, whether exponent used
-- > decimal_literal ::= integer [ . integer ] [ exponent ]
genDecimal :: (Int,Int) -> Maybe (Int,Int) -> Bool -> QC.Gen String
genDecimal (unitsFrom,unitsTo) decimalRange includeExponent = do
   intStr <- genInteger unitsFrom unitsTo
   decStr <- case decimalRange of
               Just (decsFrom,decsTo) -> do
                  baseDecStr <- genInteger decsFrom decsTo
                  return $ "." ++ baseDecStr
               Nothing -> return ""
   expStr <- if includeExponent then genExponent else return ""
   return $ intStr ++ decStr ++ expStr

-- |Generate a VHDL keyword
genKeyword :: QC.Gen String
genKeyword = do
   keyword <- QC.elements keywords
   let keywordLowerStr = show keyword & \(first:others) -> (toLower first:others)
       genBool = QC.elements [True,False]
   keywordBools <- replicateM (length keywordLowerStr) genBool
   return $ map chooseCase $ zip keywordBools keywordLowerStr
   where chooseCase (True,chr) = toUpper chr
         chooseCase (False,chr) = chr
         keywords =
            [ Abs
            , Access
            , After
            , Alias
            , All
            , And
            , Architecture
            , Array
            , Assert
            , Attribute
            , Begin
            , Block
            , Body
            , Buffer
            , Bus
            , Case
            , Component
            , Configuration
            , Constant
            , Disconnect
            , Downto
            , Else
            , Elsif
            , End
            , Entity
            , Exit
            , File
            , For
            , Function
            , Generate
            , Generic
            , Guarded
            , If
            , In
            , Inout
            , Is
            , Label
            , Library
            , Linkage
            , Loop
            , Map
            , Mod
            , Nand
            , New
            , Next
            , Nor
            , Not
            , Null
            , Of
            , On
            , Open
            , Or
            , Others
            , Out
            , Package
            , Port
            , Procedure
            , Process
            , Range
            , Record
            , Register
            , Rem
            , Report
            , Return
            , Select
            , Severity
            , Signal
            , Subtype
            , Then
            , To
            , Transport
            , Type
            , Units
            , Until
            , Use
            , Variable
            , Wait
            , When
            , While
            , With
            , Xor
            ]
