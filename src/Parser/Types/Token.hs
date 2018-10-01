{-|
   Module      : Parser.Types.Token
   Description : Base tokenising types
|-}
module Parser.Types.Token
   ( UpperString
   , mkUpperString
   , AbstractLiteral(..)
   , BitString
   , mkBitString
   ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
         ( isLatin1
         , toUpper
         )
import Data.Int (Int64)

import Parser.Types.Token.Internal
         ( UpperString(..)
         , BitString(..)
         )

-- |Upper case string constructor
-- Converts string to upper case and packs it into a bytestring
mkUpperString :: String -> UpperString
mkUpperString = UpperString . B.pack . (map mkUpperChar)
   where mkUpperChar :: Char -> Char
         mkUpperChar chr
            | isLatin1 chr = toUpper chr
            | otherwise = error $ "Invalid character in upper case string '" ++ [chr] ++ "'"

-- |Abstract literal value
-- @
--    abstract_literal ::= decimal_literal | based_literal
-- @
-- = NOTE
-- Lexed output does not match tree specification
-- There are two acceptable types of this literal
-- * Decimal Literals
-- * Based Literals
-- The lexer converts both of these to their representative value:
-- * Integer value
-- * Real - Floating point value
data AbstractLiteral =
   -- |Universal integer type value
   -- Implemented with Haskell 'Data.Int.Int64' type
   UniversalInteger Int64
   -- |Universal real type value
   -- Implemented with Haskell 'Double' type
   | UniversalReal Double
   deriving(Eq,Show)

-- |Bit string constructor
-- Forces string to contain '0' and '1' and packs it into a bytestring
-- Error if string contains any characters other than '0' or '1'
mkBitString :: String -> BitString
mkBitString = BitString . B.pack . (map mkBitChar)
   where mkBitChar :: Char -> Char
         mkBitChar '0' = '0'
         mkBitChar '1' = '1'
         mkBitChar chr = error $ "Invalid character in bit string '" ++ [chr] ++ "'"
