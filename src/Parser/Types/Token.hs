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
import Data.Char (toUpper)
import Data.Int (Int64)

import Parser.Types.Token.Internal
         ( UpperString(..)
         , BitString(..)
         )

-- |Upper case string constructor
-- Converts string to upper case and packs it into a bytestring
mkUpperString :: String -> UpperString
mkUpperString = UpperString . B.pack . (map toUpper)

-- |Abstract literal value
-- Abstract literals convert to either integer or floating point value
data AbstractLiteral =
   UniversalInteger Int64
   | UniversalReal Double
   deriving (Eq,Show)

-- |Bit string constructor
-- Forces string to contain '0' and '1' and packs it into a bytestring
-- Error if string contains any characters other than '0' or '1'
mkBitString :: String -> BitString
mkBitString = BitString . B.pack . (map mkBitChar)
   where mkBitChar :: Char -> Char
         mkBitChar '0' = '0'
         mkBitChar '1' = '1'
         mkBitChar chr = error $ "Invalid character in bit string '" ++ [chr] ++ "'"
