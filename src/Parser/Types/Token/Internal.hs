{-|
   Module      : Parser.Types.Token.Internal
   Description : Internal base tokenising types
|-}
module Parser.Types.Token.Internal
   ( UpperString(..)
   , BitString(..)
   ) where

import qualified Data.ByteString.Lazy.Char8 as B

-- |Upper case string data
-- Only permitted to contain upper case characters
newtype UpperString = UpperString B.ByteString
   deriving (Eq)

instance Show UpperString where
   show (UpperString byteStr) = B.unpack byteStr

-- |Bit string data
-- Only permitted to contain characters '0' and '1'
newtype BitString = BitString B.ByteString
   deriving (Eq)

instance Show BitString where
   show (BitString byteStr) = "\"" ++ B.unpack byteStr ++ "\""
