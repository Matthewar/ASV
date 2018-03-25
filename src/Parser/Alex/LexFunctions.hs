{-|
   Module      : Parser.Alex.LexFunctions
   Description : Custom lexer functions
|-}
module Parser.Alex.LexFunctions where

import Parser.Alex.BaseTypes
import Parser.Alex.Monad
import Parser.TokenTypes
import Parser.ErrorTypes

import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.List.Split (splitOn)
import Data.List (findIndex)
import Text.Read (readMaybe)
import Data.Function ((&))
import qualified Data.Map.Strict as MapS
import Data.Char (toUpper,digitToInt)
import Numeric (readInt)

-- |Basic token creator function signature
type CreateToken = String -> Either ParserError Token

-- |Generate a reserved word (keyword) token
makeReserved :: ReservedWord -> CreateToken
makeReserved keyword _ =
   return $ Keyword keyword

-- |Generate an identifier token
makeIdentifier :: CreateToken
makeIdentifier identifier =
   return $ Identifier identifier

-- |Generate a decimal literal token
makeDecimalLiteral :: CreateToken
makeDecimalLiteral extractedStr =
   let formattedStr = filter (\char -> char /= '_') extractedStr
       isReal = elem '.' formattedStr
       convertToLiteralType =
         if isReal then Univ_Real
         else Univ_Int . floor
       checkError Nothing =
         let errorType =
               if isReal then LexErr_UniversalReal_OutOfBounds
               else LexErr_UniversalInt_OutOfBounds
         in Left $ errorType extractedStr
       checkError (Just value) =
         value
         & convertToLiteralType
         & Literal
         & return
   in formattedStr
      & readMaybe
      & checkError

-- |Generate an invalid decimal literal error
errorDecimalLiteral :: CreateToken
errorDecimalLiteral extractedStr =
   Left $ LexErr_DecimalLiteral_InvalidFormat extractedStr

-- |Generate a based literal token
makeBasedLiteral :: Char -> Double -> CreateToken
makeBasedLiteral separator base basedStr =
   let formattedStr = filter (\char -> char /= '_') basedStr
       (value,exponent) = case splitOn [separator] formattedStr of
         (_:val:('E':exp):[]) -> (val,exp)
         (_:val:('e':exp):[]) -> (val,exp)
         (_:val:"":[]) -> (val,"0")
       shiftedValue = case splitOn "." value of
                        (units:decimals:[]) -> units ++ decimals
                        (units:[]) -> units
       exponentInt = let exponentAdjust = case findIndex (\char -> char == '.') value of
                                             Just index -> (length value) - (index+1)
                                             Nothing -> 0
                         expStr = case exponent of
                                    ('+':val) -> val
                                    val -> val
                     in (read expStr) - exponentAdjust
       convertedValue = (fromIntegral $ fromBase (toInteger $ floor base) shiftedValue) * (base ^^ exponentInt)
       convertToLiteralType =
         if floor convertedValue == ceiling convertedValue then Univ_Int . floor
         else Univ_Real
   in if isInfinite convertedValue then
         let errorType =
               if elem '.' value then
                  LexErr_UniversalReal_OutOfBounds
               else
                  LexErr_UniversalInt_OutOfBounds
         in Left $ errorType basedStr
      else
         convertedValue
         & convertToLiteralType
         & Literal
         & return
   where fromBase :: Integer -> String -> Integer
         fromBase base = fst . head . readInt base ((<base).digitToInteger) digitToInt
         digitToInteger :: Char -> Integer
         digitToInteger chr = let charMap =
                                    [ ('0',0)
                                    , ('1',1)
                                    , ('2',2)
                                    , ('3',3)
                                    , ('4',4)
                                    , ('5',5)
                                    , ('6',6)
                                    , ('7',7)
                                    , ('8',8)
                                    , ('9',9)
                                    , ('A',10)
                                    , ('B',11)
                                    , ('C',12)
                                    , ('D',13)
                                    , ('E',14)
                                    , ('F',15)
                                    ]
                                    & MapS.fromList
                              in charMap MapS.! (toUpper chr)

-- |Generate a character literal token
makeCharLiteral :: CreateToken
makeCharLiteral ('\'':char:'\'':[]) =
   return $ Literal $ Character char

-- |Generate a string literal token
makeStrLiteral :: CreateToken
makeStrLiteral completeStr =
   let container :: Char
       container = head completeStr
       strContents :: String
       strContents = -- Remove first and last elements
         completeStr & tail & init
       filterContainer :: String -> String -> String
       filterContainer (item1:item2:rest) outputStr =
         if item1 == item2 && item1 == container then
            filterContainer rest (item1:outputStr)
         else filterContainer (item2:rest) (item1:outputStr)
       filterContainer (item:rest) outputStr =
         filterContainer rest (item:outputStr)
       filterContainer [] outputStr = reverse outputStr
   in filterContainer strContents []
      & Str
      & Literal
      & return

-- |Generate a bit string literal token
makeBitStrLiteral :: LiteralBase -> CreateToken
makeBitStrLiteral base extractedStr =
   init extractedStr
   & \(_:_:bitString) ->
      filter (\c -> c /= '_') bitString
      & ByteString.pack
      & BitStr base
      & Literal
      & return

-- |Generate a generic invalid bit string literal error
errorBitStrLiteral :: CreateToken
errorBitStrLiteral bitStr =
   Left $ LexErr_BitStrLiteral_InvalidStr bitStr

-- |Generate a specific invalid bit string literal error: invalid base value
errorBitStrLiteralBase :: CreateToken
errorBitStrLiteralBase bitStr =
   let (base:_) = bitStr
   in Left $ LexErr_BitStrLiteral_InvalidBase base bitStr

-- |Generate a specific invalid bit string literal error: empty value
errorBitStrLiteralEmpty :: CreateToken
errorBitStrLiteralEmpty bitStr =
   Left $ LexErr_BitStrLiteral_EmptyStr bitStr

-- |Generate an operator token
makeOperator :: OperatorType -> CreateToken
makeOperator op _ =
   return $ Operator op
