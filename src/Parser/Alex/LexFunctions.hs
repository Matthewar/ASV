module Parser.Alex.LexFunctions where

import Parser.Alex.BaseTypes
import Parser.Alex.Monad
import Parser.TokenTypes
import Parser.ErrorTypes

import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Function ((&))
import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)

type CreateToken = String -> Either ParserError Token

makeReserved :: ReservedWord -> CreateToken
makeReserved keyword _ =
   return $ Keyword keyword

makeIdentifier :: CreateToken
makeIdentifier identifier =
   return $ Identifier identifier

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

errorDecimalLiteral :: CreateToken
errorDecimalLiteral extractedStr =
   Left $ LexErr_DecimalLiteral_InvalidFormat extractedStr

makeBasedLiteral :: Char -> Double -> CreateToken
makeBasedLiteral separator base basedStr =
   let formattedStr = filter (\char -> char /= '_') basedStr
       (value,exponent) = case splitOn [separator] formattedStr of
         (_:val:('E':exp):[]) -> (val,exp)
         (_:val:('e':exp):[]) -> (val,exp)
         (_:val:"":[]) -> (val,"0")
       exponentInt = read exponent
       exponentValue =  base ** exponentInt
       convertedValue = case splitOn "." value of
         (unitStr:decStr:[]) -> convertUnits' unitStr + convertDecimals' decStr
         (unitStr:[]) -> convertUnits' unitStr
       totalValue = convertedValue * exponentValue
       convertToLiteralType =
         if floor totalValue == ceiling totalValue then Univ_Int . floor
         else Univ_Real
   in if isInfinite totalValue then
         let errorType =
               if elem '.' value then
                  LexErr_UniversalReal_OutOfBounds
               else
                  LexErr_UniversalInt_OutOfBounds
         in Left $ errorType basedStr
      else
         totalValue
         & convertToLiteralType
         & Literal
         & return
   where convertUnits ans _ [] = ans
         convertUnits curAns iter (unit:units) =
            let multiplier = base ^^ iter
                unitVal = hexRead unit
                ans = curAns + (unitVal * multiplier)
            in convertUnits ans (iter+1) units
         convertUnits' units = convertUnits 0.0 0 $ reverse units
         convertDecimals ans _ [] = ans
         convertDecimals curAns iter (dec:decs) =
            let multiplier = base ^^ iter
                decVal = hexRead dec
                ans = curAns + (decVal * multiplier)
            in convertDecimals ans (iter-1) decs
         convertDecimals' = convertDecimals 0.0 (-1)
         hexRead chr = hexMap MapS.! toUpper chr
         hexMap =
            [ ('0',0.0)
            , ('1',1.0)
            , ('2',2.0)
            , ('3',3.0)
            , ('4',4.0)
            , ('5',5.0)
            , ('6',6.0)
            , ('7',7.0)
            , ('8',8.0)
            , ('9',9.0)
            , ('A',10.0)
            , ('B',11.0)
            , ('C',12.0)
            , ('D',13.0)
            , ('E',14.0)
            , ('F',15.0)
            ]
            & MapS.fromList

makeCharLiteral :: CreateToken
makeCharLiteral ('\'':char:'\'':[]) =
   return $ Literal $ Character char

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

makeBitStrLiteral :: LiteralBase -> CreateToken
makeBitStrLiteral base extractedStr =
   init extractedStr
   & \(_:_:bitString) ->
      filter (\c -> c /= '_') bitString
      & ByteString.pack
      & BitStr base
      & Literal
      & return

errorBitStrLiteral :: CreateToken
errorBitStrLiteral bitStr =
   Left $ LexErr_BitStrLiteral_InvalidStr bitStr

errorBitStrLiteralBase :: CreateToken
errorBitStrLiteralBase bitStr =
   let (base:_) = bitStr
   in Left $ LexErr_BitStrLiteral_InvalidBase base bitStr

errorBitStrLiteralEmpty :: CreateToken
errorBitStrLiteralEmpty bitStr =
   Left $ LexErr_BitStrLiteral_EmptyStr bitStr

makeOperator :: OperatorType -> CreateToken
makeOperator op _ =
   return $ Operator op
