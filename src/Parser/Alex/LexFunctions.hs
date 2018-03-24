module Parser.Alex.LexFunctions where

import Parser.Alex.BaseTypes
import Parser.Alex.Monad
import Parser.TokenTypes
import Parser.ErrorTypes

import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Function ((&))

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
         (_:value:('E':exponent):[]) -> (value,exponent)
         (_:value:('e':exponent):[]) -> (value,exponent)
         (_:value:[]) -> (value,"0")
       exponentInt = read exponent
       exponentValue =  base ** exponentInt
       convertedValue = case splitOn "." value of
         (unitStr:decStr:[]) -> convertUnits' unitStr + convertDecimals' decStr
         (unitStr:[]) -> convertUnits' unitStr
       totalValue = convertedValue * exponentValue
       convertToLiteralType =
         if floor totalValue == ceiling totalValue then Univ_Int . floor
         else Univ_Real
   in totalValue
      & convertToLiteralType
      & Literal -- ?? Check for overflow error
      & return
   where convertUnits ans _ [] = ans
         convertUnits curAns iter (unit:units) =
            let multiplier = base ^^ iter
                unitVal = read [unit]
                ans = curAns + (unitVal * multiplier)
            in convertUnits ans (iter+1) units
         convertUnits' = convertUnits 0.0 0
         convertDecimals ans _ [] = ans
         convertDecimals curAns iter (dec:decs) =
            let multiplier = base ^^ iter
                decVal = read [dec]
                ans = curAns + (decVal * multiplier)
            in convertDecimals ans (iter-1) decs
         convertDecimals' = convertDecimals 0.0 (-1)

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
