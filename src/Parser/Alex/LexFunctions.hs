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

-- ?? Needs recoding
makeBasedLiteral :: Char -> CreateToken
makeBasedLiteral separator basedStr = do
   (base,value,exponent) <- case splitOn [separator] basedStr of
      (base:value:('E':exponent):[]) -> return (base,value,exponent)
      (base:value:"":[]) -> return (base,value,"0")
      _ -> Left $ LexErr_BasedLiteral_InvalidValue basedStr -- ?? This error is incorrect
   baseInt <- return $ read base
   exponentInt <- return $ read exponent
   let convertBasedUnits ans iter (unit:units) =
         (read [unit]) * (baseInt ** iter)
         & \currentDigitWeight -> currentDigitWeight + ans
         & \newAns -> convertBasedUnits newAns (iter+1) units
       convertBasedUnits ans _ [] = ans
       convertBasedDecimals ans iter (decimal:decimals) =
         (read [decimal]) * (baseInt ** iter)
         & \currentDigitWeight -> currentDigitWeight + ans
         & \newAns -> convertBasedDecimals newAns (iter-1) decimals
       convertBasedDecimals ans _ [] = ans
       convertBasedFloat number =
         case splitOn "." number of
            (units:decimals:[]) -> (convertBasedUnits 0 0 $ reverse units) + (convertBasedDecimals 0 (-1) decimals)
            (units:[]) -> convertBasedUnits 0 0 $ reverse units
   if elem baseInt [2..16] then
      convertBasedFloat value
      & \numericValue -> numericValue * (baseInt ^ exponentInt)
--      & Decimal
      & Univ_Real
      & Literal
      & return
   else Left $ LexErr_BasedLiteral_InvalidBaseValue (floor baseInt) basedStr

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
