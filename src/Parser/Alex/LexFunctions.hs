module Parser.Alex.LexFunctions where

import Parser.Alex.BaseTypes
import Parser.Alex.Monad
import Parser.Alex.Functions (alexError)
import Parser.TokenTypes
import Parser.ErrorTypes

import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Function ((&))

makeReserved :: ReservedWord -> AlexInput -> Int -> Alex Token
makeReserved keyword (position, _, _, _) _ =
   let keyType = Keyword keyword
   in return $ keyType

makeIdentifier :: AlexInput -> Int -> Alex Token
makeIdentifier (position, _, _, str) length =
   let identifier = take length str
   in return $ Identifier identifier

makeDecimalLiteral :: AlexInput -> Int -> Alex Token
makeDecimalLiteral (position, _, _, str) length =
   let extractedStr = take length str
       formattedStr = filter (\char -> char /= '_') extractedStr
       isReal = elem '.' formattedStr
       convertToLiteralType =
         if isReal then Univ_Real
         else Univ_Int . floor
       checkError Nothing =
         let errorType =
               if isReal then LexErr_UniversalReal_OutOfBounds
               else LexErr_UniversalInt_OutOfBounds
         in alexError $ errorType extractedStr position
       checkError (Just value) =
         value
         & convertToLiteralType
         & Literal
         & return
   in formattedStr
      & readMaybe
      & checkError

errorDecimalLiteral :: AlexInput -> Int -> Alex Token
errorDecimalLiteral (position, _, _, str) length =
   take length str
   & \err -> LexErr_DecimalLiteral_InvalidFormat err position
   & alexError

makeBasedLiteral :: Char -> AlexInput -> Int -> Alex Token
makeBasedLiteral separator (position, _, _, str) length = do
   let basedStr = take length str
   (base,value,exponent) <- case splitOn [separator] basedStr of
      (base:value:('E':exponent):[]) -> return (base,value,exponent)
      (base:value:"":[]) -> return (base,value,"0")
      _ -> alexError $ LexErr_BasedLiteral_InvalidValue basedStr position -- ?? This error is incorrect
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
   else alexError $ LexErr_BasedLiteral_InvalidBaseValue (floor baseInt) basedStr position

makeCharLiteral :: AlexInput -> Int -> Alex Token
makeCharLiteral (position, _, _, ('\'':char:'\'':[])) _ =
   return $ Literal $ Character char

makeStrLiteral :: AlexInput -> Int -> Alex Token
makeStrLiteral (position, _, _, str) length =
   let completeStr = take length str
       container :: Char
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

makeBitStrLiteral :: LiteralBase -> AlexInput -> Int -> Alex Token
makeBitStrLiteral base (position, _, _, str) length =
   take length str
   & init
   & \(_:_:bitString) ->
      filter (\c -> c /= '_') bitString
      & ByteString.pack
      & BitStr base
      & Literal
      & return

errorBitStrLiteral :: AlexInput -> Int -> Alex Token
errorBitStrLiteral (position, _, _, str) length =
   let bitStr = take length str
   in alexError $ LexErr_BitStrLiteral_InvalidStr bitStr position

errorBitStrLiteralBase :: AlexInput -> Int -> Alex Token
errorBitStrLiteralBase (position, _, _, str) length =
   let bitStr = take length str
       (base:_) = bitStr
   in alexError $ LexErr_BitStrLiteral_InvalidBase base bitStr position

errorBitStrLiteralEmpty :: AlexInput -> Int -> Alex Token
errorBitStrLiteralEmpty (position, _, _, str) length =
   let bitStr = take length str
   in alexError $ LexErr_BitStrLiteral_EmptyStr bitStr position

makeOperator :: OperatorType -> AlexInput -> Int -> Alex Token
makeOperator op (position, _, _, _) _ =
   return $ Operator op
