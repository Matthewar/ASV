{-|
   Module      : Parser.Functions.Parse.Type
   Description : Parse and convert type declaration
-}
module Parser.Functions.Parse.Type
   ( parseType
   ) where

import Control.Monad.Except
         ( throwError
         , when
         , unless
         )
import Data.Char (toUpper)
import Data.Maybe (fromJust)

import Lexer.Types.Token (WrappedToken)
import Lexer.Types.Error(ParserError(..))
import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper
         ( raisePosition
         , passPosition
         )
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad (getToken)
import Parser.Functions.IdentifyToken
         ( isChar
         , isComma
         , isIdentifier
         , isLeftParen
         , isRightParen
         , isSemicolon
         , isKeywordAccess
         , isKeywordArray
         , isKeywordFile
         , isKeywordIs
         , isKeywordRange
         , isKeywordRecord
         , matchChar
         , matchIdentifier
         )
import Parser.Netlist.Types.Representation
         ( Type(..)
         , Enumerate(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , UnitStore
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Netlist.Functions.Stores
         ( isNameInUnit
         , isEnumNameInUnit
         )
import Manager.Types.Error (ConverterError(..))

parseType :: ScopeStore -> UnitStore -> ParserStack (String,Type)
parseType scope unit = do
   -- If name is in scope, that is okay, but if name is in package, this is an error
   typeNameTok <- getToken
   typeName <- case matchIdentifier typeNameTok of
                  Just (PosnWrapper _ name) -> return $ map toUpper name
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedTypeName typeNameTok
   when (isNameInUnit unit typeName) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_DuplicateTypeName typeName) typeNameTok
   newType <- parseTypeDefinition scope unit typeName
   return (typeName,newType)

-- |Convert type definition
parseTypeDefinition :: ScopeStore -> UnitStore -> String -> ParserStack Type
parseTypeDefinition scope unit typeName = do
   fstToken <- getToken
   when (isSemicolon fstToken) $ throwError $ ConverterError_NotImplemented $ passPosition "Incomplete type definition" fstToken
   unless (isKeywordIs fstToken) $ throwError $ ConverterError_NotImplemented $ passPosition "Correct error here would be after check for semicolon or is keyword in type definition" fstToken
   sndToken <- getToken
   typeDef <- parseTypeDefinition' scope unit typeName sndToken
   endToken <- getToken
   if isSemicolon endToken
      then return typeDef
      else throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInTypeDef endToken

parseTypeDefinition' :: ScopeStore -> UnitStore -> String -> WrappedToken -> ParserStack Type
parseTypeDefinition' scope unit typeName token
   | isKeywordRange token = throwError $ ConverterError_NotImplemented $ passPosition "Universal, physical types" token
   | isLeftParen token = do
      newEnums <- parseEnumerationLiterals unit typeName
      return $ EnumerationType newEnums
   | isKeywordArray token = throwError $ ConverterError_NotImplemented $ passPosition "Array type" token
   | isKeywordRecord token = throwError $ ConverterError_NotImplemented $ passPosition "Record type" token
   | isKeywordAccess token = throwError $ ConverterError_NotImplemented $ passPosition "Access type" token
   | isKeywordFile token = throwError $ ConverterError_NotImplemented $ passPosition "File type" token
   | otherwise = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedTypeDefinition token

--parseRange :: ScopeStore -> UnitStore -> ParserStack Range
--parseRange scope unit = do
--   expression1 <- parseSimpleExpression scope unit
--
--parseSimpleExpression :: ScopeStore -> UnitStore -> ParserStack Calculation
--parseSimpleExpression scope unit = do
--   signOrTermTok <- getToken
--   let checkSign :: WrappedToken -> ParserStack Sign
--       checkSign token
--         | isPlus token = return Plus
--         | isHyphen token = return Minus
--         | otherwise = do
--            saveToken token
--            return Plus
--   sign <- checkSign signOrTermTok
--   term <- parseTerm scope unit

parseEnumerationLiterals :: UnitStore -> String -> ParserStack [Enumerate]
parseEnumerationLiterals unit typeName =
   let parseEnumerationLiterals' :: [Enumerate] -> ParserStack [Enumerate]
       parseEnumerationLiterals' convEnums = do
         enumToken <- getToken
         newEnum <- identifyEnumToken enumToken
         when (elem newEnum convEnums) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_DuplicateEnums newEnum) enumToken
         nextToken <- getToken
         shouldContinue <- shouldEnumCont nextToken
         if shouldContinue
            then parseEnumerationLiterals' (newEnum:convEnums)
            else return $ reverse convEnums
       identifyEnumToken :: WrappedToken -> ParserStack Enumerate
       identifyEnumToken token
         | isIdentifier token = do
            let iden = map toUpper $ unPos $ fromJust $ matchIdentifier token
            when (iden == typeName) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_EnumLitIsTypeName iden) token
            when (isEnumNameInUnit unit iden) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_InvalidEnumName iden) token
            return $ Enum_Identifier iden
         | isChar token = return $ Enum_Char $ unPos $ fromJust $ matchChar token
         | otherwise = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEnumLiteral token
       shouldEnumCont :: WrappedToken -> ParserStack Bool
       shouldEnumCont token
         | isComma token = return True
         | isRightParen token = return False
         | otherwise = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEnumCont token
   in parseEnumerationLiterals' []

--parseElementDeclaration :: [(String,Subtype)] -> ParserStack RecordStore
--parseIdentifierList :: [String] -> ParserStack [String]
--parseIdentifierList list = 
