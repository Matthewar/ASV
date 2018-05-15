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
import qualified Data.Map.Strict as MapS

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
         , isKeywordDownto
         , isKeywordFile
         , isKeywordIs
         , isKeywordRange
         , isKeywordRecord
         , isKeywordTo
         , matchChar
         , matchIdentifier
         )
import Parser.Netlist.Types.Representation
         ( Type(..)
         , Subtype(..)
         , Enumerate(..)
         , NetlistName
         , Calculation(..)
         , Value(..)
         , IntegerRange(..)
         , FloatRange(..)
         , RangeDirection(..)
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
import Parser.Types.Expressions
         ( Staticity(..)
         , AllTypes(..)
         )
import Parser.Functions.Parse.Expression (parseSimpleExpression)
import Manager.Types.Error (ConverterError(..))

parseType :: ScopeStore -> UnitStore -> NetlistName -> ParserStack (MapS.Map String Type -> MapS.Map String Type,MapS.Map String Subtype -> MapS.Map String Subtype)
parseType scope unit unitName = do
   -- If name is in scope, that is okay, but if name is in package, this is an error
   typeNameTok <- getToken
   typeName <- case matchIdentifier typeNameTok of
                  Just (PosnWrapper _ name) -> return $ map toUpper name
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedTypeName typeNameTok
   when (isNameInUnit unit typeName) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_DuplicateTypeName typeName) typeNameTok
   parseTypeDefinition scope unit unitName typeName

-- |Convert type definition
parseTypeDefinition :: ScopeStore -> UnitStore -> NetlistName -> String -> ParserStack (MapS.Map String Type -> MapS.Map String Type,MapS.Map String Subtype -> MapS.Map String Subtype)
parseTypeDefinition scope unit unitName typeName = do
   fstToken <- getToken
   when (isSemicolon fstToken) $ throwError $ ConverterError_NotImplemented $ passPosition "Incomplete type definition" fstToken
   unless (isKeywordIs fstToken) $ throwError $ ConverterError_NotImplemented $ passPosition "Correct error here would be after check for semicolon or is keyword in type definition" fstToken
   sndToken <- getToken
   typeDef <- parseTypeDefinition' scope unit unitName typeName sndToken
   endToken <- getToken
   if isSemicolon endToken
      then return typeDef
      else throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInTypeDef endToken

parseTypeDefinition' :: ScopeStore -> UnitStore -> NetlistName -> String -> WrappedToken -> ParserStack (MapS.Map String Type -> MapS.Map String Type,MapS.Map String Subtype -> MapS.Map String Subtype)
parseTypeDefinition' scope unit unitName typeName token
   | isKeywordRange token = do
      leftBound <- parseSimpleExpression LocallyStatic scope unit unitName -- ?? Parse range attribute
      directionToken <- getToken
      direction <- case directionToken of
                     token | isKeywordTo token -> return To
                     token | isKeywordDownto token -> return Downto
                     token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedDirection token
      rightBound <- parseSimpleExpression LocallyStatic scope unit unitName
      let filterTypes (_,Type_Type _ IntegerType) = True
          filterTypes (_,Type_Type _ FloatingType) = True
          filterTypes (_,Type_UniversalInt) = True
          filterTypes (_,Type_UniversalReal) = True
          filterTypes _ = False
          applyFilter = filter filterTypes
      (typ,subtype) <-
         case (applyFilter leftBound,applyFilter rightBound) of
            ([],_) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_ExpectedIntOrFloatLeftBoundRange token
            (_,[]) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_ExpectedIntOrFloatRightBoundRange token
            ([(Calc_Value (Value_Int left),_)],[(Calc_Value (Value_Int right),_)]) ->
               return $ (IntegerType,IntegerSubtype Nothing (unitName,"ANON'"++typeName) (IntegerRange left right direction))
            ([(Calc_Value (Value_Float left),_)],[(Calc_Value (Value_Float right),_)]) ->
               return $ (FloatingType,FloatingSubtype Nothing (unitName,"ANON'"++typeName) (FloatRange left right direction))
            ([_],[_]) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_RangeTypeNoMatch token
            ([_],_) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotInferValueFromContextInRangeTypeLeftBound token
            (_,[_]) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotInferValueFromContextInRangeTypeRightBound token
      -- ?? Physical types if integer range
      let valTypeFunc = MapS.insert ("ANON'"++typeName) typ
          valSubtypeFunc = MapS.insert typeName subtype
      return (valTypeFunc,valSubtypeFunc)
   | isLeftParen token = do
      newEnums <- parseEnumerationLiterals unit typeName
      let enumType = EnumerationType newEnums
          enumSubtype =
            EnumerationSubtype
               Nothing
               (unitName,"ANON'"++typeName)
               newEnums
               (head newEnums,last newEnums)
          enumTypeFunc = MapS.insert ("ANON'"++typeName) enumType
          enumSubtypeFunc = MapS.insert typeName enumSubtype
      return (enumTypeFunc,enumSubtypeFunc)
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
         let newEnums = newEnum:convEnums
         if shouldContinue
            then parseEnumerationLiterals' newEnums
            else return $ reverse newEnums
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
