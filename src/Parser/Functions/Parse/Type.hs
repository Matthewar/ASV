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
         , replicateM
         )
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Data.Int (Int64)
import Data.Either
         ( isLeft
         , isRight
         )
import qualified Data.Map.Strict as MapS

import qualified Lexer.Types.Token as Tokens
import Lexer.Types.Error(ParserError(..))
import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper
         ( raisePosition
         , passPosition
         )
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Functions.IdentifyToken
         ( isChar
         , isComma
         , isEqual
         , isIdentifier
         , isLeftParen
         , isRightParen
         , isSemicolon
         , isKeywordAccess
         , isKeywordArray
         , isKeywordDownto
         , isKeywordEnd
         , isKeywordFile
         , isKeywordIs
         , isKeywordRange
         , isKeywordRecord
         , isKeywordTo
         , isKeywordUnits
         , matchChar
         , matchIdentifier
         , matchInteger
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

parseTypeDefinition' :: ScopeStore -> UnitStore -> NetlistName -> String -> Tokens.WrappedToken -> ParserStack (MapS.Map String Type -> MapS.Map String Type,MapS.Map String Subtype -> MapS.Map String Subtype)
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
      let compareFunc :: (Num a,Ord a) => a -> a -> Bool
          compareFunc = case direction of
                           To -> (<=)
                           Downto -> (>=)
          withinIntRange left right =
            let checkVal val = val <= toInteger (maxBound :: Int64) && val >= toInteger (minBound :: Int64)
            in checkVal left && checkVal right
      range <-
         case (applyFilter leftBound,applyFilter rightBound) of
            ([],_) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_ExpectedIntOrFloatLeftBoundRange token
            (_,[]) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_ExpectedIntOrFloatRightBoundRange token
            ([(Calc_Value (Value_Int left),_)],[(Calc_Value (Value_Int right),_)]) | withinIntRange left right ->
               if left `compareFunc` right
                  then return $ Right $ IntegerRange (fromInteger left) (fromInteger right) direction
                  else throwError $ ConverterError_NotImplemented $ passPosition "Integer null range" token
            ([(Calc_Value (Value_Int left),_)],[(Calc_Value (Value_Int right),_)]) ->
               throwError $ ConverterError_Netlist $ passPosition (NetlistError_IntegerTypeOutOfBounds left right) token
            ([(Calc_Value (Value_Float left),_)],[(Calc_Value (Value_Float right),_)]) | isInfinite left || isInfinite right ->
               throwError $ ConverterError_Netlist $ passPosition (NetlistError_FloatingTypeOutOfBounds left right) token
            ([(Calc_Value (Value_Float left),_)],[(Calc_Value (Value_Float right),_)]) ->
               if left `compareFunc` right
                  then return $ Left $ FloatRange left right direction
                  else throwError $ ConverterError_NotImplemented $ passPosition "Floating null range" token
            ([_],[_]) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_RangeTypeNoMatch token
            ([_],_) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotInferValueFromContextInRangeTypeLeftBound token
            (_,[_]) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotInferValueFromContextInRangeTypeRightBound token
      unitToken <- getToken
      case (unitToken,range) of
         (token,Right intRange) | isKeywordUnits token -> parsePhysicalType scope unit (unitName,typeName) intRange
         (token,Left _) | isKeywordUnits token -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FloatingRangeInPhysicalDefinition token
         (token,_) -> do
            saveToken token
            (rangeType,rangeSubtype) <-
               case range of
                  Right intRange -> return $ (IntegerType,IntegerSubtype Nothing (unitName,"ANON'"++typeName) intRange)
                  Left floatRange -> return $ (FloatingType,FloatingSubtype Nothing (unitName,"ANON'"++typeName) floatRange)
            let rangeTypeFunc = MapS.insert ("ANON'"++typeName) rangeType
                rangeSubtypeFunc = MapS.insert typeName rangeSubtype
            return (rangeTypeFunc,rangeSubtypeFunc)
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

parsePhysicalType :: ScopeStore -> UnitStore -> (NetlistName,String) -> IntegerRange -> ParserStack (MapS.Map String Type -> MapS.Map String Type,MapS.Map String Subtype -> MapS.Map String Subtype)
parsePhysicalType scope unit (unitName,typeName) range = do
   [baseUnitTok,semicolonTok] <- replicateM 2 getToken
   baseUnit <- case matchIdentifier baseUnitTok of
                  Just (PosnWrapper pos name) ->
                     let upperName = map toUpper name
                     in if isNameInUnit unit upperName
                           then throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_InvalidPhysLitName upperName
                           else return upperName
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedBaseUnitIdentifierInTypeDef baseUnitTok
   unless (isSemicolon semicolonTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInPhysTypeDef semicolonTok
   let parseSecondaryUnit :: MapS.Map String Int64 -> PosnWrapper String -> ParserStack (MapS.Map String Int64)
       parseSecondaryUnit secondaryUnits (PosnWrapper pos secondaryUnitName) = do
         when (secondaryUnitName == typeName) $ throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_SecondaryUnitNameIsTypeName secondaryUnitName
         when (secondaryUnitName == baseUnit || MapS.member secondaryUnitName secondaryUnits) $ throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_DuplicateSecondaryUnitInPhysType secondaryUnitName
         when (isNameInUnit unit secondaryUnitName) $ throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_InvalidPhysLitName secondaryUnitName
         [equalTok,litTok,optTok1,optTok2] <- replicateM 4 getToken
         unless (isEqual equalTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEqualInSecondaryUnitDecl equalTok
         let lookupBaseMul :: PosnWrapper String -> ParserStack Int64
             lookupBaseMul (PosnWrapper pos physUnitName) =
               if physUnitName == baseUnit
                  then return 1
                  else case MapS.lookup physUnitName secondaryUnits of
                        Just val -> return val
                        Nothing -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_UnrecognisedPhysicalUnitName physUnitName
             getBaseUnitMul mulVal token =
               case matchIdentifier token of
                  Just (PosnWrapper pos name) -> do
                     mul2 <- lookupBaseMul $ PosnWrapper pos $ map toUpper name
                     return $ mulVal * mul2
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPhysicalUnitName token
         baseUnitMul <-
            case matchInteger litTok of -- ?? Check for out of range units?
               Just (PosnWrapper pos int) -> do
                  val <- getBaseUnitMul int optTok1
                  unless (isSemicolon optTok2) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInPhysTypeDef optTok2
                  return val
               Nothing -> do
                  val <- getBaseUnitMul 1 litTok
                  unless (isSemicolon optTok1) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInPhysTypeDef optTok1
                  saveToken optTok2
                  return val
         parsePhysicalType' $ MapS.insert secondaryUnitName baseUnitMul secondaryUnits
       parsePhysicalType' :: MapS.Map String Int64 -> ParserStack (MapS.Map String Int64)
       parsePhysicalType' secondaryUnits = do
         firstTok <- getToken
         case firstTok of
            token | isKeywordEnd token -> do
               unitToken <- getToken
               if isKeywordUnits unitToken
                  then return secondaryUnits
                  else throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEndUnitsInPhysType unitToken
            PosnWrapper pos (Tokens.Identifier iden) -> parseSecondaryUnit secondaryUnits $ PosnWrapper pos $ map toUpper iden
            token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSecondaryDeclarationOrEndInPhysType token
   secondaryUnits <- parsePhysicalType' MapS.empty
   let physTypeFunc = MapS.insert ("ANON'"++typeName) $ PhysicalType baseUnit secondaryUnits
       physSubtypeFunc = MapS.insert typeName $ PhysicalSubtype Nothing (unitName,"ANON'"++typeName) baseUnit secondaryUnits range
   return (physTypeFunc,physSubtypeFunc)

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
       identifyEnumToken :: Tokens.WrappedToken -> ParserStack Enumerate
       identifyEnumToken token
         | isIdentifier token = do
            let iden = map toUpper $ unPos $ fromJust $ matchIdentifier token
            when (iden == typeName) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_EnumLitIsTypeName iden) token
            when (isEnumNameInUnit unit iden) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_InvalidEnumName iden) token
            return $ Enum_Identifier iden
         | isChar token = return $ Enum_Char $ unPos $ fromJust $ matchChar token
         | otherwise = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEnumLiteral token
       shouldEnumCont :: Tokens.WrappedToken -> ParserStack Bool
       shouldEnumCont token
         | isComma token = return True
         | isRightParen token = return False
         | otherwise = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEnumCont token
   in parseEnumerationLiterals' []

--parseElementDeclaration :: [(String,Subtype)] -> ParserStack RecordStore
--parseIdentifierList :: [String] -> ParserStack [String]
--parseIdentifierList list = 
