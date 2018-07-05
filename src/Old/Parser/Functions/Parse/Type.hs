{-|
   Module      : Parser.Functions.Parse.Type
   Description : Parse and convert type declaration
-}
module Parser.Functions.Parse.Type
   ( parseType
   , parseRange
   , RangeConstraint(..)
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
import Data.List
         ( nub
         , intersect
         , elemIndex
         )
import qualified Data.Map.Strict as MapS

import Lexer.Alex.Types (AlexPosn)
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
         , TypeStore
         , SubtypeStore
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

parseType :: ScopeStore -> UnitStore -> NetlistName -> ParserStack (TypeStore -> TypeStore,SubtypeStore -> SubtypeStore)
parseType scope unit unitName = do
   -- If name is in scope, that is okay, but if name is in package, this is an error
   typeNameTok <- getToken
   typeName <- case matchIdentifier typeNameTok of
                  Just (PosnWrapper _ name) -> return $ map toUpper name
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedTypeName typeNameTok
   when (isNameInUnit unit typeName) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_DuplicateTypeName typeName) typeNameTok
   parseTypeDefinition scope unit unitName typeName

-- |Convert type definition
parseTypeDefinition :: ScopeStore -> UnitStore -> NetlistName -> String -> ParserStack (TypeStore -> TypeStore,SubtypeStore -> SubtypeStore)
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

parseTypeDefinition' :: ScopeStore -> UnitStore -> NetlistName -> String -> Tokens.WrappedToken -> ParserStack (TypeStore -> TypeStore,SubtypeStore -> SubtypeStore)
parseTypeDefinition' scope unit unitName typeName token
   | isKeywordRange token = do
      range <- parseRange scope unit unitName $ getPos token
      unitToken <- getToken
      case (unitToken,range) of
         (token,IntegerConstraint intRange) | isKeywordUnits token -> parsePhysicalType scope unit (unitName,typeName) intRange
         (token,FloatConstraint _) | isKeywordUnits token -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FloatingRangeInPhysicalDefinition token
         (_,EnumerationConstraint _) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_EnumRangeInTypeDef token
         (_,PhysicalConstraint _) -> throwError $ ConverterError_Netlist $ passPosition NetlistError_PhysRangeInTypeDef token
         (token,_) -> do
            saveToken token
            (rangeType,rangeSubtype) <-
               case range of
                  IntegerConstraint intRange -> return $ (IntegerType,IntegerSubtype Nothing (unitName,"ANON'"++typeName) intRange)
                  FloatConstraint floatRange -> return $ (FloatingType,FloatingSubtype Nothing (unitName,"ANON'"++typeName) floatRange)
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

data RangeConstraint =
   FloatConstraint FloatRange
   | IntegerConstraint IntegerRange
   | EnumerationConstraint [(Enumerate,Enumerate,NetlistName,String,Type)]
   | PhysicalConstraint IntegerRange

parseRange :: ScopeStore -> UnitStore -> NetlistName -> AlexPosn -> ParserStack RangeConstraint
parseRange scope unit unitName rangePos = do
   leftBound <- parseSimpleExpression LocallyStatic scope unit unitName -- ?? Parse range attribute
   directionToken <- getToken
   direction <- case directionToken of
                  token | isKeywordTo token -> return To
                  token | isKeywordDownto token -> return Downto
                  token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedDirection token
   rightBound <- parseSimpleExpression LocallyStatic scope unit unitName
   let compareFunc :: (Num a,Ord a) => a -> a -> Bool
       compareFunc = case direction of
                        To -> (<=)
                        Downto -> (>=)
       withinIntRange left right =
         let checkVal val = val <= toInteger (maxBound :: Int64) && val >= toInteger (minBound :: Int64)
         in checkVal left && checkVal right
       convertEnumval (Calc_Value (Value_Enum _ enum) _,Type_Type typeName (EnumerationType enums)) enumData =
         if MapS.member typeName enumData
            then MapS.adjust (\(typeEnums,selectedEnums) -> (typeEnums,nub $ enum:selectedEnums)) typeName enumData
            else MapS.insert typeName (enums,[enum]) enumData
       collectEnums = foldr convertEnumval MapS.empty
       enumFilter left right = do
         when (direction == Downto) $ throwError $ ConverterError_Netlist $ PosnWrapper rangePos NetlistError_DownToInEnumRange
         let leftMap = collectEnums left
             rightMap = collectEnums right
             keys = intersect (MapS.keys leftMap) (MapS.keys rightMap)
             convertEnum :: (NetlistName,String) -> ParserStack (Enumerate,Enumerate,NetlistName,String,Type)
             convertEnum key = do
               let leftEntry = leftMap MapS.! key
                   typeEnums = fst leftEntry
                   (typePackage,typeName) = key
               case (snd leftEntry,snd $ rightMap MapS.! key) of
                  ([enum1],[enum2]) -> if (fromJust $ elemIndex enum1 typeEnums) <= (fromJust $ elemIndex enum2 typeEnums) -- ?? Should this check be here or when the exact type is known, to avoid errors with ranges that are invalid for some types but not others
                                          then return (enum1,enum2,typePackage,typeName,EnumerationType typeEnums)
                                          else throwError $ ConverterError_NotImplemented $ PosnWrapper rangePos "Null range enumerated type"
                  ([_],_) -> throwError $ ConverterError_Netlist $ PosnWrapper rangePos NetlistError_CannotInferValueFromContextInRangeTypeRightBound
                  (_,[_]) -> throwError $ ConverterError_Netlist $ PosnWrapper rangePos NetlistError_CannotInferValueFromContextInRangeTypeLeftBound
         ranges <- mapM convertEnum keys
         return $ EnumerationConstraint ranges
   case (leftBound,rightBound) of
      -- ?? Can first two errors occur
      ([],_) -> throwError $ ConverterError_Netlist $ PosnWrapper rangePos NetlistError_ExpectedIntOrFloatLeftBoundRange
      (_,[]) -> throwError $ ConverterError_Netlist $ PosnWrapper rangePos NetlistError_ExpectedIntOrFloatRightBoundRange
      ([(Calc_Value (Value_Int left) _,_)],[(Calc_Value (Value_Int right) _,_)]) | withinIntRange left right ->
         if left `compareFunc` right
            then return $ IntegerConstraint $ IntegerRange (fromInteger left) (fromInteger right) direction
            else throwError $ ConverterError_NotImplemented $ PosnWrapper rangePos "Integer null range"
      ([(Calc_Value (Value_Int left) _,_)],[(Calc_Value (Value_Int right) _,_)]) ->
         throwError $ ConverterError_Netlist $ PosnWrapper rangePos $ NetlistError_IntegerTypeOutOfBounds left right
      ([(Calc_Value (Value_Float left) _,_)],[(Calc_Value (Value_Float right) _,_)]) | isInfinite left || isInfinite right ->
         throwError $ ConverterError_Netlist $ PosnWrapper rangePos $ NetlistError_FloatingTypeOutOfBounds left right
      ([(Calc_Value (Value_Float left) _,_)],[(Calc_Value (Value_Float right) _,_)]) ->
         if left `compareFunc` right
            then return $ FloatConstraint $ FloatRange left right direction
            else throwError $ ConverterError_NotImplemented $ PosnWrapper rangePos "Floating null range"
      ([(Calc_Value (Value_Physical left) _,typeData1)],[(Calc_Value (Value_Physical right) _,typeData2)]) | withinIntRange left right && typeData1 == typeData2 ->
         if left `compareFunc` right
            then return $ PhysicalConstraint $ IntegerRange (fromInteger left) (fromInteger right) direction
            else throwError $ ConverterError_NotImplemented $ PosnWrapper rangePos "Physical null range"
      ((Calc_Value (Value_Enum _ _) _,_):_,(Calc_Value (Value_Enum _ _) _,_):_) ->
         enumFilter leftBound rightBound
      ([_],[_]) -> throwError $ ConverterError_Netlist $ PosnWrapper rangePos NetlistError_RangeTypeNoMatch
      ([_],_) -> throwError $ ConverterError_Netlist $ PosnWrapper rangePos NetlistError_CannotInferValueFromContextInRangeTypeLeftBound
      (_,[_]) -> throwError $ ConverterError_Netlist $ PosnWrapper rangePos NetlistError_CannotInferValueFromContextInRangeTypeRightBound

parsePhysicalType :: ScopeStore -> UnitStore -> (NetlistName,String) -> IntegerRange -> ParserStack (TypeStore -> TypeStore,SubtypeStore -> SubtypeStore)
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
