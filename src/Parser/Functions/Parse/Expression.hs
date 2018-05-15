{-|
   Module      : Parser.Functions.Parse.Expression
   Description : Parse and convert expressions
-}
module Parser.Functions.Parse.Expression
   ( parseSimpleExpression
   , Staticity(..)
   ) where

import Data.Function ((&))
import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Data.Maybe
         ( isNothing
         , fromJust
         )
import Control.Monad.Except (throwError)

import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper (passPosition)
import qualified Lexer.Types.Token as Tokens
import Parser.Types.Expressions
         ( Staticity(..)
         , AllTypes(..)
         )
import Parser.Functions.Expressions (notLocallyStatic)
import Parser.Functions.IdentifyToken
         ( isDoubleStar
         , isIdentifier
         , isKeywordAbs
         , isKeywordNew
         , isKeywordNot
         , isKeywordNull
         , isHyphen
         , isLeftParen
         , isPlus
         , matchIdentifier
         )
import Parser.Netlist.Types.Representation
         ( Type(..)
         , Enumerate(..)
         , Subtype(..)
         , NetlistName
         , Function(..)
         , Designator(..)
         , FunctionInterface(..)
         , FunctionInterfaceType(..)
         , FunctionBody
         , Calculation(..)
         , Constant(..)
         , Value(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore(..)
         , UnitStore(..)
         )
import Parser.Netlist.Functions.Stores
         ( matchFunctionInScope
         , matchEnumNameInScope
         , matchEnumCharInScope
         , matchPhysicalUnitInScope
         , matchConstantNameInScope
         , matchSignalNameInScope
         )
import qualified Parser.Netlist.Types.Operators as Operators
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Manager.Types.Error (ConverterError(..))

-- |Parse an expression
--parseExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]

data UnaryOperator = Plus | Minus

subtypeToType :: Subtype -> AllTypes
subtypeToType subtype =
   let (baseTypeName,baseTypeData) =
         case subtype of
            EnumerationSubtype _ name enums _ -> (name,EnumerationType enums)
            IntegerSubtype _ name _ -> (name,IntegerType)
            FloatingSubtype _ name _ -> (name,FloatingType)
            PhysicalSubtype _ name base second _ -> (name,PhysicalType base second)
            ArraySubtype _ name bounds elemName elemData -> (name,ArrayType bounds elemName elemData)
   in Type_Type baseTypeName baseTypeData

parseSimpleExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parseSimpleExpression staticLevel scope unit unitName = do
   signOrTermTok <- getToken
   sign <- case signOrTermTok of
            token | isPlus token -> return $ Just Plus
            token | isHyphen token -> return $ Just Minus
            token -> do
               saveToken token
               return Nothing
   terms <- parseTerm staticLevel scope unit unitName
   let applyStatic oldVal newVal = case fromJust sign of
                                    Plus -> oldVal
                                    Minus -> newVal
       getNonStaticFuncs functionFinder origVal =
         case matchFunctionInScope functionFinder scope unit unitName of
            Nothing -> []
            Just functions ->
               let mapper (function,(_,package)) =
                     ( Calc_FunctionCall
                        (package,function)
                        [origVal]
                     , subtypeToType $ function_returnTypeData function
                     )
               in map mapper $ MapS.toList functions
       applyNonStatic functionFinder origVal =
         if notLocallyStatic staticLevel
            then return $ getNonStaticFuncs functionFinder origVal
            else return []
       applyStaticAndNonStatic staticVal functionFinder origVal =
         if notLocallyStatic staticLevel
            then return (staticVal:getNonStaticFuncs functionFinder origVal)
            else return [staticVal]
       operatorCheck operator = case fromJust sign of
                                 Plus -> operator == Operators.Plus
                                 Minus -> operator == Operators.Hyphen
       constIntFunctionFinder (Type_Type typeName IntegerType) (Function (Designator_Operator operator) [FunctionInterface FunctionInterfaceType_Constant _ _ subtype@(IntegerSubtype _ _ _)] _ _) =
         typeName == intSubtype_typeName subtype && operatorCheck operator
       constIntFunctionFinder Type_UniversalInt (Function (Designator_Operator operator) [FunctionInterface FunctionInterfaceType_Constant _ _ (IntegerSubtype _ _ _)] _ _) = operatorCheck operator
       constIntFunctionFinder _ _ = False
       constFloatFunctionFinder (Type_Type typeName FloatingType) (Function (Designator_Operator operator) [FunctionInterface FunctionInterfaceType_Constant _ _ subtype@(FloatingSubtype _ _ _)] _ _) =
         typeName == floatSubtype_typeName subtype && operatorCheck operator
       constFloatFunctionFinder Type_UniversalReal (Function (Designator_Operator operator) [FunctionInterface FunctionInterfaceType_Constant _ _ (FloatingSubtype _ _ _)] _ _) = operatorCheck operator
       constFloatFunctionFinder _ _ = False
       constPhysFunctionFinder (Type_Type typeName (PhysicalType _ _)) (Function (Designator_Operator operator) [FunctionInterface FunctionInterfaceType_Constant _ _ subtype@(PhysicalSubtype _ _ _ _ _)] _ _) =
         typeName == physSubtype_typeName subtype && operatorCheck operator
         -- ?? Can you overload physical type units, if not then can instantly match literal types
       constPhysFunctionFinder _ _ = False
       applyArith :: (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
       applyArith valComb@(Calc_Value (Value_Int val),typeData) =
         let staticVal = applyStatic valComb (Calc_Value $ Value_Int $ -val,typeData)
         in applyStaticAndNonStatic staticVal (constIntFunctionFinder typeData) (Calc_Value $ Value_Int val)
       applyArith valComb@(Calc_Value (Value_Float val),typeData) =
         let staticVal = applyStatic valComb (Calc_Value $ Value_Float $ - val,typeData)
         in applyStaticAndNonStatic staticVal (constFloatFunctionFinder typeData) (Calc_Value $ Value_Float val)
       applyArith valComb@(Calc_Value (Value_Physical val),typeData) =
         let staticVal = applyStatic valComb (Calc_Value $ Value_Physical $ -val,typeData)
         in applyStaticAndNonStatic staticVal (constPhysFunctionFinder typeData) (Calc_Value $ Value_Physical val)
       applyArith (calc,typeData@(Type_Type _ IntegerType))
         -- | isSignalCalc calc = applyNonStatic (sigIntFunctionFinder typeData) calc
         | otherwise = applyNonStatic (constIntFunctionFinder typeData) calc
       applyArith (calc,typeData@(Type_Type _ FloatingType))
         -- | isSignalCalc calc = applyNonStatic (sigFloatFunctionFinder typeData) calc
         | otherwise = applyNonStatic (constFloatFunctionFinder typeData) calc
       applyArith (calc,typeData@(Type_Type _ (PhysicalType _ _)))
         -- | isSignalCalc calc = applyNonStatic (sigPhysFunctionFinder typeData) calc
         | otherwise = applyNonStatic (constPhysFunctionFinder typeData) calc
       -- ?? Finish patterns
       --applyArith (,Type_Type name (EnumerationType enums))
       --applyArith (,Type_Type name ArrayType bounds elemTypeName elemTypeData)
       --applyArith (,Type_UniversalInt)
       --applyArith (,Type_UniversalReal)
       --applyArith (,Type_String str)
       --applyArith (,Type_BitString len)
   if isNothing sign
      then return terms
      else do
         allArith <- mapM applyArith terms
         case concat allArith of
            emptyList | null emptyList -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FailedSimpleExpression signOrTermTok
            nonEmpty -> return nonEmpty

parseTerm :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
-- ?? Temp
parseTerm = parseFactor

parseFactor :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parseFactor staticLevel scope unit unitName = do
   token <- getToken
   case token of
      _ | isKeywordAbs token -> throwError $ ConverterError_NotImplemented $ passPosition "abs processing in factor" token
      _ | isKeywordNot token -> throwError $ ConverterError_NotImplemented $ passPosition "not processing in factor" token
      _ -> do
         saveToken token
         primary1 <- parsePrimary staticLevel scope unit unitName
         middleToken <- getToken
         if isDoubleStar middleToken
            then do
               primary2 <- parsePrimary staticLevel scope unit unitName
               throwError $ ConverterError_NotImplemented $ passPosition "exponential processing in factor" middleToken
            else do
               saveToken middleToken
               return primary1

parsePrimary :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parsePrimary staticLevel scope unit unitName = do
   token <- getToken
   case token of
      _ | isIdentifier token -> -- Name, enum literal, type conversions, function call, qualified expression
         parsePrimaryIdentifier staticLevel scope unit unitName $ fromJust $ matchIdentifier token
      token@(PosnWrapper _ (Tokens.Literal (Tokens.Univ_Int intVal))) -> do
         potentialUnit <- getToken
         case matchIdentifier potentialUnit of
            Nothing -> do
               saveToken potentialUnit
               return [(Calc_Value $ Value_Int intVal,Type_UniversalInt)]
            Just (PosnWrapper pos potUnitName) ->
               let upperPotUnitName = map toUpper potUnitName
               in case matchPhysicalUnitInScope scope unit unitName upperPotUnitName of
                     Just ((typeName,typeData@(PhysicalType baseUnit otherUnits)),packageName) ->
                        let value = if upperPotUnitName == baseUnit
                                       then intVal
                                       else (otherUnits MapS.! upperPotUnitName) * intVal
                        in return [(Calc_Value $ Value_Physical $ value,Type_Type (packageName,typeName) typeData)]
                     Nothing -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_PhysicalUnitNotFound potUnitName
      token@(PosnWrapper _ (Tokens.Literal (Tokens.Univ_Real realVal))) -> do
         potentialUnit <- getToken
         case matchIdentifier potentialUnit of
            Nothing -> do
               saveToken potentialUnit
               return [(Calc_Value $ Value_Float realVal,Type_UniversalReal)]
            Just (PosnWrapper pos potUnitName) ->
               let upperPotUnitName = map toUpper potUnitName
               in case matchPhysicalUnitInScope scope unit unitName upperPotUnitName of
                     Just ((typeName,typeData@(PhysicalType baseUnit otherUnits)),packageName) ->
                        let value = if upperPotUnitName == baseUnit -- ?? round or error
                                       then round realVal
                                       else round $ (fromIntegral $ otherUnits MapS.! upperPotUnitName) * realVal
                        in return [(Calc_Value $ Value_Physical $ value,Type_Type (packageName,typeName) typeData)]
                     Nothing -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_PhysicalUnitNotFound potUnitName
      token@(PosnWrapper _ (Tokens.Literal (Tokens.Character char))) ->
         case matchEnumCharInScope char scope unit unitName of
            Just enumMap ->
               MapS.toList enumMap
               & map (\(typeName,(typ,packageName)) -> (Calc_Value $ Value_Enum (packageName,typeName) $ Enum_Char char,Type_Type (packageName,typeName) typ))
               & return
            Nothing -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_UnrecognisedEnumChar char) token
--      _ | isStr -- function call or array
--      _ | isBitstr token -> -- array of bits
      _ | isKeywordNull token -> throwError $ ConverterError_NotImplemented $ passPosition "Null literal" token
      _ | isKeywordNew token -> throwError $ ConverterError_NotImplemented $ passPosition "Primary allocator" token
      _ | isLeftParen token -> do -- ?? or aggregate
            throwError $ ConverterError_NotImplemented $ passPosition "Expression or aggregate" token
            --expression <- parseExpression staticLevel scope unit
            --endToken <- getToken
            --if isRightParen endToken
            --   then return expression
            --   else throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedRightParenInPrimary endToken

-- ?? Currently ignoring expanded names
parsePrimaryIdentifier :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> PosnWrapper String -> ParserStack [(Calculation,AllTypes)]
parsePrimaryIdentifier staticLevel scope unit unitName (PosnWrapper pos iden) =
   let --checkTypes =
       --  case matchTypeNameInScope scope unit unitName iden of
       --     Just (typ,netlistName) -> do
       --        nextToken <- getToken
       --        case nextToken of
       --           _ | isLeftParen nextToken -> -- ?? type conversion
       --              throwError $ ConverterError_NotImplemented $ passPosition "Explicit type conversions" nextToken
       --           _ | isApostrophe nextToken -> -- qualified expressions, attributes
       --              reviewToken <- getToken
       --              case matchIdentifier reviewToken of
       --                 Just wrappedAttributeName ->
       --                    parseTypeAttribute staticLevel scope unit typ netlistName iden wrappedAttributeName
       --                 Nothing | isLeftParen reviewToken -> -- qualified expressions
       --                    throwError $ ConverterError_NotImplemented $ passPosition "Qualified type expressions" reviewToken
       --                 Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedAttrOrTypeConv reviewToken
       --           _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedTypeExpression nextToken
       --     Nothing -> checkSubtypes
       --checkSubtypes =
       --  case matchSubtypeNameInScope scope unit unitName iden of
       --     Just (subtype,netlistName) -> --qualified expressions, attributes
       --     Nothing -> checkFunctions
       --checkFunctions =
       --  case matchFunctionNameInScope scope unit unitName iden of
       --     Just funcMap ->
       --        throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Function call in expression"
       --        --readFuncCall scope unit funcMap (PosnWrapper pos iden)
       --     Nothing -> checkEnums
       checkEnums =
         case matchEnumNameInScope iden scope unit unitName of
            Just enumMap ->
               MapS.toList enumMap
               & map (\(typeName,(typ,packageName)) -> (Calc_Value $ Value_Enum (packageName,typeName) $ Enum_Identifier iden,Type_Type (packageName,typeName) typ))
               & return
            Nothing -> checkConstants
       checkConstants =
         case matchConstantNameInScope scope unit unitName iden of -- ?? need attributes, indexed
            Just (Constant _ subtype Nothing,constPackage) | notLocallyStatic staticLevel -> return $ [(Calc_Const (constPackage,iden),subtypeToType subtype)]
            Just (Constant _ _ Nothing,_) -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_DeferredConst iden
            Just (Constant _ subtype (Just val),_) -> return [(Calc_Value val,subtypeToType subtype)]
            Nothing -> checkSignals
       checkSignals =
         case matchSignalNameInScope scope unit unitName iden of
            Just (sig,packageName) -> throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Signal name in expression" -- attributes, return $ [(Calc_Signal iden,typ)]
            Nothing -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_UnrecognisedName iden
   in checkEnums --checkTypes

--readFuncCall :: Staticity -> ExpectedType -> ScopeStore -> UnitStore -> MapS.Map Function (Maybe FunctionBody,Maybe NetlistName) -> PosnWrapper String -> ParserStack [(Calculation,AllTypes)]
--readFuncCall staticLevel scope unit namePos funcMap (PosnWrapper pos funcName) = do
--   parenToken <- getToken
--   functions <- if isLeftParen parenToken
--                  then
--                  else return $ MapS.filterWithKey (\(Function _ [] _) _ -> True)
--   if MapS.null functions
--      then throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_FunctionCallTypeMismatch iden
--   contToken <- getToken
--   case contToken of
--   -- ?? If return type is record, selected name (not implemented yet)
--   -- ?? If access type, could be selected name (not implemented yet)
--      _ | isPeriod contToken -> throwError $ ConverterError_NotImplemented $ passPosition "Record/access type from function call" contToken
--   -- ?? If array type, indexed name
--   -- ?? If 1D array type, sliced name
--      _ | isLeftParen contToken -> 
--   -- ?? Can it be an attribute (I don't think so)
--   -- ?? Should eventually have locally static functions
--      _ | isLocallyStatic staticLevel -> throwError $ ConverterError_NotImplemented $ passPosition "Static function detection" contToken
--      _ -> map (\(Function _ _ returnType) (_,packageName) -> (Calc_FunctionCall packageName funcName
--   return $ 

--parseFunctionArgs :: Staticity -> MapS.Map Function (Maybe FunctionBody,Maybe NetlistName) -> ScopeStore -> UnitStore -> ParserStack 
--parseFunctionArgs staticLevel functionMap scope unit =
--   let parseFunctionArgs' :: AssociationType -> MapS.Map String Calculation -> MapS.Map Function (Maybe FunctionBody,Maybe NetlistName) -> ParserStack (MapS.Map Function (Maybe FunctionBody,Maybe NetlistName))
--       parseFunctionArgs' assocType interface remainingFunctions = do
--         fstToken <- getToken
--         (reducedFunctions,thisAssoc) <-
--            case matchIdentifier fstToken of
--               Just (PosnWrapper pos iden) ->
--                  let upperIden = map toUpper iden
--                      filterFunction (FunctionInterface _ name _) _ = upperIden == name
--                  in case (MapS.filterWithKey filterFunction remainingFunctions,assocType) of
--                        (emptyMap,NamedAssoc _) | MapS.null emptyMap -> throwError $ ConverterError_Parse $ PosnWrapper pos $ ParseErr_ExpectedAssocParameterName upperIden
--                        (emptyMap,PositionalAssoc assocPos) | MapS.null emptyMap ->
--                           let sizeFilter (FunctionInterface interface _ _) _ = length interface > assocPos + 1
--                               validPosFunctions = MapS.filterWithKey sizeFilter remainingFunctions
--                           in if MapS.null validPosFunctions
--                                 then throwError $ ConverterError_Netlist $ passPosition (NetlistError_UnmatchedFuncSign interface) fstToken
--                                 else return (validPosFunctions,assocType)
--                        nonEmpty -> return (nonEmpty,NamedAssoc upperIden)
--               Nothing ->
--                  case assocType of
--                     NamedAssoc -> throwError $ ConverterError_Netlist $ passPosition NetlistError_PosAssocAfterNamed fstToken
--                     PositionalAssoc assocPos ->
--                        let sizeFilter (FunctionInterface interface _ _) _ = length interface > assocPos + 1
--                            validPosFunctions = MapS.filterWithKey sizeFilter remainingFunctions
--                        in if MapS.null validPosFunctions
--                              then throwError $ ConverterError_Netlist $ passPosition (NetlistError_UnmatchedFuncSign interface) fstToken
--                              else return (validPosFunctions,assocType)
--         validTypes <- case thisAssoc of
--                        NamedAssoc iden -> do
--                           when (MapS.member iden interface) $ throwError ConverterError_Netlist $ PosnWrapper pos $ NetlistError_DuplicateNamedFunctionArg iden
--                           linkToken <- getToken
--                           unless (isArrow linkToken) $ throwError ConverterError_Parse $ raisePosition ParseErr_ExpectedAssocArrow linkToken
--                           return $
--                              MapS.fromList $
--                              map (\key@(FunctionInterface _ _ subtype) -> (Subtype subtype,key)) $
--                              filter (\FunctionInterface _ interfaceName _ -> interfaceName == iden) $
--                              MapS.keys reducedFunctions
--                        PositionalAssoc pos -> do
--                           saveToken fstToken
--                           return $
--                              MapS.fromList $
--                              map (\key@(FunctionInterface _ _ subtype) -> (Subtype subtype,key)) $
--                              MapS.keys reducedFunctions
--         possibleCalcs <- parseExpression staticLevel Nothing scope unit
--         -- ?? Compare types
--         --filter (\typ -> any (\(_,typ2) -> compareTypes typ typ2) possibleCalcs) validTypes
--   in parseFunctionArgs' NoAssocType 

data AssociationType =
   PositionalAssoc Int
   | NamedAssoc String

--parseTypeAttribute :: Staticity -> ScopeStore -> UnitStore -> Type -> NetlistName -> String -> PosnWrapper String -> ParserStack [(Calculation,AllTypes)]
--parseTypeAttribute staticLevel scope unit typ packageName typeName (PosnWrapper pos attrName) = do
