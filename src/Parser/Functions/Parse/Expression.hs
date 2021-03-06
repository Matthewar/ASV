{-|
   Module      : Parser.Functions.Parse.Expression
   Description : Parse and convert expressions
-}
module Parser.Functions.Parse.Expression
   ( parseExpression
   , parseSimpleExpression
   , staticTypeCompare
   , parseWithExpectedType
   , Staticity(..)
   ) where

import Data.Function ((&))
import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Data.Maybe
         ( isNothing
         , fromJust
         , mapMaybe
         )
import Data.List (elemIndex)
import Data.Bits
         ( (.&.)
         , (.|.)
         , xor
         )
import Data.Int (Int64)
import Control.Monad.Except (throwError)
import Control.Monad (unless)

import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper
         ( passPosition
         , raisePosition
         )
import qualified Lexer.Types.Token as Tokens
import Lexer.Alex.Types (AlexPosn)
import Lexer.Types.Error (ParserError(..))
import Parser.Types.Expressions
         ( Staticity(..)
         , AllTypes(..)
         )
import Parser.Functions.Expressions
         ( isLocallyStatic
         , notLocallyStatic
         , isNotStatic
         )
import Parser.Functions.IdentifyToken
         ( isAmpersand
         , isApostrophe
         , isDoubleStar
         , isEqual
         , isGreaterThan
         , isGreaterThanOrEqual
         , isHyphen
         , isIdentifier
         , isInequality
         , isKeywordAbs
         , isKeywordAnd
         , isKeywordMod
         , isKeywordNand
         , isKeywordNew
         , isKeywordNot
         , isKeywordNor
         , isKeywordNull
         , isKeywordOr
         , isKeywordRem
         , isKeywordXor
         , isLessThan
         , isLeftParen
         , isPlus
         , isRightParen
         , isSignAssign
         , isSlash
         , isStar
         , matchIdentifier
         )
import Parser.Netlist.Types.Representation
         ( Type(..)
         , Enumerate(..)
         , Subtype(..)
         , NetlistName(..)
         , Function(..)
         , Designator(..)
         , FunctionInterface(..)
         , FunctionInterfaceType(..)
         , FunctionBody
         , IntegerRange(..)
         , FloatRange(..)
         , RangeDirection(..)
         , Calculation(..)
         , Constant(..)
         , Value(..)
         , Signal(..)
         , SignalType(..)
         , Port(..)
         , Mode(..)
         , Generic(..)
         )
import Parser.Netlist.Functions.Representation
         ( enum_discretePos
         , enum_discreteVal
         , enum_discreteSucc
         , enum_discretePred
         , enum_discreteLeftOf
         , enum_discreteRightOf
         , int_scalarLeft
         , int_scalarRight
         , int_scalarHigh
         , int_scalarLow
         , int_discreteSucc
         , int_discretePred
         , int_discreteLeftOf
         , int_discreteRightOf
         , float_scalarLeft
         , float_scalarRight
         , float_scalarHigh
         , float_scalarLow
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore(..)
         , UnitStore(..)
         , Package(..)
         )
import Parser.Netlist.Functions.Stores
         ( matchFunctionInScope
         , matchFunctionNameInScope
         , matchSubtypeNameInScope
         , matchEnumNameInScope
         , matchEnumCharInScope
         , matchPhysicalUnitInScope
         , matchConstantNameInScope
         , matchSignalNameInScope
         , matchPortNameInScope
         , matchGenericNameInScope
         )
import qualified Parser.Netlist.Types.Operators as Operators
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Netlist.Builtin.Standard (standardPackage)
import Manager.Types.Error (ConverterError(..))

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

boolToBoolean :: Bool -> Value
boolToBoolean bool =
   let boolVal = if bool
                  then "TRUE"
                  else "FALSE"
   in Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") $ Enum_Identifier boolVal

-- |Parse an expression
parseExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parseExpression staticLevel scope unit unitName = do
   relation <- parseRelation staticLevel scope unit unitName
   logicalTok <- getToken
   case logicalTok of
      token | isKeywordAnd token -> parseAndExpression staticLevel scope unit unitName $ passPosition relation logicalTok
      token | isKeywordOr token -> parseOrExpression staticLevel scope unit unitName $ passPosition relation logicalTok
      token | isKeywordXor token -> parseXorExpression staticLevel scope unit unitName $ passPosition relation logicalTok
      token | isKeywordNand token -> parseNandExpression staticLevel scope unit unitName $ passPosition relation logicalTok
      token | isKeywordNor token -> parseNorExpression staticLevel scope unit unitName $ passPosition relation logicalTok
      token -> do
         saveToken token
         return relation

parseAndExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> PosnWrapper [(Calculation,AllTypes)] -> ParserStack [(Calculation,AllTypes)]
parseAndExpression staticLevel scope unit unitName (PosnWrapper pos prevCalcs) = do
   relation2 <- parseRelation staticLevel scope unit unitName
   let getNonStaticFuncs functionFinder origVal1 origVal2 =
         case matchFunctionInScope functionFinder scope unit unitName of
            Nothing -> []
            Just functions ->
               let mapper (function,(_,package)) =
                     ( Calc_FunctionCall
                        (package,function)
                        [origVal1,origVal2]
                     , subtypeToType $ function_returnTypeData function
                     )
               in map mapper $ MapS.toList functions
       applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return []
       applyNonStatic functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return $ getNonStaticFuncs functionFinder origVal1 origVal2
            else return []
       applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return [staticVal]
       constFunctionFinder leftIn rightIn
         (Function (Designator_Operator Operators.And)
            [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
            , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
            ] _ _) =
               let inputCheck calcType funcType = case (calcType,funcType) of
                     (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                     (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                     (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                     -- ?? need other checks
                     _ -> False
               in inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
       constFunctionFinder _ _ _ = False
       applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
       applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
         let typeCheck = case (typeData1,typeData2) of
                           -- ?? Need checks for string and bitstring
                           _ | typeData1 == typeData2 -> Just typeData1
                           _ -> Nothing
             nomatchCalcs = applyNonStatic
                              (constFunctionFinder typeData1 typeData2)
                              wrappedVal1
                              wrappedVal2
             convertCalcs valFunc arith val1 val2 =
               case typeCheck of
                  Just typeData -> applyStaticAndNonStatic
                                    (Calc_Value (valFunc $ val1 `arith` val2) typeData,typeData)
                                    (constFunctionFinder typeData1 typeData2)
                                    wrappedVal1
                                    wrappedVal2
                  Nothing -> nomatchCalcs
             enumToBool :: Enumerate -> Bool
             enumToBool (Enum_Identifier "TRUE") = True
             enumToBool (Enum_Identifier "FALSE") = False
             enumToBool (Enum_Char '1') = True
             enumToBool (Enum_Char '0') = False
         in case (val1,val2) of
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val2) -> convertCalcs boolToBoolean (.&.) (enumToBool val1) (enumToBool val2)
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val2) -> convertCalcs boolToBoolean (.&.) (enumToBool val1) (enumToBool val2)
               -- ?? Need checks for string and bitstring, arrays
               _ -> nomatchCalcs
       applyArith (calc1,typeData1) (calc2,typeData2) =
         let convertCalcs calcFunc typeData =
               applyNonStaticBuiltin
                  (constFunctionFinder typeData1 typeData2)
                  (calcFunc calc1 calc2 typeData,typeData)
                  calc1
                  calc2
         in case (typeData1,typeData2) of
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinAnd typeData1
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinAnd typeData1
               -- ?? Need checks for string and bitstring, arrays
               _ -> applyNonStatic
                     (constFunctionFinder typeData1 typeData2)
                     calc1
                     calc2
   allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- prevCalcs, right <- relation2]
   case concat allArith of
      emptyList | null emptyList -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_FailedExpression
      nonEmpty -> do
         contTok <- getToken
         case contTok of
            token | isKeywordAnd token -> parseAndExpression staticLevel scope unit unitName $ passPosition nonEmpty contTok
            token -> do
               saveToken token
               return nonEmpty

parseOrExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> PosnWrapper [(Calculation,AllTypes)] -> ParserStack [(Calculation,AllTypes)]
parseOrExpression staticLevel scope unit unitName (PosnWrapper pos prevCalcs) = do
   relation2 <- parseRelation staticLevel scope unit unitName
   let getNonStaticFuncs functionFinder origVal1 origVal2 =
         case matchFunctionInScope functionFinder scope unit unitName of
            Nothing -> []
            Just functions ->
               let mapper (function,(_,package)) =
                     ( Calc_FunctionCall
                        (package,function)
                        [origVal1,origVal2]
                     , subtypeToType $ function_returnTypeData function
                     )
               in map mapper $ MapS.toList functions
       applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return []
       applyNonStatic functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return $ getNonStaticFuncs functionFinder origVal1 origVal2
            else return []
       applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return [staticVal]
       constFunctionFinder leftIn rightIn
         (Function (Designator_Operator Operators.Or)
            [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
            , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
            ] _ _) =
               let inputCheck calcType funcType = case (calcType,funcType) of
                     (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                     (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                     (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                     -- ?? need other checks
                     _ -> False
               in inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
       constFunctionFinder _ _ _ = False
       applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
       applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
         let typeCheck = case (typeData1,typeData2) of
                           -- ?? Need checks for string and bitstring
                           _ | typeData1 == typeData2 -> Just typeData1
                           _ -> Nothing
             nomatchCalcs = applyNonStatic
                              (constFunctionFinder typeData1 typeData2)
                              wrappedVal1
                              wrappedVal2
             convertCalcs valFunc arith val1 val2 =
               case typeCheck of
                  Just typeData -> applyStaticAndNonStatic
                                    (Calc_Value (valFunc $ val1 `arith` val2) typeData,typeData)
                                    (constFunctionFinder typeData1 typeData2)
                                    wrappedVal1
                                    wrappedVal2
                  Nothing -> nomatchCalcs
             enumToBool :: Enumerate -> Bool
             enumToBool (Enum_Identifier "TRUE") = True
             enumToBool (Enum_Identifier "FALSE") = False
             enumToBool (Enum_Char '1') = True
             enumToBool (Enum_Char '0') = False
         in case (val1,val2) of
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val2) -> convertCalcs boolToBoolean (.|.) (enumToBool val1) (enumToBool val2)
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val2) -> convertCalcs boolToBoolean (.|.) (enumToBool val1) (enumToBool val2)
               -- ?? Need checks for string and bitstring, arrays
               _ -> nomatchCalcs
       applyArith (calc1,typeData1) (calc2,typeData2) =
         let convertCalcs calcFunc typeData =
               applyNonStaticBuiltin
                  (constFunctionFinder typeData1 typeData2)
                  (calcFunc calc1 calc2 typeData,typeData)
                  calc1
                  calc2
         in case (typeData1,typeData2) of
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinOr typeData1
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinOr typeData1
               -- ?? Need checks for string and bitstring, arrays
               _ -> applyNonStatic
                     (constFunctionFinder typeData1 typeData2)
                     calc1
                     calc2
   allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- prevCalcs, right <- relation2]
   case concat allArith of
      emptyList | null emptyList -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_FailedExpression
      nonEmpty -> do
         contTok <- getToken
         case contTok of
            token | isKeywordOr token -> parseOrExpression staticLevel scope unit unitName $ passPosition nonEmpty contTok
            token -> do
               saveToken token
               return nonEmpty

parseXorExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> PosnWrapper [(Calculation,AllTypes)] -> ParserStack [(Calculation,AllTypes)]
parseXorExpression staticLevel scope unit unitName (PosnWrapper pos prevCalcs) = do
   relation2 <- parseRelation staticLevel scope unit unitName
   let getNonStaticFuncs functionFinder origVal1 origVal2 =
         case matchFunctionInScope functionFinder scope unit unitName of
            Nothing -> []
            Just functions ->
               let mapper (function,(_,package)) =
                     ( Calc_FunctionCall
                        (package,function)
                        [origVal1,origVal2]
                     , subtypeToType $ function_returnTypeData function
                     )
               in map mapper $ MapS.toList functions
       applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return []
       applyNonStatic functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return $ getNonStaticFuncs functionFinder origVal1 origVal2
            else return []
       applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return [staticVal]
       constFunctionFinder leftIn rightIn
         (Function (Designator_Operator Operators.Xor)
            [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
            , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
            ] _ _) =
               let inputCheck calcType funcType = case (calcType,funcType) of
                     (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                     (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                     (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                     -- ?? need other checks
                     _ -> False
               in inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
       constFunctionFinder _ _ _ = False
       applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
       applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
         let typeCheck = case (typeData1,typeData2) of
                           -- ?? Need checks for string and bitstring
                           _ | typeData1 == typeData2 -> Just typeData1
                           _ -> Nothing
             nomatchCalcs = applyNonStatic
                              (constFunctionFinder typeData1 typeData2)
                              wrappedVal1
                              wrappedVal2
             convertCalcs valFunc arith val1 val2 =
               case typeCheck of
                  Just typeData -> applyStaticAndNonStatic
                                    (Calc_Value (valFunc $ val1 `arith` val2) typeData,typeData)
                                    (constFunctionFinder typeData1 typeData2)
                                    wrappedVal1
                                    wrappedVal2
                  Nothing -> nomatchCalcs
             enumToBool :: Enumerate -> Bool
             enumToBool (Enum_Identifier "TRUE") = True
             enumToBool (Enum_Identifier "FALSE") = False
             enumToBool (Enum_Char '1') = True
             enumToBool (Enum_Char '0') = False
         in case (val1,val2) of
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val2) -> convertCalcs boolToBoolean xor (enumToBool val1) (enumToBool val2)
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val2) -> convertCalcs boolToBoolean xor (enumToBool val1) (enumToBool val2)
               -- ?? Need checks for string and bitstring, arrays
               _ -> nomatchCalcs
       applyArith (calc1,typeData1) (calc2,typeData2) =
         let convertCalcs calcFunc typeData =
               applyNonStaticBuiltin
                  (constFunctionFinder typeData1 typeData2)
                  (calcFunc calc1 calc2 typeData,typeData)
                  calc1
                  calc2
         in case (typeData1,typeData2) of
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinXor typeData1
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinXor typeData1
               -- ?? Need checks for string and bitstring, arrays
               _ -> applyNonStatic
                     (constFunctionFinder typeData1 typeData2)
                     calc1
                     calc2
   allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- prevCalcs, right <- relation2]
   case concat allArith of
      emptyList | null emptyList -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_FailedExpression
      nonEmpty -> do
         contTok <- getToken
         case contTok of
            token | isKeywordXor token -> parseXorExpression staticLevel scope unit unitName $ passPosition nonEmpty contTok
            token -> do
               saveToken token
               return nonEmpty

parseNandExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> PosnWrapper [(Calculation,AllTypes)] -> ParserStack [(Calculation,AllTypes)]
parseNandExpression staticLevel scope unit unitName (PosnWrapper pos prevCalcs) = do
   relation2 <- parseRelation staticLevel scope unit unitName
   let getNonStaticFuncs functionFinder origVal1 origVal2 =
         case matchFunctionInScope functionFinder scope unit unitName of
            Nothing -> []
            Just functions ->
               let mapper (function,(_,package)) =
                     ( Calc_FunctionCall
                        (package,function)
                        [origVal1,origVal2]
                     , subtypeToType $ function_returnTypeData function
                     )
               in map mapper $ MapS.toList functions
       applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return []
       applyNonStatic functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return $ getNonStaticFuncs functionFinder origVal1 origVal2
            else return []
       applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return [staticVal]
       constFunctionFinder leftIn rightIn
         (Function (Designator_Operator Operators.Nand)
            [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
            , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
            ] _ _) =
               let inputCheck calcType funcType = case (calcType,funcType) of
                     (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                     (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                     (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                     -- ?? need other checks
                     _ -> False
               in inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
       constFunctionFinder _ _ _ = False
       applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
       applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
         let typeCheck = case (typeData1,typeData2) of
                           -- ?? Need checks for string and bitstring
                           _ | typeData1 == typeData2 -> Just typeData1
                           _ -> Nothing
             nomatchCalcs = applyNonStatic
                              (constFunctionFinder typeData1 typeData2)
                              wrappedVal1
                              wrappedVal2
             convertCalcs valFunc arith val1 val2 =
               case typeCheck of
                  Just typeData -> applyStaticAndNonStatic
                                    (Calc_Value (valFunc $ val1 `arith` val2) typeData,typeData)
                                    (constFunctionFinder typeData1 typeData2)
                                    wrappedVal1
                                    wrappedVal2
                  Nothing -> nomatchCalcs
             enumToBool :: Enumerate -> Bool
             enumToBool (Enum_Identifier "TRUE") = True
             enumToBool (Enum_Identifier "FALSE") = False
             enumToBool (Enum_Char '1') = True
             enumToBool (Enum_Char '0') = False
         in case (val1,val2) of
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val2) -> convertCalcs boolToBoolean (\a1 a2 -> not $ a1 .&. a2) (enumToBool val1) (enumToBool val2)
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val2) -> convertCalcs boolToBoolean (\a1 a2 -> not $ a1 .&. a2) (enumToBool val1) (enumToBool val2)
               -- ?? Need checks for string and bitstring, arrays
               _ -> nomatchCalcs
       applyArith (calc1,typeData1) (calc2,typeData2) =
         let convertCalcs calcFunc typeData =
               applyNonStaticBuiltin
                  (constFunctionFinder typeData1 typeData2)
                  (calcFunc calc1 calc2 typeData,typeData)
                  calc1
                  calc2
         in case (typeData1,typeData2) of
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinNand typeData1
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinNand typeData1
               -- ?? Need checks for string and bitstring, arrays
               _ -> applyNonStatic
                     (constFunctionFinder typeData1 typeData2)
                     calc1
                     calc2
   allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- prevCalcs, right <- relation2]
   case concat allArith of
      emptyList | null emptyList -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_FailedExpression
      nonEmpty -> return nonEmpty

parseNorExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> PosnWrapper [(Calculation,AllTypes)] -> ParserStack [(Calculation,AllTypes)]
parseNorExpression staticLevel scope unit unitName (PosnWrapper pos prevCalcs) = do
   relation2 <- parseRelation staticLevel scope unit unitName
   let getNonStaticFuncs functionFinder origVal1 origVal2 =
         case matchFunctionInScope functionFinder scope unit unitName of
            Nothing -> []
            Just functions ->
               let mapper (function,(_,package)) =
                     ( Calc_FunctionCall
                        (package,function)
                        [origVal1,origVal2]
                     , subtypeToType $ function_returnTypeData function
                     )
               in map mapper $ MapS.toList functions
       applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return []
       applyNonStatic functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return $ getNonStaticFuncs functionFinder origVal1 origVal2
            else return []
       applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
         if notLocallyStatic staticLevel
            then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
            else return [staticVal]
       constFunctionFinder leftIn rightIn
         (Function (Designator_Operator Operators.Nor)
            [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
            , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
            ] _ _) =
               let inputCheck calcType funcType = case (calcType,funcType) of
                     (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                     (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                     (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                     (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                     -- ?? need other checks
                     _ -> False
               in inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
       constFunctionFinder _ _ _ = False
       applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
       applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
         let typeCheck = case (typeData1,typeData2) of
                           -- ?? Need checks for string and bitstring
                           _ | typeData1 == typeData2 -> Just typeData1
                           _ -> Nothing
             nomatchCalcs = applyNonStatic
                              (constFunctionFinder typeData1 typeData2)
                              wrappedVal1
                              wrappedVal2
             convertCalcs valFunc arith val1 val2 =
               case typeCheck of
                  Just typeData -> applyStaticAndNonStatic
                                    (Calc_Value (valFunc $ val1 `arith` val2) typeData,typeData)
                                    (constFunctionFinder typeData1 typeData2)
                                    wrappedVal1
                                    wrappedVal2
                  Nothing -> nomatchCalcs
             enumToBool :: Enumerate -> Bool
             enumToBool (Enum_Identifier "TRUE") = True
             enumToBool (Enum_Identifier "FALSE") = False
             enumToBool (Enum_Char '1') = True
             enumToBool (Enum_Char '0') = False
         in case (val1,val2) of
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") val2) -> convertCalcs boolToBoolean (\a1 a2 -> not $ a1 .|. a2) (enumToBool val1) (enumToBool val2)
               (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val1,Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") val2) -> convertCalcs boolToBoolean (\a1 a2 -> not $ a1 .|. a2) (enumToBool val1) (enumToBool val2)
               -- ?? Need checks for string and bitstring, arrays
               _ -> nomatchCalcs
       applyArith (calc1,typeData1) (calc2,typeData2) =
         let convertCalcs calcFunc typeData =
               applyNonStaticBuiltin
                  (constFunctionFinder typeData1 typeData2)
                  (calcFunc calc1 calc2 typeData,typeData)
                  calc1
                  calc2
         in case (typeData1,typeData2) of
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinNor typeData1
               (Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _),Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _)) ->
                  convertCalcs Calc_BuiltinNor typeData1
               -- ?? Need checks for string and bitstring, arrays
               _ -> applyNonStatic
                     (constFunctionFinder typeData1 typeData2)
                     calc1
                     calc2
   allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- prevCalcs, right <- relation2]
   case concat allArith of
      emptyList | null emptyList -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_FailedExpression
      nonEmpty -> return nonEmpty

data RelationalOperator = Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual

parseRelation :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parseRelation staticLevel scope unit unitName = do
   exp1 <- parseSimpleExpression staticLevel scope unit unitName
   relationTok <- getToken
   wrappedRelate <- case relationTok of
      token | isEqual token -> return $ Just Equal
      token | isInequality token -> return $ Just NotEqual
      token | isLessThan token -> return $ Just LessThan
      token | isSignAssign token -> return $ Just LessThanOrEqual
      token | isGreaterThan token -> return $ Just GreaterThan
      token | isGreaterThanOrEqual token -> return $ Just GreaterThanOrEqual
      token -> do
         saveToken token
         return Nothing
   case wrappedRelate of
      Just relation -> do
         exp2 <- parseSimpleExpression staticLevel scope unit unitName
         let getNonStaticFuncs functionFinder origVal1 origVal2 =
               case matchFunctionInScope functionFinder scope unit unitName of
                  Nothing -> []
                  Just functions ->
                     let mapper (function,(_,package)) =
                           ( Calc_FunctionCall
                              (package,function)
                              [origVal1,origVal2]
                           , subtypeToType $ function_returnTypeData function
                           )
                     in map mapper $ MapS.toList functions
             applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
                  else return []
             applyNonStatic functionFinder origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return $ getNonStaticFuncs functionFinder origVal1 origVal2
                  else return []
             applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
                  else return [staticVal]
             operatorCheck operator = case relation of
                                       Equal -> operator == Operators.Equal
                                       NotEqual -> operator == Operators.Inequality
                                       LessThan -> operator == Operators.LessThan
                                       LessThanOrEqual -> operator == Operators.Assign
                                       GreaterThan -> operator == Operators.GreaterThan
                                       GreaterThanOrEqual -> operator == Operators.GreaterThanOrEqual
             constFunctionFinder leftIn rightIn
               (Function (Designator_Operator operator)
                  [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
                  , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
                  ] _ _) =
                     let inputCheck calcType funcType = case (calcType,funcType) of
                           (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                           (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                           (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                           -- ?? need other checks
                           _ -> False
                     in operatorCheck operator && inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
             constFunctionFinder _ _ _ = False
             applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
             applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
               let typeCheck = case (typeData1,typeData2) of
                                 (Type_Type _ IntegerType,Type_UniversalInt) -> True
                                 (Type_Type _ FloatingType,Type_UniversalReal) -> True
                                 (Type_UniversalInt,Type_Type _ IntegerType) -> True
                                 (Type_UniversalReal,Type_Type _ FloatingType) -> True
                                 -- ?? Need checks for string and bitstring
                                 _ -> typeData1 == typeData2
                   nomatchCalcs = applyNonStatic
                                    (constFunctionFinder typeData1 typeData2)
                                    wrappedVal1
                                    wrappedVal2
                   convertCalcs valFunc arith val1 val2 =
                     let outputType = Type_Type
                                       (NetlistName "STD" "STANDARD","ANON'BOOLEAN")
                                       (EnumerationType
                                          [ Enum_Identifier "FALSE"
                                          , Enum_Identifier "TRUE"
                                          ]
                                       )
                     in if typeCheck
                           then applyStaticAndNonStatic
                                 (Calc_Value (valFunc $ val1 `arith` val2) outputType,outputType)
                                 (constFunctionFinder typeData1 typeData2)
                                 wrappedVal1
                                 wrappedVal2
                           else nomatchCalcs
                   enumPos (Type_Type _ (EnumerationType enums)) enum = fromJust $ elemIndex enum enums
               in case (val1,val2,relation) of
                     (Value_Enum _ val1,Value_Enum _ val2,Equal) -> convertCalcs boolToBoolean (==) val1 val2
                     (Value_Enum _ val1,Value_Enum _ val2,NotEqual) -> convertCalcs boolToBoolean (/=) val1 val2
                     (Value_Enum _ val1,Value_Enum _ val2,LessThan) -> convertCalcs boolToBoolean (<) (enumPos typeData1 val1) (enumPos typeData2 val2)
                     (Value_Enum _ val1,Value_Enum _ val2,LessThanOrEqual) -> convertCalcs boolToBoolean (<=) (enumPos typeData1 val1) (enumPos typeData2 val2)
                     (Value_Enum _ val1,Value_Enum _ val2,GreaterThan) -> convertCalcs boolToBoolean (>) (enumPos typeData1 val1) (enumPos typeData2 val2)
                     (Value_Enum _ val1,Value_Enum _ val2,GreaterThanOrEqual) -> convertCalcs boolToBoolean (>=) (enumPos typeData1 val1) (enumPos typeData2 val2)
                     (Value_Int val1,Value_Int val2,Equal) -> convertCalcs boolToBoolean (==) val1 val2
                     (Value_Int val1,Value_Int val2,NotEqual) -> convertCalcs boolToBoolean (/=) val1 val2
                     (Value_Int val1,Value_Int val2,LessThan) -> convertCalcs boolToBoolean (<) val1 val2
                     (Value_Int val1,Value_Int val2,LessThanOrEqual) -> convertCalcs boolToBoolean (<=) val1 val2
                     (Value_Int val1,Value_Int val2,GreaterThan) -> convertCalcs boolToBoolean (>) val1 val2
                     (Value_Int val1,Value_Int val2,GreaterThanOrEqual) -> convertCalcs boolToBoolean (>=) val1 val2
                     (Value_Float val1,Value_Float val2,Equal) -> convertCalcs boolToBoolean (==) val1 val2
                     (Value_Float val1,Value_Float val2,NotEqual) -> convertCalcs boolToBoolean (/=) val1 val2
                     (Value_Float val1,Value_Float val2,LessThan) -> convertCalcs boolToBoolean (<) val1 val2
                     (Value_Float val1,Value_Float val2,LessThanOrEqual) -> convertCalcs boolToBoolean (<=) val1 val2
                     (Value_Float val1,Value_Float val2,GreaterThan) -> convertCalcs boolToBoolean (>) val1 val2
                     (Value_Float val1,Value_Float val2,GreaterThanOrEqual) -> convertCalcs boolToBoolean (>=) val1 val2
                     (Value_Physical val1,Value_Physical val2,Equal) -> convertCalcs boolToBoolean (==) val1 val2
                     (Value_Physical val1,Value_Physical val2,NotEqual) -> convertCalcs boolToBoolean (/=) val1 val2
                     (Value_Physical val1,Value_Physical val2,LessThan) -> convertCalcs boolToBoolean (<) val1 val2
                     (Value_Physical val1,Value_Physical val2,LessThanOrEqual) -> convertCalcs boolToBoolean (<=) val1 val2
                     (Value_Physical val1,Value_Physical val2,GreaterThan) -> convertCalcs boolToBoolean (>) val1 val2
                     (Value_Physical val1,Value_Physical val2,GreaterThanOrEqual) -> convertCalcs boolToBoolean (>=) val1 val2
                     -- ?? Need checks for string and bitstring
                     _ -> nomatchCalcs
             applyArith (calc1,typeData1) (calc2,typeData2) =
               let convertCalcs calcFunc =
                     applyNonStaticBuiltin
                        (constFunctionFinder typeData1 typeData2)
                        ( calcFunc (calc1,typeData1) (calc2,typeData2)
                        , Type_Type
                           (NetlistName "STD" "STANDARD","ANON'BOOLEAN")
                           (EnumerationType
                              [ Enum_Identifier "FALSE"
                              , Enum_Identifier "TRUE"
                              ]
                           )
                        )
                        calc1
                        calc2
               in case (typeData1,typeData2,relation) of
                     (Type_Type typeName1 (EnumerationType _),Type_Type typeName2 (EnumerationType _),Equal) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinEqual
                     (Type_Type typeName1 (EnumerationType _),Type_Type typeName2 (EnumerationType _),NotEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinNotEqual
                     (Type_Type typeName1 (EnumerationType _),Type_Type typeName2 (EnumerationType _),LessThan) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinLessThan
                     (Type_Type typeName1 (EnumerationType _),Type_Type typeName2 (EnumerationType _),LessThanOrEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_Type typeName1 (EnumerationType _),Type_Type typeName2 (EnumerationType _),GreaterThan) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinGreaterThan
                     (Type_Type typeName1 (EnumerationType _),Type_Type typeName2 (EnumerationType _),GreaterThanOrEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_Type _ IntegerType,Type_UniversalInt,Equal) -> convertCalcs Calc_BuiltinEqual
                     (Type_Type _ IntegerType,Type_UniversalInt,NotEqual) -> convertCalcs Calc_BuiltinNotEqual
                     (Type_Type _ IntegerType,Type_UniversalInt,LessThan) -> convertCalcs Calc_BuiltinLessThan
                     (Type_Type _ IntegerType,Type_UniversalInt,LessThanOrEqual) -> convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_Type _ IntegerType,Type_UniversalInt,GreaterThan) -> convertCalcs Calc_BuiltinGreaterThan
                     (Type_Type _ IntegerType,Type_UniversalInt,GreaterThanOrEqual) -> convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_UniversalInt,Type_Type _ IntegerType,Equal) -> convertCalcs Calc_BuiltinEqual
                     (Type_UniversalInt,Type_Type _ IntegerType,NotEqual) -> convertCalcs Calc_BuiltinNotEqual
                     (Type_UniversalInt,Type_Type _ IntegerType,LessThan) -> convertCalcs Calc_BuiltinLessThan
                     (Type_UniversalInt,Type_Type _ IntegerType,LessThanOrEqual) -> convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_UniversalInt,Type_Type _ IntegerType,GreaterThan) -> convertCalcs Calc_BuiltinGreaterThan
                     (Type_UniversalInt,Type_Type _ IntegerType,GreaterThanOrEqual) -> convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,Equal) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinEqual
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,NotEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinNotEqual
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,LessThan) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinLessThan
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,LessThanOrEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,GreaterThan) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinGreaterThan
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,GreaterThanOrEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_UniversalInt,Type_UniversalInt,Equal) -> convertCalcs Calc_BuiltinEqual
                     (Type_UniversalInt,Type_UniversalInt,NotEqual) -> convertCalcs Calc_BuiltinNotEqual
                     (Type_UniversalInt,Type_UniversalInt,LessThan) -> convertCalcs Calc_BuiltinLessThan
                     (Type_UniversalInt,Type_UniversalInt,LessThanOrEqual) -> convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_UniversalInt,Type_UniversalInt,GreaterThan) -> convertCalcs Calc_BuiltinGreaterThan
                     (Type_UniversalInt,Type_UniversalInt,GreaterThanOrEqual) -> convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_Type _ FloatingType,Type_UniversalReal,Equal) -> convertCalcs Calc_BuiltinEqual
                     (Type_Type _ FloatingType,Type_UniversalReal,NotEqual) -> convertCalcs Calc_BuiltinNotEqual
                     (Type_Type _ FloatingType,Type_UniversalReal,LessThan) -> convertCalcs Calc_BuiltinLessThan
                     (Type_Type _ FloatingType,Type_UniversalReal,LessThanOrEqual) -> convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_Type _ FloatingType,Type_UniversalReal,GreaterThan) -> convertCalcs Calc_BuiltinGreaterThan
                     (Type_Type _ FloatingType,Type_UniversalReal,GreaterThanOrEqual) -> convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_UniversalReal,Type_Type _ FloatingType,Equal) -> convertCalcs Calc_BuiltinEqual
                     (Type_UniversalReal,Type_Type _ FloatingType,NotEqual) -> convertCalcs Calc_BuiltinNotEqual
                     (Type_UniversalReal,Type_Type _ FloatingType,LessThan) -> convertCalcs Calc_BuiltinLessThan
                     (Type_UniversalReal,Type_Type _ FloatingType,LessThanOrEqual) -> convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_UniversalReal,Type_Type _ FloatingType,GreaterThan) -> convertCalcs Calc_BuiltinGreaterThan
                     (Type_UniversalReal,Type_Type _ FloatingType,GreaterThanOrEqual) -> convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,Equal) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinEqual
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,NotEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinNotEqual
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,LessThan) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinLessThan
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,LessThanOrEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,GreaterThan) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinGreaterThan
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,GreaterThanOrEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_UniversalReal,Type_UniversalReal,Equal) -> convertCalcs Calc_BuiltinEqual
                     (Type_UniversalReal,Type_UniversalReal,NotEqual) -> convertCalcs Calc_BuiltinNotEqual
                     (Type_UniversalReal,Type_UniversalReal,LessThan) -> convertCalcs Calc_BuiltinLessThan
                     (Type_UniversalReal,Type_UniversalReal,LessThanOrEqual) -> convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_UniversalReal,Type_UniversalReal,GreaterThan) -> convertCalcs Calc_BuiltinGreaterThan
                     (Type_UniversalReal,Type_UniversalReal,GreaterThanOrEqual) -> convertCalcs Calc_BuiltinGreaterThanOrEqual
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),Equal) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinEqual
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),NotEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinNotEqual
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),LessThan) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinLessThan
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),LessThanOrEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinLessThanOrEqual
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),GreaterThan) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinGreaterThan
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),GreaterThanOrEqual) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinGreaterThanOrEqual
                     -- ?? Need checks for string and bitstring
                     _ -> applyNonStatic
                           (constFunctionFinder typeData1 typeData2)
                           calc1
                           calc2
         allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- exp1, right <- exp2]
         case concat allArith of
            emptyList | null emptyList -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FailedRelation relationTok
            nonEmpty -> return nonEmpty
      Nothing -> return exp1

parseSimpleExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parseSimpleExpression staticLevel scope unit unitName = do
   unaryCalc <- parseUnarySignExpression staticLevel scope unit unitName
   parseAdditionExpression staticLevel scope unit unitName unaryCalc

data UnaryOperator = Plus | Minus

parseUnarySignExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parseUnarySignExpression staticLevel scope unit unitName = do
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
       applyNonStaticBuiltin functionFinder (calc,typeData) =
         let fixedCalc = case fromJust sign of
                           Plus -> (calc,typeData)
                           Minus -> (Calc_BuiltinNegate calc typeData,typeData)
         in if notLocallyStatic staticLevel
               then return (fixedCalc:getNonStaticFuncs functionFinder calc)
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
       applyArith valComb@(Calc_Value (Value_Int val) _,typeData) =
         let staticVal = applyStatic valComb (Calc_Value (Value_Int $ -val) typeData,typeData)
         in applyStaticAndNonStatic staticVal (constIntFunctionFinder typeData) (Calc_Value (Value_Int val) typeData)
       applyArith valComb@(Calc_Value (Value_Float val) _,typeData) =
         let staticVal = applyStatic valComb (Calc_Value (Value_Float $ - val) typeData,typeData)
         in applyStaticAndNonStatic staticVal (constFloatFunctionFinder typeData) (Calc_Value (Value_Float val) typeData)
       applyArith valComb@(Calc_Value (Value_Physical val) _,typeData) =
         let staticVal = applyStatic valComb (Calc_Value (Value_Physical $ -val) typeData,typeData)
         in applyStaticAndNonStatic staticVal (constPhysFunctionFinder typeData) (Calc_Value (Value_Physical val) typeData)
       applyArith valComb@(_,typeData@(Type_Type _ IntegerType))
         ---- | isSignalCalc calc = applyNonStatic (sigIntFunctionFinder typeData) valComb
         | otherwise = applyNonStaticBuiltin (constIntFunctionFinder typeData) valComb
       applyArith valComb@(_,typeData@(Type_Type _ FloatingType))
         ---- | isSignalCalc calc = applyNonStatic (sigFloatFunctionFinder typeData) valComb
         | otherwise = applyNonStaticBuiltin (constFloatFunctionFinder typeData) valComb
       applyArith valComb@(_,typeData@(Type_Type _ (PhysicalType _ _)))
         ---- | isSignalCalc calc = applyNonStatic (sigPhysFunctionFinder typeData) valComb
         | otherwise = applyNonStaticBuiltin (constPhysFunctionFinder typeData) valComb
       applyArith valComb@(_,Type_UniversalInt)
         ---- | isSignalCalc calc = applyNonStatic (sigIntFunctionFinder typeData) valComb
         | otherwise = applyNonStaticBuiltin (constIntFunctionFinder Type_UniversalInt) valComb
       applyArith valComb@(_,Type_UniversalReal)
         ---- | isSignalCalc calc = applyNonStatic (sigFloatFunctionFinder typeData) valComb
         | otherwise = applyNonStaticBuiltin (constFloatFunctionFinder Type_UniversalReal) valComb
       -- ?? Finish patterns
       --applyArith (,Type_Type name (EnumerationType enums))
       --applyArith (,Type_Type name ArrayType bounds elemTypeName elemTypeData)
       --applyArith (,Type_String str)
       --applyArith (,Type_BitString len)
   if isNothing sign
      then return terms
      else do
         allArith <- mapM applyArith terms
         case concat allArith of
            emptyList | null emptyList -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FailedSimpleExpression signOrTermTok
            nonEmpty -> return nonEmpty

data AddingOperator = Sum | Subtract | Concat

parseAdditionExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> [(Calculation,AllTypes)] -> ParserStack [(Calculation,AllTypes)]
parseAdditionExpression staticLevel scope unit unitName prevCalcs = do
   addingOpTok <- getToken
   wrappedSign <- case addingOpTok of
            token | isPlus token -> return $ Just Sum
            token | isHyphen token -> return $ Just Subtract
            token | isAmpersand token -> return $ Just Concat
            token -> do
               saveToken token
               return Nothing
   case wrappedSign of
      Just sign -> do
         terms <- parseTerm staticLevel scope unit unitName
         let getNonStaticFuncs functionFinder origVal1 origVal2 =
               case matchFunctionInScope functionFinder scope unit unitName of
                  Nothing -> []
                  Just functions ->
                     let mapper (function,(_,package)) =
                           ( Calc_FunctionCall
                              (package,function)
                              [origVal1,origVal2]
                           , subtypeToType $ function_returnTypeData function
                           )
                     in map mapper $ MapS.toList functions
             applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
                  else return []
             applyNonStatic functionFinder origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return $ getNonStaticFuncs functionFinder origVal1 origVal2
                  else return []
             applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
                  else return [staticVal]
             operatorCheck operator = case sign of
                                       Sum -> operator == Operators.Plus
                                       Subtract -> operator == Operators.Hyphen
                                       Concat -> operator == Operators.Ampersand
             constFunctionFinder leftIn rightIn
               (Function (Designator_Operator operator)
                  [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
                  , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
                  ] _ _) =
                     let inputCheck calcType funcType = case (calcType,funcType) of
                           (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                           (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                           (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                           -- ?? need other checks
                           _ -> False
                     in operatorCheck operator && inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
             constFunctionFinder _ _ _ = False
             applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
             applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
               let typeCheck = case (typeData1,typeData2) of
                                 (Type_Type _ IntegerType,Type_UniversalInt) -> Just typeData1
                                 (Type_Type _ FloatingType,Type_UniversalReal) -> Just typeData1
                                 (Type_UniversalInt,Type_Type _ IntegerType) -> Just typeData2
                                 (Type_UniversalReal,Type_Type _ FloatingType) -> Just typeData2
                                 -- ?? Need checks for string and bitstring
                                 _ | typeData1 == typeData2 -> Just typeData1
                                 _ -> Nothing
                   nomatchCalcs = applyNonStatic
                                    (constFunctionFinder typeData1 typeData2)
                                    wrappedVal1
                                    wrappedVal2
                   convertCalcs valFunc arith val1 val2 =
                     case typeCheck of
                        Just typeData -> applyStaticAndNonStatic
                                          (Calc_Value (valFunc $ val1 `arith` val2) typeData,typeData)
                                          (constFunctionFinder typeData1 typeData2)
                                          wrappedVal1
                                          wrappedVal2
                        Nothing -> nomatchCalcs
               in case (val1,val2,sign) of
                     (Value_Int val1,Value_Int val2,Sum) ->
                        convertCalcs Value_Int (+) val1 val2
                     (Value_Int val1,Value_Int val2,Subtract) ->
                        convertCalcs Value_Int (-) val1 val2
                     (Value_Float val1,Value_Float val2,Sum) ->
                        convertCalcs Value_Float (+) val1 val2
                     (Value_Float val1,Value_Float val2,Subtract) ->
                        convertCalcs Value_Float (-) val1 val2
                     (Value_Physical val1,Value_Physical val2,Sum) ->
                        convertCalcs Value_Physical (+) val1 val2
                     (Value_Physical val1,Value_Physical val2,Subtract) ->
                        convertCalcs Value_Physical (-) val1 val2
                     -- ?? Need checks for string and bitstring
                     _ -> nomatchCalcs
             applyArith (calc1,typeData1) (calc2,typeData2) =
               let convertCalcs calcFunc typeData =
                     applyNonStaticBuiltin
                        (constFunctionFinder typeData1 typeData2)
                        (calcFunc (calc1,typeData1) (calc2,typeData2),typeData)
                        calc1
                        calc2
               in case (typeData1,typeData2,sign) of
                     (Type_Type _ IntegerType,Type_UniversalInt,Sum) -> convertCalcs Calc_BuiltinSum typeData1
                     (Type_Type _ IntegerType,Type_UniversalInt,Subtract) -> convertCalcs Calc_BuiltinSubtract typeData1
                     (Type_UniversalInt,Type_Type _ IntegerType,Sum) -> convertCalcs Calc_BuiltinSum typeData2
                     (Type_UniversalInt,Type_Type _ IntegerType,Subtract) -> convertCalcs Calc_BuiltinSubtract typeData2
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,Sum) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinSum typeData1
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,Subtract) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinSubtract typeData1
                     (Type_UniversalInt,Type_UniversalInt,Sum) -> convertCalcs Calc_BuiltinSum Type_UniversalInt
                     (Type_UniversalInt,Type_UniversalInt,Subtract) -> convertCalcs Calc_BuiltinSubtract Type_UniversalInt
                     (Type_Type _ FloatingType,Type_UniversalReal,Sum) -> convertCalcs Calc_BuiltinSum typeData1
                     (Type_Type _ FloatingType,Type_UniversalReal,Subtract) -> convertCalcs Calc_BuiltinSubtract typeData1
                     (Type_UniversalReal,Type_Type _ FloatingType,Sum) -> convertCalcs Calc_BuiltinSum typeData2
                     (Type_UniversalReal,Type_Type _ FloatingType,Subtract) -> convertCalcs Calc_BuiltinSubtract typeData2
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,Sum) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinSum typeData1
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,Subtract) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinSubtract typeData1
                     (Type_UniversalReal,Type_UniversalReal,Sum) -> convertCalcs Calc_BuiltinSum Type_UniversalReal
                     (Type_UniversalReal,Type_UniversalReal,Subtract) -> convertCalcs Calc_BuiltinSubtract Type_UniversalReal
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),Sum) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinSum typeData1
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),Subtract) | typeName1 == typeName2 ->
                        convertCalcs Calc_BuiltinSubtract typeData1
                     -- ?? Need checks for string and bitstring
                     _ -> applyNonStatic
                           (constFunctionFinder typeData1 typeData2)
                           calc1
                           calc2
         allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- prevCalcs, right <- terms]
         case concat allArith of
            emptyList | null emptyList -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FailedSimpleExpression addingOpTok
            nonEmpty -> parseAdditionExpression staticLevel scope unit unitName nonEmpty
      Nothing -> return prevCalcs

data MultiplyingOperator = Mult | Div | Mod | Rem

parseTerm :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parseTerm staticLevel scope unit unitName = do
   factor <- parseFactor staticLevel scope unit unitName
   parseMultiplicationExpression staticLevel scope unit unitName factor

parseMultiplicationExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> [(Calculation,AllTypes)] -> ParserStack [(Calculation,AllTypes)]
parseMultiplicationExpression staticLevel scope unit unitName prevCalcs = do
   multTok <- getToken
   wrappedOp <- case multTok of
                  token | isStar token -> return $ Just Mult
                  token | isSlash token -> return $ Just Div
                  token | isKeywordMod token -> return $ Just Mod
                  token | isKeywordRem token -> return $ Just Rem
                  token -> do
                     saveToken token
                     return Nothing
   case wrappedOp of
      Just mult -> do
         factors <- parseFactor staticLevel scope unit unitName
         let getNonStaticFuncs functionFinder origVal1 origVal2 =
               case matchFunctionInScope functionFinder scope unit unitName of
                  Nothing -> []
                  Just functions ->
                     let mapper (function,(_,package)) =
                           ( Calc_FunctionCall
                              (package,function)
                              [origVal1,origVal2]
                           , subtypeToType $ function_returnTypeData function
                           )
                     in map mapper $ MapS.toList functions
             applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
                  else return []
             applyNonStatic functionFinder origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return $ getNonStaticFuncs functionFinder origVal1 origVal2
                  else return []
             applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
               if notLocallyStatic staticLevel
                  then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
                  else return [staticVal]
             operatorCheck operator = case mult of
                                       Mult -> operator == Operators.Star
                                       Div -> operator == Operators.Slash
                                       Mod -> operator == Operators.Mod
                                       Rem -> operator == Operators.Rem
             constFunctionFinder leftIn rightIn
               (Function (Designator_Operator operator)
                  [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
                  , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
                  ] _ _) =
                     let inputCheck calcType funcType = case (calcType,funcType) of
                           (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                           (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                           (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                           -- ?? need other checks
                           _ -> False
                     in operatorCheck operator && inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
             constFunctionFinder _ _ _ = False
             applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
             applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
               let typeCheck = case (typeData1,typeData2) of
                                 (Type_Type _ IntegerType,Type_UniversalInt) -> Just typeData1
                                 (Type_Type _ FloatingType,Type_UniversalReal) -> Just typeData1
                                 (Type_UniversalInt,Type_Type _ IntegerType) -> Just typeData2
                                 (Type_UniversalReal,Type_Type _ FloatingType) -> Just typeData2
                                 (Type_Type _ (PhysicalType _ _),Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> Just typeData1
                                 (Type_Type _ (PhysicalType _ _),Type_Type (NetlistName "STD" "STANDARD","ANON'REAL") FloatingType) -> Just typeData1
                                 (Type_Type _ (PhysicalType _ _),Type_UniversalInt) -> Just typeData1
                                 (Type_Type _ (PhysicalType _ _),Type_UniversalReal) -> Just typeData1
                                 (Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType,Type_Type _ (PhysicalType _ _)) -> Just typeData2
                                 (Type_Type (NetlistName "STD" "STANDARD","ANON'REAL") FloatingType,Type_Type _ (PhysicalType _ _)) -> Just typeData2
                                 (Type_UniversalInt,Type_Type _ (PhysicalType _ _)) -> Just typeData2
                                 (Type_UniversalReal,Type_Type _ (PhysicalType _ _)) -> Just typeData2
                                 (phys1@(Type_Type _ (PhysicalType _ _)),phys2@(Type_Type _ (PhysicalType _ _))) | phys1 == phys2 -> Just Type_UniversalInt
                                 -- ?? Need checks for string and bitstring
                                 _ | typeData1 == typeData2 -> Just typeData1
                                 _ -> Nothing
                   nomatchCalcs = applyNonStatic
                                    (constFunctionFinder typeData1 typeData2)
                                    wrappedVal1
                                    wrappedVal2
                   convertCalcs valFunc arith val1 val2 =
                     case typeCheck of
                        Just typeData -> applyStaticAndNonStatic
                                          (Calc_Value (valFunc $ val1 `arith` val2) typeData,typeData)
                                          (constFunctionFinder typeData1 typeData2)
                                          wrappedVal1
                                          wrappedVal2
                        Nothing -> nomatchCalcs
               in case (val1,val2,mult) of
                     (Value_Int val1,Value_Int val2,Mult) -> convertCalcs Value_Int (*) val1 val2
                     (Value_Int val1,Value_Int val2,Div) -> convertCalcs Value_Int div val1 val2
                     (Value_Int val1,Value_Int val2,Mod) -> convertCalcs Value_Int mod val1 val2
                     (Value_Int val1,Value_Int val2,Rem) -> convertCalcs Value_Int rem val1 val2
                     (Value_Float val1,Value_Float val2,Mult) -> convertCalcs Value_Float (*) val1 val2
                     (Value_Float val1,Value_Float val2,Div) -> convertCalcs Value_Float (/) val1 val2
                     (Value_Physical val1,Value_Int val2,Mult) -> convertCalcs Value_Physical (*) val1 val2
                     (Value_Physical val1,Value_Float val2,Mult) -> convertCalcs Value_Physical (\a1 a2 -> round $ a1 * a2) (fromIntegral val1) val2
                     (Value_Int val1,Value_Physical val2,Mult) -> convertCalcs Value_Physical (*) val1 val2
                     (Value_Float val1,Value_Physical val2,Mult) -> convertCalcs Value_Physical (\a1 a2 -> round $ a1 * a2) val1 (fromIntegral val2)
                     (Value_Physical val1,Value_Int val2,Div) -> convertCalcs Value_Physical div val1 val2
                     (Value_Physical val1,Value_Float val2,Div) -> convertCalcs Value_Physical (\a1 a2 -> round $ a1 / a2) (fromIntegral val1) val2
                     (Value_Physical val1,Value_Physical val2,Div) -> convertCalcs Value_Int div val1 val2
                     -- ?? Need checks for string and bitstring
                     _ -> nomatchCalcs
             applyArith (calc1,typeData1) (calc2,typeData2) =
               let convertCalcs argsFunc calcFunc typeData =
                     applyNonStaticBuiltin
                        (constFunctionFinder typeData1 typeData2)
                        (argsFunc calcFunc typeData,typeData)
                        calc1
                        calc2
                   singleArgsFunc calcFunc typeData = calcFunc calc1 calc2 typeData
                   dualArgsFunc calcFunc _ = calcFunc (calc1,typeData1) (calc1,typeData2)
               in case (typeData1,typeData2,mult) of
                     (Type_Type _ IntegerType,Type_UniversalInt,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData1
                     (Type_Type _ IntegerType,Type_UniversalInt,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv typeData1
                     (Type_Type _ IntegerType,Type_UniversalInt,Mod) -> convertCalcs singleArgsFunc Calc_BuiltinMod typeData1
                     (Type_Type _ IntegerType,Type_UniversalInt,Rem) -> convertCalcs singleArgsFunc Calc_BuiltinRem typeData1
                     (Type_UniversalInt,Type_Type _ IntegerType,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData2
                     (Type_UniversalInt,Type_Type _ IntegerType,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv typeData2
                     (Type_UniversalInt,Type_Type _ IntegerType,Mod) -> convertCalcs singleArgsFunc Calc_BuiltinMod typeData2
                     (Type_UniversalInt,Type_Type _ IntegerType,Rem) -> convertCalcs singleArgsFunc Calc_BuiltinRem typeData2
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,Mult) | typeName1 == typeName2 ->
                        convertCalcs dualArgsFunc Calc_BuiltinMult typeData1
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,Div) | typeName1 == typeName2 ->
                        convertCalcs dualArgsFunc Calc_BuiltinDiv typeData1
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,Mod) | typeName1 == typeName2 ->
                        convertCalcs singleArgsFunc Calc_BuiltinMod typeData1
                     (Type_Type typeName1 IntegerType,Type_Type typeName2 IntegerType,Rem) | typeName1 == typeName2 ->
                        convertCalcs singleArgsFunc Calc_BuiltinRem typeData1
                     (Type_UniversalInt,Type_UniversalInt,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult Type_UniversalInt
                     (Type_UniversalInt,Type_UniversalInt,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv Type_UniversalInt
                     (Type_UniversalInt,Type_UniversalInt,Mod) -> convertCalcs singleArgsFunc Calc_BuiltinMod Type_UniversalInt
                     (Type_UniversalInt,Type_UniversalInt,Rem) -> convertCalcs singleArgsFunc Calc_BuiltinRem Type_UniversalInt
                     (Type_Type _ FloatingType,Type_UniversalReal,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData1
                     (Type_Type _ FloatingType,Type_UniversalReal,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv typeData1
                     (Type_UniversalReal,Type_Type _ FloatingType,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData2
                     (Type_UniversalReal,Type_Type _ FloatingType,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv typeData2
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,Mult) | typeName1 == typeName2 ->
                        convertCalcs dualArgsFunc Calc_BuiltinMult typeData1
                     (Type_Type typeName1 FloatingType,Type_Type typeName2 FloatingType,Div) | typeName1 == typeName2 ->
                        convertCalcs dualArgsFunc Calc_BuiltinDiv typeData1
                     (Type_UniversalReal,Type_UniversalReal,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult Type_UniversalReal
                     (Type_UniversalReal,Type_UniversalReal,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv Type_UniversalReal
                     (Type_Type _ (PhysicalType _ _),Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData1
                     (Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType,Type_Type _ (PhysicalType _ _),Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData2
                     (Type_Type _ (PhysicalType _ _),Type_UniversalInt,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData1
                     (Type_UniversalInt,Type_Type _ (PhysicalType _ _),Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData2
                     (Type_Type _ (PhysicalType _ _),Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv typeData1
                     (Type_Type _ (PhysicalType _ _),Type_UniversalInt,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv typeData1
                     (Type_Type _ (PhysicalType _ _),Type_Type (NetlistName "STD" "STANDARD","ANON'REAL") FloatingType,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData1
                     (Type_Type (NetlistName "STD" "STANDARD","ANON'REAL") FloatingType,Type_Type _ (PhysicalType _ _),Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData2
                     (Type_Type _ (PhysicalType _ _),Type_UniversalReal,Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData1
                     (Type_UniversalReal,Type_Type _ (PhysicalType _ _),Mult) -> convertCalcs dualArgsFunc Calc_BuiltinMult typeData2
                     (Type_Type _ (PhysicalType _ _),Type_Type (NetlistName "STD" "STANDARD","ANON'REAL") FloatingType,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv typeData1
                     (Type_Type _ (PhysicalType _ _),Type_UniversalReal,Div) -> convertCalcs dualArgsFunc Calc_BuiltinDiv typeData1
                     (Type_Type typeName1 (PhysicalType _ _),Type_Type typeName2 (PhysicalType _ _),Div) | typeName1 == typeName2 ->
                        convertCalcs dualArgsFunc Calc_BuiltinDiv Type_UniversalInt
                     -- ?? Need checks for string and bitstring
                     _ -> applyNonStatic
                           (constFunctionFinder typeData1 typeData2)
                           calc1
                           calc2
         allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- prevCalcs, right <- factors]
         case concat allArith of
            emptyList | null emptyList -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FailedTerm multTok
            nonEmpty -> parseMultiplicationExpression staticLevel scope unit unitName nonEmpty
      Nothing -> return prevCalcs

parseFactor :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]
parseFactor staticLevel scope unit unitName = do
   token <- getToken
   case token of
      _ | isKeywordAbs token -> do
         primary <- parsePrimary staticLevel scope unit unitName
         let getNonStaticFuncs functionFinder origVal =
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
             applyNonStaticBuiltin functionFinder (calc,typeData) =
               let fixedCalc = (Calc_BuiltinAbs calc typeData,typeData)
               in if notLocallyStatic staticLevel
                     then return (fixedCalc:getNonStaticFuncs functionFinder calc)
                     else return []
             applyNonStatic functionFinder origVal =
               if notLocallyStatic staticLevel
                  then return $ getNonStaticFuncs functionFinder origVal
                  else return []
             applyStaticAndNonStatic staticVal functionFinder origVal =
               if notLocallyStatic staticLevel
                  then return (staticVal:getNonStaticFuncs functionFinder origVal)
                  else return [staticVal]
             constFunctionFinder inputType
               (Function (Designator_Operator Operators.Abs)
                  [ FunctionInterface FunctionInterfaceType_Constant _ _ inputSubtype
                  ] _ _) =
                     let inputCheck calcType funcType = case (calcType,funcType) of
                           (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                           (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                           (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                           -- ?? need other checks
                           _ -> False
                     in inputCheck inputType inputSubtype
             constFunctionFinder _ _ = False
             applyArith :: (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
             applyArith (wrappedVal@(Calc_Value val _),typeData) =
               let typeCheck = case typeData of
                                 Type_Type _ IntegerType -> True
                                 Type_Type _ FloatingType -> True
                                 Type_Type _ (PhysicalType _ _) -> True
                                 Type_UniversalInt -> True
                                 Type_UniversalReal -> True
                                 _ -> False
                   nomatchCalcs = applyNonStatic
                                    (constFunctionFinder typeData)
                                    wrappedVal
                   convertCalcs newCalc =
                     if typeCheck
                        then applyStaticAndNonStatic
                              (newCalc,typeData)
                              (constFunctionFinder typeData)
                              wrappedVal
                        else nomatchCalcs
               in case val of
                     Value_Int val -> convertCalcs $ Calc_Value (Value_Int $ abs val) typeData
                     Value_Float val -> convertCalcs $ Calc_Value (Value_Float $ abs val) typeData
                     Value_Physical val -> convertCalcs $ Calc_Value (Value_Physical $ abs val) typeData
                     _ -> nomatchCalcs
             applyArith (calc,typeData) =
               let convertCalcs =
                     applyNonStaticBuiltin
                        (constFunctionFinder typeData)
                        (calc,typeData)
               in case typeData of
                     Type_Type _ IntegerType -> convertCalcs
                     Type_Type _ FloatingType -> convertCalcs
                     Type_Type _ (PhysicalType _ _) -> convertCalcs
                     Type_UniversalInt -> convertCalcs
                     Type_UniversalReal -> convertCalcs
                     _ -> applyNonStatic
                           (constFunctionFinder typeData)
                           calc
         allArith <- mapM applyArith primary
         case concat allArith of
            emptyList | null emptyList -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FailedFactor token
            nonEmpty -> return nonEmpty
      _ | isKeywordNot token -> do
         primary <- parsePrimary staticLevel scope unit unitName
         let getNonStaticFuncs functionFinder origVal =
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
             applyNonStaticBuiltin functionFinder (calc,typeData) =
               let fixedCalc = (Calc_BuiltinNot calc typeData,typeData)
               in if notLocallyStatic staticLevel
                     then return (fixedCalc:getNonStaticFuncs functionFinder calc)
                     else return []
             applyNonStatic functionFinder origVal =
               if notLocallyStatic staticLevel
                  then return $ getNonStaticFuncs functionFinder origVal
                  else return []
             applyStaticAndNonStatic staticVal functionFinder origVal =
               if notLocallyStatic staticLevel
                  then return (staticVal:getNonStaticFuncs functionFinder origVal)
                  else return [staticVal]
             constFunctionFinder inputType
               (Function (Designator_Operator Operators.Not)
                  [ FunctionInterface FunctionInterfaceType_Constant _ _ inputSubtype
                  ] _ _) =
                     let inputCheck calcType funcType = case (calcType,funcType) of
                           (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                           (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                           (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                           (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                           -- ?? need other checks
                           _ -> False
                     in inputCheck inputType inputSubtype
             constFunctionFinder _ _ = False
             applyArith :: (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
             applyArith (wrappedVal@(Calc_Value val _),typeData) =
               let typeCheck = case typeData of
                                 Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _) -> True
                                 Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _) -> True
                                 -- ?? Need 1D arrays of BIT and BOOLEAN
                                 _ -> False
                   nomatchCalcs = applyNonStatic
                                    (constFunctionFinder typeData)
                                    wrappedVal
                   convertCalcs newCalc =
                     if typeCheck
                        then applyStaticAndNonStatic
                              (newCalc,typeData)
                              (constFunctionFinder typeData)
                              wrappedVal
                        else nomatchCalcs
               in case val of
                     (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") (Enum_Char '0')) -> convertCalcs $ Calc_Value (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") $ Enum_Char '1') typeData
                     (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") (Enum_Char '1')) -> convertCalcs $ Calc_Value (Value_Enum (NetlistName "STD" "STANDARD","ANON'BIT") $ Enum_Char '0') typeData
                     (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (Enum_Identifier "FALSE")) -> convertCalcs $ Calc_Value (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") $ Enum_Identifier "TRUE") typeData
                     (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (Enum_Identifier "TRUE")) -> convertCalcs $ Calc_Value (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") $ Enum_Identifier "FALSE") typeData
                     -- ?? Need checks for bit string, bit and boolean arrays
                     _ -> nomatchCalcs
             applyArith (calc,typeData) =
               let convertCalcs =
                     applyNonStaticBuiltin
                        (constFunctionFinder typeData)
                        (calc,typeData)
               in case typeData of
                     Type_Type (NetlistName "STD" "STANDARD","ANON'BIT") (EnumerationType _) -> convertCalcs
                     Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") (EnumerationType _) -> convertCalcs
                     -- ?? Need checks for bit string, bit and boolean arrays
                     _ -> applyNonStatic
                           (constFunctionFinder typeData)
                           calc
         allArith <- mapM applyArith primary
         case concat allArith of
            emptyList | null emptyList -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FailedFactor token
            nonEmpty -> return nonEmpty
      _ -> do
         saveToken token
         primary1 <- parsePrimary staticLevel scope unit unitName
         middleToken <- getToken
         if isDoubleStar middleToken
            then do
               primary2 <- parsePrimary staticLevel scope unit unitName
               let getNonStaticFuncs functionFinder origVal1 origVal2 =
                     case matchFunctionInScope functionFinder scope unit unitName of
                        Nothing -> []
                        Just functions ->
                           let mapper (function,(_,package)) =
                                 ( Calc_FunctionCall
                                    (package,function)
                                    [origVal1,origVal2]
                                 , subtypeToType $ function_returnTypeData function
                                 )
                           in map mapper $ MapS.toList functions
                   applyNonStaticBuiltin functionFinder builtinCalc origVal1 origVal2 =
                     if notLocallyStatic staticLevel
                        then return (builtinCalc:getNonStaticFuncs functionFinder origVal1 origVal2)
                        else return []
                   applyNonStatic functionFinder origVal1 origVal2 =
                     if notLocallyStatic staticLevel
                        then return $ getNonStaticFuncs functionFinder origVal1 origVal2
                        else return []
                   applyStaticAndNonStatic staticVal functionFinder origVal1 origVal2 =
                     if notLocallyStatic staticLevel
                        then return (staticVal:getNonStaticFuncs functionFinder origVal1 origVal2)
                        else return [staticVal]
                   constFunctionFinder leftIn rightIn
                     (Function (Designator_Operator Operators.DoubleStar)
                        [ FunctionInterface FunctionInterfaceType_Constant _ _ leftSubtype
                        , FunctionInterface FunctionInterfaceType_Constant _ _ rightSubtype
                        ] _ _) =
                           let inputCheck calcType funcType = case (calcType,funcType) of
                                 (Type_Type typeName IntegerType,IntegerSubtype _ baseTypeName _) -> typeName == baseTypeName
                                 (Type_UniversalInt,IntegerSubtype _ _ _) -> True
                                 (Type_Type typeName FloatingType,FloatingSubtype _ baseTypeName _) -> typeName == baseTypeName
                                 (Type_UniversalReal,FloatingSubtype _ _ _) -> True
                                 (Type_Type typeName (PhysicalType _ _),PhysicalSubtype _ baseTypeName _ _ _) -> typeName == baseTypeName
                                 -- ?? need other checks
                                 _ -> False
                           in inputCheck leftIn leftSubtype && inputCheck rightIn rightSubtype
                   constFunctionFinder _ _ _ = False
                   applyArith :: (Calculation,AllTypes) -> (Calculation,AllTypes) -> ParserStack [(Calculation,AllTypes)]
                   applyArith (wrappedVal1@(Calc_Value val1 _),typeData1) (wrappedVal2@(Calc_Value val2 _),typeData2) =
                     let typeCheck = case (typeData1,typeData2) of
                                       (Type_Type _ IntegerType,Type_UniversalInt) -> Just typeData1
                                       (Type_Type _ IntegerType,Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> Just typeData1
                                       (Type_UniversalInt,Type_UniversalInt) -> Just typeData1
                                       (Type_UniversalInt,Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> Just typeData1
                                       (Type_Type _ FloatingType,Type_UniversalInt) -> Just typeData1
                                       (Type_Type _ FloatingType,Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> Just typeData1
                                       (Type_UniversalReal,Type_UniversalInt) -> Just typeData1
                                       (Type_UniversalReal,Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> Just typeData1
                                       _ -> Nothing
                         nomatchCalcs = applyNonStatic
                                          (constFunctionFinder typeData1 typeData2)
                                          wrappedVal1
                                          wrappedVal2
                         convertCalcs valFunc arith val1 val2 =
                           case typeCheck of
                              Just typeData -> applyStaticAndNonStatic
                                                (Calc_Value (valFunc $ val1 `arith` val2) typeData,typeData)
                                                (constFunctionFinder typeData1 typeData2)
                                                wrappedVal1
                                                wrappedVal2
                              Nothing -> nomatchCalcs
                     in case (val1,val2) of
                           (Value_Int val1,Value_Int val2) -> convertCalcs Value_Int (\a1 a2 -> round $ a1 ^^ a2) (fromInteger val1) val2
                           (Value_Float val1,Value_Int val2) -> convertCalcs Value_Float (^^) val1 val2
                           -- ?? Need checks for string and bitstring
                           _ -> nomatchCalcs
                   applyArith (calc1,typeData1) (calc2,typeData2) =
                     let convertCalcs calcFunc typeData =
                           applyNonStaticBuiltin
                              (constFunctionFinder typeData1 typeData2)
                              (calcFunc (calc1,typeData1) (calc2,typeData2),typeData)
                              calc1
                              calc2
                     in case (typeData1,typeData2) of
                           (Type_Type _ IntegerType,Type_UniversalInt) -> convertCalcs Calc_BuiltinExp typeData1
                           (Type_Type _ IntegerType,Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> convertCalcs Calc_BuiltinExp typeData1
                           (Type_UniversalInt,Type_UniversalInt) -> convertCalcs Calc_BuiltinExp typeData1
                           (Type_UniversalInt,Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> convertCalcs Calc_BuiltinExp typeData1
                           (Type_Type _ FloatingType,Type_UniversalInt) -> convertCalcs Calc_BuiltinExp typeData1
                           (Type_Type _ FloatingType,Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> convertCalcs Calc_BuiltinExp typeData1
                           (Type_UniversalReal,Type_UniversalInt) -> convertCalcs Calc_BuiltinExp typeData1
                           (Type_UniversalReal,Type_Type (NetlistName "STD" "STANDARD","ANON'INTEGER") IntegerType) -> convertCalcs Calc_BuiltinExp typeData1
                           -- ?? Need checks for string and bitstring
                           _ -> applyNonStatic
                                 (constFunctionFinder typeData1 typeData2)
                                 calc1
                                 calc2
               allArith <- mapM (\(left,right) -> applyArith left right) $ [(left,right) | left <- primary1, right <- primary2]
               case concat allArith of
                  emptyList | null emptyList -> throwError $ ConverterError_Netlist $ passPosition NetlistError_FailedFactor token
                  nonEmpty -> return nonEmpty
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
               return [(Calc_Value (Value_Int $ toInteger intVal) Type_UniversalInt,Type_UniversalInt)]
            Just (PosnWrapper pos potUnitName) ->
               let upperPotUnitName = map toUpper potUnitName
               in case matchPhysicalUnitInScope scope unit unitName upperPotUnitName of
                     Just ((typeName,typeData@(PhysicalType baseUnit otherUnits)),packageName) ->
                        let integerVal = toInteger intVal
                            value = if upperPotUnitName == baseUnit
                                       then integerVal
                                       else (toInteger $ otherUnits MapS.! upperPotUnitName) * integerVal
                            outputType = Type_Type (packageName,typeName) typeData
                        in return [(Calc_Value (Value_Physical value) outputType,outputType)]
                     Nothing -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_PhysicalUnitNotFound potUnitName
      token@(PosnWrapper _ (Tokens.Literal (Tokens.Univ_Real realVal))) -> do
         potentialUnit <- getToken
         case matchIdentifier potentialUnit of
            Nothing -> do
               saveToken potentialUnit
               return [(Calc_Value (Value_Float realVal) Type_UniversalReal,Type_UniversalReal)]
            Just (PosnWrapper pos potUnitName) ->
               let upperPotUnitName = map toUpper potUnitName
               in case matchPhysicalUnitInScope scope unit unitName upperPotUnitName of
                     Just ((typeName,typeData@(PhysicalType baseUnit otherUnits)),packageName) ->
                        let value = if upperPotUnitName == baseUnit -- ?? round or error
                                       then round realVal
                                       else round $ (fromIntegral $ otherUnits MapS.! upperPotUnitName) * realVal
                            outputType = Type_Type (packageName,typeName) typeData
                        in return [(Calc_Value (Value_Physical value) outputType,outputType)]
                     Nothing -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_PhysicalUnitNotFound potUnitName
      token@(PosnWrapper _ (Tokens.Literal (Tokens.Character char))) ->
         case matchEnumCharInScope char scope unit unitName of
            Just enumMap ->
               MapS.toList enumMap
               & map (\(typeName,(typ,packageName)) -> (Calc_Value (Value_Enum (packageName,typeName) $ Enum_Char char) (Type_Type (packageName,typeName) typ),Type_Type (packageName,typeName) typ))
               & return
            Nothing -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_UnrecognisedEnumChar char) token
--      _ | isStr -- function call or array
--      _ | isBitstr token -> -- array of bits
      _ | isKeywordNull token -> throwError $ ConverterError_NotImplemented $ passPosition "Null literal" token
      _ | isKeywordNew token -> throwError $ ConverterError_NotImplemented $ passPosition "Primary allocator" token
      _ | isLeftParen token -> do -- ?? or aggregate
            expression <- parseExpression staticLevel scope unit unitName
            endToken <- getToken
            if isRightParen endToken
               then return expression
               else throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedRightParenInPrimary endToken

-- ?? Currently ignoring expanded names
parsePrimaryIdentifier :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> PosnWrapper String -> ParserStack [(Calculation,AllTypes)]
parsePrimaryIdentifier staticLevel scope unit unitName (PosnWrapper pos iden) =
   let upperIden = map toUpper iden
       --checkTypes =
       --  case matchTypeNameInScope scope unit unitName upperIden of
       --     Just (typ,netlistName) -> do
       --        nextToken <- getToken
       --        case nextToken of
       --           _ | isLeftParen nextToken -> -- ?? type conversion
       --              throwError $ ConverterError_NotImplemented $ passPosition "Explicit type conversions" nextToken
       --           _ | isApostrophe nextToken -> -- qualified expressions, attributes
       --              reviewToken <- getToken
       --              case matchIdentifier reviewToken of
       --                 Just wrappedAttributeName ->
       --                    parseTypeAttribute staticLevel scope unit typ netlistName upperIden wrappedAttributeName
       --                 Nothing | isLeftParen reviewToken -> -- qualified expressions
       --                    throwError $ ConverterError_NotImplemented $ passPosition "Qualified type expressions" reviewToken
       --                 Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedAttrOrTypeConv reviewToken
       --           _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedTypeExpression nextToken
       --     Nothing -> checkSubtypes
       checkSubtypes =
         case matchSubtypeNameInScope scope unit unitName upperIden of
            Just (subtype,netlistName) -> do
               nextTok <- getToken
               case nextTok of
                  token | isApostrophe token -> do
                     potentialLeftParenTok <- getToken
                     case potentialLeftParenTok of
                        token | isLeftParen token -> parseQualifiedExpression staticLevel scope unit unitName (netlistName,upperIden) subtype $ getPos token
                        token -> do
                           saveToken token
                           parseSubtypeAttribute staticLevel scope unit unitName (netlistName,upperIden) subtype
                  token | isLeftParen token -> parseTypeConversion staticLevel scope unit unitName (netlistName,upperIden) subtype $ getPos token
                  token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedQualExpOrTypeConvOrAttrAfterTypeNamePrimary token
            Nothing -> checkFunctions
       checkFunctions =
         case matchFunctionNameInScope scope unit unitName upperIden of
            Just funcMap ->
               throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Function call in expression"
               --readFuncCall scope unit funcMap (PosnWrapper pos upperIden)
            Nothing -> checkEnums
       checkEnums =
         case matchEnumNameInScope upperIden scope unit unitName of
            Just enumMap ->
               MapS.toList enumMap
               & map (\(typeName,(typ,packageName)) -> (Calc_Value (Value_Enum (packageName,typeName) $ Enum_Identifier upperIden) (Type_Type (packageName,typeName) typ),Type_Type (packageName,typeName) typ))
               & return
            Nothing -> checkConstants
       checkConstants =
         case matchConstantNameInScope scope unit unitName upperIden of -- ?? need attributes, indexed
            Just (Constant _ subtype Nothing,constPackage) | notLocallyStatic staticLevel -> return $ [(Calc_Const (constPackage,upperIden),subtypeToType subtype)]
            Just (Constant _ _ Nothing,_) -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_DeferredConst upperIden
            Just (Constant _ subtype (Just val),_) -> return [(Calc_Value val $ subtypeToType subtype,subtypeToType subtype)]
            Nothing -> checkSignals
       checkSignals =
         case matchSignalNameInScope scope unit upperIden of
            Just (sig,packageName) | isNotStatic staticLevel -> parseSignalName scope unit unitName (packageName,upperIden) sig InternalSignal
            Just _ -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_SignalNameInStaticExpression upperIden
            Nothing -> checkGenerics
       checkGenerics =
         case matchGenericNameInScope unit upperIden of
            Just generic | not $ isLocallyStatic staticLevel ->
               let typeData = generic_subtypeData generic
                   allType = subtypeToType typeData
                   value = generic_default generic
                   calc = case value of
                           Just value -> Calc_Value value allType
                           Nothing -> Calc_Generic upperIden
               in return [(calc,allType)]
            Nothing -> checkPorts
       checkPorts =
         case matchPortNameInScope unit upperIden of
            Just port | isNotStatic staticLevel -> do
               let portName = (Nothing,upperIden)
               portType <- case port_mode port of
                  Mode_Out -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_CannotReadOutputPort upperIden
                  Mode_In -> return PortIn
                  _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Other port mode types in primary"
               let portAsSignal = Signal (port_subtypeName port) (port_subtypeData port) (port_default port)
               parseSignalName scope unit unitName portName portAsSignal portType
            Nothing -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_UnrecognisedName upperIden
   in checkSubtypes

-- ?? Note: Aggregates not yet parsed
parseQualifiedExpression :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> AlexPosn -> ParserStack [(Calculation,AllTypes)]
parseQualifiedExpression staticLevel scope unit unitName subtypeName subtypeData pos = do
   calc <- parseWithExpectedType staticLevel parseExpression scope unit unitName subtypeName subtypeData pos
   rightParenTok <- getToken
   unless (isRightParen rightParenTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedRightParenInQualifiedExpression rightParenTok
   let (typeName,typeData) = case subtypeData of
         EnumerationSubtype _ typeName enums _ -> (typeName,EnumerationType enums)
         IntegerSubtype _ typeName _ -> (typeName,IntegerType)
         FloatingSubtype _ typeName _ -> (typeName,FloatingType)
         PhysicalSubtype _ typeName baseUnit otherUnits _ -> (typeName,PhysicalType baseUnit otherUnits)
   return [(calc,Type_Type typeName typeData)]

parseTypeConversion :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> AlexPosn -> ParserStack [(Calculation,AllTypes)]
parseTypeConversion staticLevel scope unit unitName subtypeName subtypeData pos = do
   expressions <- parseExpression staticLevel scope unit unitName
   rightParenTok <- getToken
   unless (isRightParen rightParenTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedRightParenInTypeConversion rightParenTok
   let convertType :: (Calculation,AllTypes) -> Maybe (Calculation,AllTypes)
       convertType calcPair@(Calc_Value (Value_Enum typeName1 enum) _,_) =
         case subtypeData of
            EnumerationSubtype _ typeName2 enums range | typeName1 == typeName2 && valueInEnumRange enum enums range -> Just calcPair
            _ -> Nothing
       convertType (Calc_Value (Value_Int int) _,_) =
         case subtypeData of
            IntegerSubtype _ typeName constraint | valueInIntRange int constraint ->
               let typeData = Type_Type typeName IntegerType
               in Just (Calc_Value (Value_Int int) typeData,typeData)
            --IntegerSubtype _ typeName constraint -> Nothing
            FloatingSubtype _ typeName constraint | valueInFloatRange (fromInteger int) constraint ->
               let typeData = Type_Type typeName FloatingType
               in Just (Calc_Value (Value_Float $ fromInteger int) typeData,typeData)
            --FloatingSubtype _ typeName constraint -> Nothing
            _ -> Nothing
       convertType (Calc_Value (Value_Float flt) _,_) =
         case subtypeData of
            FloatingSubtype _ typeName constraint | valueInFloatRange flt constraint ->
               let typeData = Type_Type typeName FloatingType
               in Just (Calc_Value (Value_Float flt) typeData,typeData)
            --FloatingSubtype _ typeName constraint -> Nothing
            IntegerSubtype _ typeName constraint | valueInIntRange (round flt) constraint ->
               let typeData = Type_Type typeName IntegerType
               in Just (Calc_Value (Value_Int $ round flt) typeData,typeData)
            --IntegerSubtype _ typeName constraint -> Nothing
            _ -> Nothing
       convertType calcPair@(Calc_Value (Value_Physical int) _,Type_Type typeName1 _) =
         case subtypeData of
            PhysicalSubtype _ typeName2 _ _ constraint | typeName1 == typeName2 && valueInIntRange int constraint -> Just calcPair
       --array
       convertType _
         | isLocallyStatic staticLevel = Nothing
       convertType (calc,typeData@(Type_Type typeName1 (EnumerationType _))) =
         case subtypeData of
            EnumerationSubtype _ typeName2 _ _ | typeName1 == typeName2 -> Just (Calc_SubtypeResult subtypeName calc,typeData)
            _ -> Nothing
       convertType calcPair@(calc,Type_Type _ IntegerType) =
         case subtypeData of
            IntegerSubtype _ typeName _ -> Just (Calc_SubtypeResult subtypeName $ Calc_ExplicitTypeConversion (typeName,IntegerType) calcPair,Type_Type typeName IntegerType)
            FloatingSubtype _ typeName _ -> Just (Calc_SubtypeResult subtypeName $ Calc_ExplicitTypeConversion (typeName,FloatingType) calcPair,Type_Type typeName FloatingType)
            _ -> Nothing
       convertType calcPair@(calc,Type_Type _ FloatingType) =
         case subtypeData of
            IntegerSubtype _ typeName _ -> Just (Calc_SubtypeResult subtypeName $ Calc_ExplicitTypeConversion (typeName,IntegerType) calcPair,Type_Type typeName IntegerType)
            FloatingSubtype _ typeName _ -> Just (Calc_SubtypeResult subtypeName $ Calc_ExplicitTypeConversion (typeName,FloatingType) calcPair,Type_Type typeName FloatingType)
            _ -> Nothing
       convertType (calc,typeData@(Type_Type typeName1 (PhysicalType _ _))) =
         case subtypeData of
            PhysicalSubtype _ typeName2 _ _ _ -> Just (Calc_SubtypeResult subtypeName calc,typeData)
       convertType calcPair@(calc,Type_UniversalInt) =
         case subtypeData of
            IntegerSubtype _ typeName _ -> Just (Calc_SubtypeResult subtypeName $ Calc_ImplicitTypeConversion typeName calc,Type_Type typeName IntegerType)
            FloatingSubtype _ typeName _ -> Just (Calc_SubtypeResult subtypeName $ Calc_ExplicitTypeConversion (typeName,FloatingType) calcPair,Type_Type typeName FloatingType)
            _ -> Nothing
       convertType calcPair@(calc,Type_UniversalReal) =
         case subtypeData of
            IntegerSubtype _ typeName _ -> Just (Calc_SubtypeResult subtypeName $ Calc_ExplicitTypeConversion (typeName,IntegerType) calcPair,Type_Type typeName IntegerType)
            FloatingSubtype _ typeName _ -> Just (Calc_SubtypeResult subtypeName $ Calc_ImplicitTypeConversion typeName calc,Type_Type typeName FloatingType)
            _ -> Nothing
   case mapMaybe convertType expressions of
      calcPair@[_] -> return calcPair
      _ -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_FailedTypeConversion

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

data SubtypeAttributeDesignators =
   BaseSubtypeAttribute
   | LeftSubtypeAttribute
   | RightSubtypeAttribute
   | HighSubtypeAttribute
   | LowSubtypeAttribute
   | PosSubtypeAttribute
   | ValSubtypeAttribute
   | SuccSubtypeAttribute
   | PredSubtypeAttribute
   | LeftOfSubtypeAttribute
   | RightOfSubtypeAttribute
   ---- | CustomSubtypeAttribute ?? Not implemented

parseSubtypeAttribute :: Staticity -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [(Calculation,AllTypes)]
parseSubtypeAttribute staticLevel scope unit unitName subtypeName subtypeData = do
   attrTok <- getToken
   attrDesignator <- case matchIdentifier attrTok of
      Just (PosnWrapper pos name) ->
         case MapS.lookup (map toUpper name) attrMap of
            Just attr -> return attr
            Nothing -> throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Custom subtype attributes"
      Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSubtypeAttribute attrTok
   case attrDesignator of
      BaseSubtypeAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Base attribute" attrTok
      LeftSubtypeAttribute -> return $ subtypeAttributeLeft subtypeData
      RightSubtypeAttribute -> return $ subtypeAttributeRight subtypeData
      HighSubtypeAttribute -> return $ subtypeAttributeHigh subtypeData
      LowSubtypeAttribute -> return $ subtypeAttributeLow subtypeData
      -- ?? Do these change depending on subtype or is the result the same no matter the base type
      PosSubtypeAttribute | discreteSubtype -> subtypeAttributePos scope unit unitName subtypeName subtypeData
      PosSubtypeAttribute -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_NonDiscreteTypeDiscreteAttr "POS") attrTok
      ValSubtypeAttribute | discreteSubtype -> subtypeAttributeVal scope unit unitName subtypeName subtypeData
      ValSubtypeAttribute -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_NonDiscreteTypeDiscreteAttr "VAL") attrTok
      SuccSubtypeAttribute | discreteSubtype -> subtypeAttributeSucc (getPos attrTok) scope unit unitName subtypeName subtypeData
      SuccSubtypeAttribute -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_NonDiscreteTypeDiscreteAttr "SUCC") attrTok
      PredSubtypeAttribute | discreteSubtype -> subtypeAttributePred (getPos attrTok) scope unit unitName subtypeName subtypeData
      PredSubtypeAttribute -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_NonDiscreteTypeDiscreteAttr "PRED") attrTok
      LeftOfSubtypeAttribute | discreteSubtype -> subtypeAttributeLeftOf (getPos attrTok) scope unit unitName subtypeName subtypeData
      LeftOfSubtypeAttribute -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_NonDiscreteTypeDiscreteAttr "LEFTOF") attrTok
      RightOfSubtypeAttribute | discreteSubtype -> subtypeAttributeRightOf (getPos attrTok) scope unit unitName subtypeName subtypeData
      RightOfSubtypeAttribute -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_NonDiscreteTypeDiscreteAttr "RIGHTOF") attrTok
   where attrMap :: MapS.Map String SubtypeAttributeDesignators
         attrMap = MapS.fromList $
            [ ("BASE",BaseSubtypeAttribute)
            , ("LEFT",LeftSubtypeAttribute)
            , ("RIGHT",RightSubtypeAttribute)
            , ("HIGH",HighSubtypeAttribute)
            , ("LOW",LowSubtypeAttribute)
            , ("POS",PosSubtypeAttribute)
            , ("VAL",ValSubtypeAttribute)
            , ("SUCC",SuccSubtypeAttribute)
            , ("PRED",PredSubtypeAttribute)
            , ("LEFTOF",LeftOfSubtypeAttribute)
            , ("RIGHTOF",RightOfSubtypeAttribute)
            ]
         discreteSubtype :: Bool
         discreteSubtype = case subtypeData of
                              FloatingSubtype _ _ _ -> False
                              _ -> True

subtypeAttributeValue :: ((Enumerate,Enumerate) -> Enumerate,IntegerRange -> Int64,FloatRange -> Double) -> Subtype -> [(Calculation,AllTypes)]
subtypeAttributeValue (enumFunc,intFunc,fltFunc) subtype =
   let typeData = subtypeToType subtype
       value = case subtype of
                  EnumerationSubtype _ typeName _ range -> Value_Enum typeName (enumFunc range)
                  IntegerSubtype _ _ range -> Value_Int $ toInteger $ intFunc range
                  FloatingSubtype _ _ range -> Value_Float $ fltFunc range
                  PhysicalSubtype _ _ _ _ range -> Value_Physical $ toInteger $ intFunc range
                  -- ??Array
   in [(Calc_Value value typeData,typeData)]
subtypeAttributeLeft :: Subtype -> [(Calculation,AllTypes)]
subtypeAttributeLeft = subtypeAttributeValue (fst,int_scalarLeft,float_scalarLeft)
subtypeAttributeRight :: Subtype -> [(Calculation,AllTypes)]
subtypeAttributeRight = subtypeAttributeValue (snd,int_scalarRight,float_scalarRight)
subtypeAttributeHigh :: Subtype -> [(Calculation,AllTypes)]
subtypeAttributeHigh = subtypeAttributeValue (snd,int_scalarHigh,float_scalarHigh)
subtypeAttributeLow :: Subtype -> [(Calculation,AllTypes)]
subtypeAttributeLow = subtypeAttributeValue (fst,int_scalarLow,float_scalarLow)

parseSubtypeAttributeFunc :: ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack Value
parseSubtypeAttributeFunc scope unit unitName subtypeName subtypeData = do
   leftParenTok <- getToken
   unless (isLeftParen leftParenTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedLeftParenInSubtypeAttrFunc leftParenTok
   calc <- parseWithExpectedType LocallyStatic parseExpression scope unit unitName subtypeName subtypeData $ getPos leftParenTok
   rightParenTok <- getToken
   unless (isRightParen rightParenTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedRightParenInSubtypeAttrFunc rightParenTok
   return $ case calc of Calc_Value value _ -> value

subtypeAttributePos :: ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [(Calculation,AllTypes)]
subtypeAttributePos scope unit unitName subtypeName subtypeData = do
   value <- parseSubtypeAttributeFunc scope unit unitName subtypeName subtypeData
   let int = case (value,subtypeData) of
               (Value_Enum _ enum,EnumerationSubtype _ _ enums _) -> toInteger $ enum_discretePos enums enum
               (Value_Int int,_) -> int
               (Value_Physical int,_) -> int
   return [(Calc_Value (Value_Int int) Type_UniversalInt,Type_UniversalInt)]

subtypeAttributeVal :: ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [(Calculation,AllTypes)]
subtypeAttributeVal scope unit unitName subtypeName subtypeData = do
   leftParenTok <- getToken
   unless (isLeftParen leftParenTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedLeftParenInSubtypeAttrFunc leftParenTok
   calcs <- parseExpression LocallyStatic scope unit unitName
   rightParenTok <- getToken
   unless (isRightParen rightParenTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedRightParenInSubtypeAttrFunc rightParenTok
   let filterTypes (calc,Type_Type _ IntegerType) = Just calc
       filterTypes (calc,Type_UniversalInt) = Just calc
       filterTypes _ = Nothing
   int <- case mapMaybe filterTypes calcs of
      [Calc_Value (Value_Int int) _] -> return int
      _ -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotFindValueWithContext leftParenTok
   let typeData = subtypeToType subtypeData
   case subtypeData of
      EnumerationSubtype _ typeName enums range ->
         let enum = enum_discreteVal enums $ fromInteger int
         in if valueInEnumRange enum enums range
               then return [(Calc_Value (Value_Enum typeName enum) typeData,typeData)]
               else throwError $ ConverterError_Netlist $ passPosition (NetlistError_EnumValueOutOfRange typeName enum) leftParenTok
      IntegerSubtype _ _ range | valueInIntRange int range -> return [(Calc_Value (Value_Int int) typeData,typeData)]
      IntegerSubtype _ _ range -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_IntValueOutOfRange int) leftParenTok
      PhysicalSubtype _ _ _ _ range | valueInIntRange int range -> return [(Calc_Value (Value_Physical int) typeData,typeData)]
      PhysicalSubtype _ _ _ _ range -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_PhysValueOutOfRange int) leftParenTok

subtypeAttributeFunc :: ([Enumerate] -> Enumerate -> Enumerate,IntegerRange -> Int64 -> Int64) -> AlexPosn -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [(Calculation,AllTypes)]
subtypeAttributeFunc (enumFunc,intFunc) pos scope unit unitName subtypeName subtypeData = do
   value <- parseSubtypeAttributeFunc scope unit unitName subtypeName subtypeData
   let typeData = subtypeToType subtypeData
   result <- case (value,subtypeData) of
               (Value_Enum typeName enum,EnumerationSubtype _ _ enums constraint) ->
                  let newEnum = enumFunc enums enum
                  in if valueInEnumRange newEnum enums constraint
                        then return $ Value_Enum typeName newEnum
                        else throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_SubtypeAttributeArgOutOfRange
               (Value_Int int,IntegerSubtype _ _ constraint) ->
                  let newInt = toInteger $ intFunc constraint $ fromInteger int
                  in if valueInIntRange newInt constraint
                        then return $ Value_Int newInt
                        else throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_SubtypeAttributeArgOutOfRange
               (Value_Physical int,PhysicalSubtype _ _ _ _ constraint) -> 
                  let newInt = toInteger $ intFunc constraint $ fromInteger int
                  in if valueInIntRange newInt constraint
                        then return $ Value_Physical newInt
                        else throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_SubtypeAttributeArgOutOfRange
   return [(Calc_Value result typeData,typeData)]

subtypeAttributeSucc :: AlexPosn -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [(Calculation,AllTypes)]
subtypeAttributeSucc = subtypeAttributeFunc (enum_discreteSucc,int_discreteSucc)
subtypeAttributePred :: AlexPosn -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [(Calculation,AllTypes)]
subtypeAttributePred = subtypeAttributeFunc (enum_discretePred,int_discretePred)
subtypeAttributeLeftOf :: AlexPosn -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [(Calculation,AllTypes)]
subtypeAttributeLeftOf = subtypeAttributeFunc (enum_discreteLeftOf,int_discreteLeftOf)
subtypeAttributeRightOf :: AlexPosn -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [(Calculation,AllTypes)]
subtypeAttributeRightOf = subtypeAttributeFunc (enum_discreteRightOf,int_discreteRightOf)

staticTypeCompare :: Subtype -> [(Calculation,AllTypes)] -> AlexPosn -> ParserStack Value
staticTypeCompare subtypeData values pos =
   case subtypeData of
      EnumerationSubtype _ baseTypeName enums (left,right) ->
         let filterFunc (_,Type_Type name _) = name == baseTypeName
             filterFunc _ = False
             value = filter filterFunc values
         in case value of
               [(Calc_Value (Value_Enum _ enum) _,_)] | valueInEnumRange enum enums (left,right) -> return $ Value_Enum baseTypeName enum
               [(Calc_Value (Value_Enum _ enum) _,_)] -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_EnumValueOutOfRange baseTypeName enum
               _ -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_CannotFindValueWithContext
      IntegerSubtype _ baseTypeName range ->
         let filterFunc (_,Type_Type name _) = name == baseTypeName
             filterFunc (_,Type_UniversalInt) = True
             filterFunc _ = False
             value = filter filterFunc values
         in case value of
               [(Calc_Value (Value_Int int) _,_)] | valueInIntRange int range -> return $ Value_Int int
               [(Calc_Value (Value_Int int) _,_)] -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_IntValueOutOfRange int
               _ -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_CannotFindValueWithContext
      FloatingSubtype _ baseTypeName range ->
         let filterFunc (_,Type_Type name _) = name == baseTypeName
             filterFunc (_,Type_UniversalReal) = True
             filterFunc _ = False
             value = filter filterFunc values
         in case value of
               [(Calc_Value (Value_Float float) _,_)] | valueInFloatRange float range -> return $ Value_Float float
               [(Calc_Value (Value_Float float) _,_)] -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_FloatValueOutOfRange float
               _ -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_CannotFindValueWithContext
      PhysicalSubtype _ baseTypeName _ _ range ->
         let filterFunc (_,Type_Type name _) = name == baseTypeName
             filterFunc _ = False
             value = filter filterFunc values
         in case value of
               [(Calc_Value (Value_Physical int) _,_)] | valueInIntRange int range -> return $ Value_Physical int
               [(Calc_Value (Value_Physical int) _,_)] -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_PhysValueOutOfRange int
               _ -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_CannotFindValueWithContext
      --ArraySubtype

valueInIntRange :: Integer -> IntegerRange -> Bool -- ?? Should this error be dealt with earlier
valueInIntRange int (IntegerRange left right To) = int >= (toInteger left) && int <= (toInteger right)
valueInIntRange int (IntegerRange left right Downto) = int <= (toInteger left) && int >= (toInteger right)
valueInFloatRange :: Double -> FloatRange -> Bool
valueInFloatRange float (FloatRange left right To) = (not $ isInfinite float) && float >= left && float <= right
valueInFloatRange float (FloatRange left right Downto) = (not $ isInfinite float) && float <= left && float >= right
valueInEnumRange :: Enumerate -> [Enumerate] -> (Enumerate,Enumerate) -> Bool
valueInEnumRange enum enums (left,right) = elem enum (right:(takeWhile (/= right) $ dropWhile (/= left) enums))

parseWithExpectedType :: Staticity -> (Staticity -> ScopeStore -> UnitStore -> NetlistName -> ParserStack [(Calculation,AllTypes)]) -> ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> AlexPosn -> ParserStack Calculation
parseWithExpectedType staticLevel parseFunction scope unit unitName expectedSubtypeName expectedSubtype pos = do
   expressions <- parseFunction staticLevel scope unit unitName
   let findType :: Subtype -> (Calculation,AllTypes) -> Maybe Calculation
       findType (EnumerationSubtype _ typeName1 enums range) (calc@(Calc_Value (Value_Enum _ enum) _),Type_Type typeName2 _)
         | typeName1 == typeName2 && valueInEnumRange enum enums range = Just calc
         | otherwise = Nothing
       findType (EnumerationSubtype _ typeName1 _ _) (calc,Type_Type typeName2 _)
         | typeName1 == typeName2 && notLocallyStatic staticLevel = Just $ Calc_SubtypeResult expectedSubtypeName calc
         | otherwise = Nothing --throwError $ ConverterError $ passPosition NetlistError_NonStaticInStaticExpression
       findType (IntegerSubtype _ typeName1 constraint) (calc@(Calc_Value (Value_Int int) _),Type_Type typeName2 _)
         | typeName1 == typeName2 && valueInIntRange int constraint = Just calc
         | otherwise = Nothing
       findType (IntegerSubtype _ baseTypeName constraint) (calc@(Calc_Value (Value_Int int) _),Type_UniversalInt)
         | valueInIntRange int constraint = Just $ Calc_ImplicitTypeConversion baseTypeName calc
         | otherwise = Nothing
       findType (IntegerSubtype _ typeName1 _) (calc,Type_Type typeName2 _)
         | typeName1 == typeName2 && notLocallyStatic staticLevel = Just $ Calc_SubtypeResult expectedSubtypeName calc
         | otherwise = Nothing
       findType (IntegerSubtype _ _ _) (calc,Type_UniversalInt)
         | notLocallyStatic staticLevel = Just $ Calc_ImplicitTypeConversion expectedSubtypeName calc
         | otherwise = Nothing
       findType (FloatingSubtype _ typeName1 constraint) (calc@(Calc_Value (Value_Float flt) _),Type_Type typeName2 _)
         | typeName1 == typeName2 && valueInFloatRange flt constraint = Just calc
         | otherwise = Nothing
       findType (FloatingSubtype _ baseTypeName constraint) (calc@(Calc_Value (Value_Float flt) _),Type_UniversalReal)
         | valueInFloatRange flt constraint = Just $ Calc_ImplicitTypeConversion baseTypeName calc
         | otherwise = Nothing
       findType (FloatingSubtype _ typeName1 _) (calc,Type_Type typeName2 _)
         | typeName1 == typeName2 && notLocallyStatic staticLevel = Just $ Calc_SubtypeResult expectedSubtypeName calc
         | otherwise = Nothing
       findType (FloatingSubtype _ _ _) (calc,Type_UniversalReal)
         | notLocallyStatic staticLevel = Just $ Calc_ImplicitTypeConversion expectedSubtypeName calc
         | otherwise = Nothing
       findType (PhysicalSubtype _ typeName1 _ _ constraint) (calc@(Calc_Value (Value_Physical int) _),Type_Type typeName2 _)
         | typeName1 == typeName2 && valueInIntRange int constraint = Just calc
         | otherwise = Nothing
       findType (PhysicalSubtype _ typeName1 _ _ _) (calc,Type_Type typeName2 _)
         | typeName1 == typeName2 && notLocallyStatic staticLevel = Just $ Calc_SubtypeResult expectedSubtypeName calc
         | otherwise = Nothing
       exp = mapMaybe (findType expectedSubtype) expressions
   case exp of
      [] -> throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_CannotFindCalculationOfType expectedSubtypeName
      [calc] -> return calc
      _ -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_CannotFindValueWithContext

parseSignalName :: ScopeStore -> UnitStore -> NetlistName -> (Maybe NetlistName,String) -> Signal -> SignalType -> ParserStack [(Calculation,AllTypes)]
parseSignalName scope unit unitName signalName signalData signalType = do
   nextTok <- getToken
   case nextTok of
      token | isLeftParen token -> throwError $ ConverterError_NotImplemented $ passPosition "Array type slice/indices" token
      token | isApostrophe token -> parseSignalAttribute scope unit unitName signalName signalData signalType
      token -> do
         saveToken token
         return [(Calc_Signal signalName signalType,subtypeToType $ signal_typeData signalData)]

data SignalAttributeDesignators =
   DelayedSignalAttribute
   | StableSignalAttribute
   | QuietSignalAttribute
   | TransactionSignalAttribute
   | EventSignalAttribute
   | ActiveSignalAttribute
   | LastEventSignalAttribute
   | LastActiveSignalAttribute
   | LastValueSignalAttribute

parseSignalAttribute :: ScopeStore -> UnitStore -> NetlistName -> (Maybe NetlistName,String) -> Signal -> SignalType -> ParserStack [(Calculation,AllTypes)]
parseSignalAttribute scope unit unitName signalName signalData signalType = do
   attrTok <- getToken
   attrDesignator <- case matchIdentifier attrTok of
      Just (PosnWrapper pos name) ->
         case MapS.lookup (map toUpper name) attrMap of
            Just attr -> return attr
            Nothing -> throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Custom signal attributes"
      Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSignalAttribute attrTok
   case attrDesignator of
      DelayedSignalAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Delayed signal attribute" attrTok --optionalAttrFunc Calc_SignalDelayed $ subtypeToType $ signal_typeData signal
      StableSignalAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Stable signal attribute" attrTok --optionalAttrFunc Calc_SignalStable $ Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") ((packageTypes standardPackage) MapS.! "ANON'BOOLEAN")
      QuietSignalAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Quiet signal attribute" attrTok --optionalAttrFunc Calc_SignalQuiet
      TransactionSignalAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Transaction attribute" attrTok
      EventSignalAttribute -> return [(Calc_SignalEvent signalName signalType,Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") $ (packageTypes standardPackage) MapS.! "ANON'BOOLEAN")]
      ActiveSignalAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Active signal attribute" attrTok
      LastEventSignalAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Last event attribute" attrTok
      LastActiveSignalAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Last active attribute" attrTok
      LastValueSignalAttribute -> throwError $ ConverterError_NotImplemented $ passPosition "Last value attribute" attrTok
   where attrMap :: MapS.Map String SignalAttributeDesignators
         attrMap = MapS.fromList $
            [ ("DELAYED",DelayedSignalAttribute)
            , ("STABLE",StableSignalAttribute)
            , ("QUIET",QuietSignalAttribute)
            , ("TRANSACTION",TransactionSignalAttribute)
            , ("EVENT",EventSignalAttribute)
            , ("ACTIVE",ActiveSignalAttribute)
            , ("LAST_EVENT",LastEventSignalAttribute)
            , ("LAST_ACTIVE",LastActiveSignalAttribute)
            , ("LAST_VALUE",LastValueSignalAttribute)
            ]
         optionalAttrFunc :: ((Maybe NetlistName,String) -> Int64 -> Calculation) -> AllTypes -> ParserStack [(Calculation,AllTypes)]
         optionalAttrFunc newCalcWithName returnType = do -- ?? Need signal type (internal, port, etc.)
            leftParenTok <- getToken
            let newCalc = newCalcWithName signalName
            case leftParenTok of
               token | isLeftParen token -> do
                  calc <- parseWithExpectedType LocallyStatic parseExpression scope unit unitName (NetlistName "STD" "STANDARD","TIME") ((packageSubtypes standardPackage) MapS.! "TIME") $ getPos token
                  case calc of
                     Calc_Value (Value_Physical int) _ | int >= 0 -> return [(newCalc $ fromInteger int,returnType)]
                     Calc_Value (Value_Physical int) _ -> throwError $ ConverterError_Netlist $ passPosition NetlistError_NegativeTimeInSignalAttribute leftParenTok
               token -> do
                  saveToken token
                  return [(newCalc 0,returnType)]
