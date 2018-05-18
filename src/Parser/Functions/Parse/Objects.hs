module Parser.Functions.Parse.Objects
   ( parseConstant
   ) where

import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Data.List (find)
import Control.Monad.Except
         ( throwError
         , unless
         , when
         )

import Lexer.Types.PositionWrapper
import Lexer.Types.Error (ParserError(..))
import Lexer.Functions.PositionWrapper
         ( passPosition
         , raisePosition
         )
import Parser.Types.Expressions
         ( AllTypes(..)
         , Staticity(..)
         )
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Netlist.Types.Representation
         ( Type(..)
         , Subtype(..)
         , NetlistName
         , IntegerRange(..)
         , FloatRange(..)
         , RangeDirection(..)
         , Calculation(..)
         , Value(..)
         , Constant(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , UnitStore
         , ConstantStore
         , SubtypeStore
         )
import Parser.Netlist.Functions.Stores
         ( matchSubtypeNameInScope
         , matchFunctionNameInScope
         , isNameInUnit
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Functions.IdentifyToken
         ( isColon
         , isComma
         , isKeywordRange
         , isLeftParen
         , isSemicolon
         , isVarAssign
         , matchIdentifier
         )
import Parser.Functions.Parse.Type
         ( parseRange
         , RangeConstraint(..)
         )
import Parser.Functions.Parse.Expression (parseExpression)
import Manager.Types.Error (ConverterError(..))
import Sim.Types (showEnum)

-- ?? Pass constant keyword position for error messages?
parseConstant :: ScopeStore -> UnitStore -> NetlistName -> ParserStack (ConstantStore,SubtypeStore -> SubtypeStore)
parseConstant scope unit unitName = do
   idenList <- parseIdentifierList scope unit
   contTok <- getToken
   unless (isColon contTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedColonInConstDecl contTok
   typeNameTok <- getToken -- ?? Adapt to allow expanded names
   typeName <- case matchIdentifier typeNameTok of
                  Just (PosnWrapper _ name) -> return $ map toUpper name
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedTypeMarkInConstDecl typeNameTok
   let --typeCheck = case matchTypeNameInScope scope unit unitName typeName of -- Name denotes type and unconstrained subtype
       --              Just (typeData,packageName) ->
       --              Nothing -> subtypeCheck
       subtypeCheck = case matchSubtypeNameInScope scope unit unitName typeName of
                        Just (subtypeData,packageName) -> parseConstraint scope unit unitName (subtypeData,packageName,typeName)
                        Nothing -> functionCheck
       functionCheck =  let errorName = case matchFunctionNameInScope scope unit unitName typeName of
                                          Just _ -> NetlistError_ResFuncNotAllowedInConstDecl --typeName
                                          Nothing -> NetlistError_UnrecognisedNameInConstTypeIndic --typeName
                        in throwError $ ConverterError_Netlist $ passPosition errorName typeNameTok
   (subtypePackage,subtypeName,subtypeData,modSubtype) <- subtypeCheck
   valueTok <- getToken
   let valueInIntRange :: Integer -> IntegerRange -> Bool -- ?? Should this error be dealt with earlier
       valueInIntRange int (IntegerRange left right To) = int >= (toInteger left) && int <= (toInteger right)
       valueInIntRange int (IntegerRange left right Downto) = int <= (toInteger left) && int >= (toInteger right)
       valueInFloatRange :: Double -> FloatRange -> Bool
       valueInFloatRange float (FloatRange left right To) = (not $ isInfinite float) && float >= left && float <= right
       valueInFloatRange float (FloatRange left right Downto) = (not $ isInfinite float) && float <= left && float >= right
   case valueTok of
      token | isVarAssign token -> do
         values <- parseExpression LocallyStatic scope unit unitName
         constant <- case subtypeData of
            EnumerationSubtype _ baseTypeName enums (left,right) ->
               let filterFunc (_,Type_Type name _) = name == baseTypeName
                   filterFunc _ = False
                   value = filter filterFunc values
               in case value of
                     [(Calc_Value (Value_Enum _ enum),_)] | elem enum (right:(takeWhile (/= right) $ dropWhile (/= left) enums)) -> return $ Constant (subtypePackage,subtypeName) subtypeData $ Just $ Value_Enum baseTypeName enum
                     [(Calc_Value (Value_Enum _ enum),_)] -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_EnumValueOutOfRange enum) token
                     _ -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotFindValueWithContext token
            IntegerSubtype _ baseTypeName range ->
               let filterFunc (_,Type_Type name _) = name == baseTypeName
                   filterFunc (_,Type_UniversalInt) = True
                   filterFunc _ = False
                   value = filter filterFunc values
               in case value of
                     [(Calc_Value (Value_Int int),_)] | valueInIntRange int range -> return $ Constant (subtypePackage,subtypeName) subtypeData $ Just $ Value_Int int
                     [(Calc_Value (Value_Int int),_)] -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_IntValueOutOfRange int) token
                     _ -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotFindValueWithContext token
            FloatingSubtype _ baseTypeName range ->
               let filterFunc (_,Type_Type name _) = name == baseTypeName
                   filterFunc (_,Type_UniversalReal) = True
                   filterFunc _ = False
                   value = filter filterFunc values
               in case value of
                     [(Calc_Value (Value_Float float),_)] | valueInFloatRange float range -> return $ Constant (subtypePackage,subtypeName) subtypeData $ Just $ Value_Float float
                     [(Calc_Value (Value_Float float),_)] -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_FloatValueOutOfRange float) token
                     _ -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotFindValueWithContext token
            PhysicalSubtype _ baseTypeName _ _ range ->
               let filterFunc (_,Type_Type name _) = name == baseTypeName
                   filterFunc _ = False
                   value = filter filterFunc values
               in case value of
                     [(Calc_Value (Value_Physical int),_)] | valueInIntRange int range -> return $ Constant (subtypePackage,subtypeName) subtypeData $ Just $ Value_Physical int
                     [(Calc_Value (Value_Physical int),_)] -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_PhysValueOutOfRange int) token
                     _ -> throwError $ ConverterError_Netlist $ passPosition NetlistError_CannotFindValueWithContext token
            --ArraySubtype
         finalTok <- getToken
         unless (isSemicolon finalTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedConstEnd finalTok
         let newConsts = MapS.fromList $ zip idenList $ repeat constant
         return (newConsts,modSubtype)
      token | isSemicolon token -> throwError $ ConverterError_NotImplemented $ passPosition "Deferred constants" token
      token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedConstValueOrEnd token

parseIdentifierList :: ScopeStore -> UnitStore -> ParserStack [String]
parseIdentifierList scope unit =
   let parseIdentifierList' :: [String] -> ParserStack [String]
       parseIdentifierList' identifierList = do
         nameTok <- getToken
         constName <- case matchIdentifier nameTok of
                        Just (PosnWrapper _ name) -> return $ map toUpper name
                        Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedConstName nameTok
         when (isNameInUnit unit constName) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_ConstNameAlreadyDefined constName) nameTok
         when (elem constName identifierList) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_DuplicateConstName constName) nameTok
         contTok <- getToken
         case contTok of
            token | isComma token -> parseIdentifierList' (constName:identifierList)
            token -> do
               saveToken token
               return identifierList
   in parseIdentifierList' []

parseConstraint :: ScopeStore -> UnitStore -> NetlistName -> (Subtype,NetlistName,String) -> ParserStack (NetlistName,String,Subtype,SubtypeStore -> SubtypeStore)
parseConstraint scope unit unitName (subtypeData,packageName,subtypeName) = do
   constraintTok <- getToken
   case constraintTok of
      token | isKeywordRange token -> do
         case subtypeData of
            ArraySubtype _ _ _ _ _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_RangeConstraintForArrayType token
            _ -> return ()
         range <- parseRange scope unit unitName $ getPos token
         let integerConstraintCompare (IntegerRange left1 right1 dir1) (IntegerRange left2 right2 dir2) =
               let checkVal val = case dir1 of
                                    To -> val >= left1 && val <= right1
                                    Downto -> val <= left1 && val >= right1
               in checkVal left2 && checkVal right2
             floatingConstraintCompare (FloatRange left1 right1 dir1) (FloatRange left2 right2 dir2) =
               let checkVal val = case dir1 of
                                    To -> val >= left1 && val <= right1
                                    Downto -> val <= left1 && val >= right1
               in checkVal left2 && checkVal right2
         case (subtypeData,range) of
            (IntegerSubtype resFunc baseTypeName constraint,IntegerConstraint newConstraint) | integerConstraintCompare constraint newConstraint ->
               let (left,right,dir) = case newConstraint of
                                       (IntegerRange left right To) -> (show left,show right,"TO")
                                       (IntegerRange left right Downto) -> (show left,show right,"DOWNTO")
                   newTypeName = subtypeName ++ "'ANON'" ++ dir ++ "'" ++ left ++ "'" ++ right
                   newTypeData = IntegerSubtype resFunc baseTypeName newConstraint
               in return (unitName,newTypeName,newTypeData,MapS.insert newTypeName newTypeData)
            (IntegerSubtype _ _ constraint,IntegerConstraint newConstraint) -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_InvalidSubtypeIntConstraint constraint newConstraint) token
            (FloatingSubtype resFunc baseTypeName constraint,FloatConstraint newConstraint) | floatingConstraintCompare constraint newConstraint ->
               let (left,right,dir) = case newConstraint of
                                       (FloatRange left right To) -> (show left,show right,"TO")
                                       (FloatRange left right Downto) -> (show left,show right,"DOWNTO")
                   newTypeName = subtypeName ++ "'ANON'" ++ dir ++ "'" ++ left ++ "'" ++ right
                   newTypeData = FloatingSubtype resFunc baseTypeName newConstraint
               in return (unitName,newTypeName,newTypeData,MapS.insert newTypeName newTypeData)
            (FloatingSubtype _ _ constraint,FloatConstraint newConstraint) -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_InvalidSubtypeFloatConstraint constraint newConstraint) token
            (EnumerationSubtype resFunc baseTypeName enums constraint,EnumerationConstraint potentialConstraints) ->
               case find (\(_,_,enumPackage,enumTypeName,_) -> enumPackage == packageName && enumTypeName == subtypeName) potentialConstraints of
                  Just (left,right,_,_,EnumerationType _) ->
                     let newTypeName = subtypeName ++ "'ANON" ++ showEnum "" left ++ showEnum "" right
                         newTypeData = EnumerationSubtype resFunc baseTypeName enums (left,right)
                     in return (unitName,newTypeName,newTypeData,MapS.insert newTypeName newTypeData)
                  Nothing -> throwError $ ConverterError_Netlist $ passPosition NetlistError_NoValidEnumRangeFound token
            (PhysicalSubtype resFunc baseTypeName baseUnit secondaryUnits constraint,PhysicalConstraint newConstraint) | integerConstraintCompare constraint newConstraint ->
               let (left,right,dir) = case newConstraint of
                                       (IntegerRange left right To) -> (show left,show right,"TO")
                                       (IntegerRange left right Downto) -> (show left,show right,"DOWNTO")
                   newTypeName = subtypeName ++ "'ANON'" ++ dir ++ "'" ++ left ++ "'" ++ right
                   newTypeData = PhysicalSubtype resFunc baseTypeName baseUnit secondaryUnits newConstraint
               in return (unitName,newTypeName,newTypeData,MapS.insert newTypeName newTypeData)
            (PhysicalSubtype resFunc baseTypeName baseUnit secondaryUnits constraint,PhysicalConstraint newConstraint) -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_InvalidSubtypePhysConstraint constraint newConstraint) token
      token | isLeftParen token -> throwError $ ConverterError_NotImplemented $ passPosition "Discrete range constraint (array type subtype indication)" token
      token -> do
         saveToken token
         return (packageName,subtypeName,subtypeData,\s -> s)
