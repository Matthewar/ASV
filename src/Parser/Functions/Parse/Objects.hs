module Parser.Functions.Parse.Objects
   ( parseConstant
   ) where

import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
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
         ( NetlistName
         , Subtype(..)
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
import Parser.Netlist.Functions.Stores (isNameInUnit)
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Functions.IdentifyToken
         ( isColon
         , isComma
         , isSemicolon
         , isVarAssign
         , matchIdentifier
         )
import Parser.Functions.Parse.Subtype (parseSubtypeIndication)
import Parser.Functions.Parse.Expression (parseExpression)
import Manager.Types.Error (ConverterError(..))

-- ?? Pass constant keyword position for error messages?
parseConstant :: ScopeStore -> UnitStore -> NetlistName -> ParserStack (ConstantStore,SubtypeStore -> SubtypeStore)
parseConstant scope unit unitName = do
   idenList <- parseIdentifierList scope unit
   contTok <- getToken
   unless (isColon contTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedColonInConstDecl contTok
   (subtypePackage,subtypeName,subtypeData,modSubtype) <- parseSubtypeIndication False scope unit unitName
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
               return $ reverse identifierList
   in parseIdentifierList' []
