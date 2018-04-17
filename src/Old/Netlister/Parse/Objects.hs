{-|
   Module      : Netlister.Parse.Objects
   Description : Navigate and convert objects declaration parts of tree
-}
module Netlister.Parse.Objects
   ( convertConstant
   , --convertSignal
   ) where

import qualified Data.Map.Strict as MapS
import Control.Monad.Except (throwError)
import Data.Char (toUpper)

import Lexer.Types.PositionWrapper
import Parser.Happy.Types
            ( ConstantDeclaration(..)
            , WrappedConstantDeclaration
            , SubtypeIndication(..)
            , Name(..)
            , WrappedSimpleName
            )
import Netlister.Types.Stores
            ( ConstantStore
            , ScopeStore(..)
            , UnitStore(..)
            )
import Netlister.Functions.Stores (isNameInUnit)
import Netlister.Types.Objects
            ( Constant(..)
            )
import Netlister.Types.Top
            ( ConversionStack
            , ConverterError(..)
            , NetlistError(..)
            )

convertConstant :: ScopeStore -> UnitStore -> WrappedConstantDeclaration -> ConversionStack ConstantStore
convertConstant _ _ (PosnWrapper pos (ConstantDeclaration _ _ (Just _))) =
   throwError $ ConverterError_NotImplemented $ PosnWrapper pos "expression in constant declaration"
convertConstant scope unit (PosnWrapper constPos (ConstantDeclaration names subtypeIndication Nothing)) = do
   constNames <- case checkNames unit names of
                     Right constNames -> return constNames
                     Left errNames -> throwError $ ConverterError_Netlist $ PosnWrapper constPos $ NetlistError_DuplicateConstantNames errNames
   (typePos,typeName) <- case subtypeIndication of
                           PosnWrapper _ (SubtypeIndication (Just (PosnWrapper pos _)) _ _) -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_ConstantResolutionFunction
                           PosnWrapper _ (SubtypeIndication _ _ (Just (PosnWrapper pos _))) -> throwError $ ConverterError_Netlist $ PosnWrapper pos NetlistError_ConstantConstraint
                           PosnWrapper _ (SubtypeIndication Nothing (PosnWrapper typePos typeName) Nothing) -> return (typePos,typeName)
   checkedTypeName <- case typeName of
                        Name_Simple name -> return $ map toUpper name
                        Name_Operator _ -> throwError $ ConverterError_Netlist $ PosnWrapper typePos $ NetlistError_InvalidTypeName_Operator
                        _ -> throwError $ ConverterError_NotImplemented  $ PosnWrapper typePos "Other name type in constant declaration"
   typeVal <- case MapS.lookup checkedTypeName $ unitTypes unit of
                  Nothing -> case MapS.lookup checkedTypeName $ scopeTypes scope of
                                 Nothing -> throwError $ ConverterError_Netlist $ PosnWrapper typePos $ NetlistError_TypeNotFound checkedTypeName
                                 Just typeVal -> return typeVal
                  Just typeVal -> return typeVal
   -- ?? Add calculation/expression
   return $ MapS.fromList $ map (\(PosnWrapper _ constName) -> (constName,Constant typeVal Nothing)) names

checkNames :: UnitStore -> [WrappedSimpleName] -> Either [WrappedSimpleName] [String]
checkNames unit names =
   let checkNames' :: [WrappedSimpleName] -> [WrappedSimpleName] -> [String] -> Either [WrappedSimpleName] [String]
       checkNames' (wrappedName:otherNames) errNames successNames =
         let name = map toUpper $ unPos wrappedName
             continueCheck = checkNames' otherNames
         in if isNameInUnit unit name
               then continueCheck (wrappedName:errNames) successNames
               else continueCheck errNames (name:successNames)
       checkNames' [] [] successNames = Right successNames
       checkNames' [] errNames _ = Left errNames
   in checkNames' names [] []

--convertSignal :: TypeStore -> TypeStore -> FunctionStore -> ConstantStore -> WrappedConstantDeclaration -> ConversionStack ConstantStore


-- |convertInterface
-- Function interface:
--    For constant type: only mode allowed is 'in'
