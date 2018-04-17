{-|
   Module      : Netlister.Parse.Type
   Description : Navigate and convert type declaration in tree
-}
module Netlister.Parse.Type
   ( convertType
   ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Data.List
         ( sortBy
         , find
         )

import Lexer.Types.PositionWrapper
import Parser.Happy.Types
         ( TypeDeclaration(..)
         , WrappedTypeDeclaration
         , TypeDefinition(..)
         , WrappedTypeDefinition
         , EnumerationLiteral(..)
         , WrappedEnumerationLiteral
         , WrappedSimpleName
         )
import Netlister.Types.Representation
         ( Type(..)
         , Enumerate(..)
         )
import Netlister.Types.Stores
         ( TypeStore
         , FunctionStore
         , ScopeStore
         , UnitStore
         )
import Netlister.Functions.Stores
         ( isNameInUnit
         , isEnumNameInUnit
         )
import Netlister.Types.Top
         ( ConversionStack
         , ConverterError
            ( ConverterError_NotImplemented
            , ConverterError_Netlist
            )
         , NetlistError(..)
         )

convertType :: ScopeStore -> UnitStore -> WrappedTypeDeclaration -> ConversionStack (String,Type)
convertType _ _ (PosnWrapper typePos (IncompleteTypeDefinition _)) =
   throwError $ ConverterError_NotImplemented $ PosnWrapper typePos "incomplete type definition"
convertType scope unit (PosnWrapper typePos (FullTypeDeclaration (PosnWrapper namePos typeName) typeDef)) = do
   -- If name is in scope, that is okay, but if name is in package, this is an error
   let upperTypeName = map toUpper typeName
   when (isNameInUnit unit upperTypeName) $ throwError $ ConverterError_Netlist $ PosnWrapper typePos $ NetlistError_DuplicateTypes upperTypeName
   newType <- convertTypeDefinition unit upperTypeName typeDef
   return (typeName,newType)

-- |Convert type definition
convertTypeDefinition :: UnitStore -> String -> WrappedTypeDefinition -> ConversionStack Type
convertTypeDefinition unit typeName (PosnWrapper definePos (EnumerationTypeDefinition enumerates)) = do
   newEnums <- convertEnumerationLiterals unit (PosnWrapper definePos typeName) enumerates []
   return $ EnumerationType newEnums
--convertTypeDefinition unit (PosnWrapper definePos (UniversalTypeDefinition range)) =
--convertTypeDefinition unit (PosnWrapper definePos (PhysicalTypeDefinition range name units)) =
--   when (isNameInUnit unit
--convertTypeDefinition unit (PosnWrapper definePos (UnconstrainedArrayTypeDefinition names subtypeIndication)) =
--convertTypeDefinition unit (PosnWrapper definePos (ConstrainedArrayTypeDefinition constraint subtypeIndication)) =
--convertTypeDefinition unit (PosnWrapper definePos (RecordTypeDefinition elementDeclares)) =
--convertTypeDefinition unit (PosnWrapper definePos (AccessTypeDefinition subtypeIndication)) =
--convertTypeDefinition unit (PosnWrapper definePos (FileTypeDefinition typeName)) =

-- |Convert enumeration literals
-- Return converted enumerates or list of non-converted literals if error occurs
convertEnumerationLiterals :: UnitStore -> WrappedSimpleName -> [WrappedEnumerationLiteral] -> [Enumerate] -> ConversionStack [Enumerate]
convertEnumerationLiterals unit typeName (enum:otherEnums) convEnums = do
   newEnum <- case unPos enum of
                  EnumerationLiteral_Identifier iden -> do
                     let upperIden = map toUpper iden
                     when (iden == unPos typeName) $ throwError $ ConverterError_Netlist $ PosnWrapper (getPos enum) $ NetlistError_EnumLitIsTypeName iden
                     when (isEnumNameInUnit unit upperIden) $ throwError $ ConverterError_Netlist $ PosnWrapper (getPos enum) $ NetlistError_InvalidEnumName iden
                     return $ Enum_Identifier iden
                  EnumerationLiteral_Char char -> return $ Enum_Char char
   if elem newEnum convEnums
      then let (fstErrEnum:otherErrEnums) = sortBy sortFunction (enum:otherEnums)
           in throwError $ ConverterError_Netlist $ PosnWrapper (getPos typeName) $ NetlistError_DuplicateEnums $ filterFunction fstErrEnum otherErrEnums [[]]
      else convertEnumerationLiterals unit typeName otherEnums (newEnum:convEnums)
   where -- |Sort enumation literals
         sortFunction :: WrappedEnumerationLiteral -> WrappedEnumerationLiteral -> Ordering
         sortFunction a b = compare (unPos a) (unPos b)
         -- |Find pairs of duplicate enum names
         filterFunction :: WrappedEnumerationLiteral -> [WrappedEnumerationLiteral] -> [[WrappedEnumerationLiteral]] -> [[WrappedEnumerationLiteral]]
         filterFunction compareEnum (enum:enums) (currentErrEnum:otherErrEnums) =
           if unPos enum == unPos compareEnum
              then filterFunction enum enums $
                    if null currentErrEnum
                       then (enum:compareEnum:currentErrEnum):otherErrEnums
                       else if head currentErrEnum == compareEnum
                                then (enum:currentErrEnum):otherErrEnums
                                else ([enum,compareEnum]):currentErrEnum:otherErrEnums
              else filterFunction enum enums (currentErrEnum:otherErrEnums)
         filterFunction _ [] errEnums = errEnums
convertEnumerationLiterals _ _ [] enums = return enums

--convertUniversalType :: WrappedRange -> ConversionStack Type
--convertUniversalType (PosnWrapper rangePos (RangeAttributeName _)) = throwError $ ConverterError_NotImplemented $ PosnWrapper rangePos "Range attribute name"
--convertUniversalType (PosnWrapper 
