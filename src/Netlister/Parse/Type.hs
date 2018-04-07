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
import Data.List (sortBy)

import Parser.Happy.Types
         ( TypeDeclaration(..)
         , WrappedTypeDeclaration
         , TypeDefinition(..)
         , WrappedTypeDefinition
         , EnumerationLiteral(..)
         , WrappedEnumerationLiteral
         )
import Parser.PositionWrapper
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
import Netlister.Convert.Stores (isNameDeclaredInUnit)
import Netlister.Types.Top
         ( ConversionStack
         , ConverterError
            ( ConverterError_NotImplemented
            , ConverterError_Netlist
            )
         , NetlistError
            ( NetlistError_DuplicateTypes
            , NetlistError_DuplicateEnums
            )
         )

convertType :: ScopeStore -> UnitStore -> WrappedTypeDeclaration -> ConversionStack (String,Type)
convertType _ _ (PosnWrapper typePos (IncompleteTypeDefinition _)) =
   throwError $ ConverterError_NotImplemented $ PosnWrapper typePos "incomplete type definition"
convertType scope unit (PosnWrapper typePos (FullTypeDeclaration (PosnWrapper namePos typeName) typeDef)) = do
   -- If name is in scope, that is okay, but if name is in package, this is an error
   let upperTypeName = map toUpper typeName
   when (isNameDeclaredInUnit unit upperTypeName) $ throwError $ ConverterError_Netlist $ PosnWrapper typePos $ NetlistError_DuplicateTypes typeName
   newType <- convertTypeDefinition typeDef
   return (typeName,newType)

-- |Convert type definition
convertTypeDefinition :: WrappedTypeDefinition -> ConversionStack Type
convertTypeDefinition (PosnWrapper definePos (EnumerationTypeDefinition enumerates)) =
   case convertEnumerationLiterals enumerates [] of
      Left invalidEnums ->
         let sortFunction :: WrappedEnumerationLiteral -> WrappedEnumerationLiteral -> Ordering
             sortFunction a b = compare (unPos a) (unPos b)
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
             (fstErrEnum:otherErrEnums) = sortBy sortFunction invalidEnums
         in throwError $ ConverterError_Netlist $ PosnWrapper definePos $ NetlistError_DuplicateEnums $ filterFunction fstErrEnum otherErrEnums [[]]
      Right newEnums -> return $ EnumerationType newEnums
--convertTypeDefinition (PosnWrapper definePos (UniversalTypeDefinition range)) =
--convertTypeDefinition (PosnWrapper definePos (PhysicalTypeDefinition range name units)) =
--convertTypeDefinition (PosnWrapper definePos (UnconstrainedArrayTypeDefinition names subtypeIndication)) =
--convertTypeDefinition (PosnWrapper definePos (ConstrainedArrayTypeDefinition constraint subtypeIndication)) =
--convertTypeDefinition (PosnWrapper definePos (RecordTypeDefinition elementDeclares)) =
--convertTypeDefinition (PosnWrapper definePos (AccessTypeDefinition subtypeIndication)) =
--convertTypeDefinition (PosnWrapper definePos (FileTypeDefinition typeName)) =

-- |Convert enumeration literals
-- Return converted enumerates or list of non-converted literals if error occurs
convertEnumerationLiterals :: [WrappedEnumerationLiteral] -> [Enumerate] -> Either [WrappedEnumerationLiteral] [Enumerate]
convertEnumerationLiterals (enum:otherEnums) convEnums =
   let newEnum = case unPos enum of
                     EnumerationLiteral_Identifier iden -> Enum_Identifier iden
                     EnumerationLiteral_Char char -> Enum_Char char
   in if elem newEnum convEnums
         then throwError (enum:otherEnums)
         else convertEnumerationLiterals otherEnums (newEnum:convEnums)
convertEnumerationLiterals [] enums = return enums

--convertUniversalType :: WrappedRange -> ConversionStack Type
--convertUniversalType (PosnWrapper rangePos (RangeAttributeName _)) = throwError $ ConverterError_NotImplemented $ PosnWrapper rangePos "Range attribute name"
--convertUniversalType (PosnWrapper 
