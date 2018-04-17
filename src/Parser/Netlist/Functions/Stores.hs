{-|
   Module      : Parser.Netlist.Convert.Stores
   Description : Functions for storing the representation of netlists
-}
module Parser.Netlist.Functions.Stores
   ( newPackage
   , convertPackageToGenericUnit
   , convertPackageToScope
   , isNameInUnit
   , isEnumNameInUnit
   ) where

import qualified Data.Map.Strict as MapS
import Data.List (nub)
import Data.Maybe (isJust)

import Parser.Netlist.Types.Representation
         ( Function(..)
         , Designator(..)
         , Type(..)
         , Enumerate(..)
         )
import Parser.Netlist.Types.Stores
         ( Package(..)
         , ScopeStore(..)
         , UnitStore(..)
         , TypeStore
         , SubtypeStore
         , FunctionStore
         , ConstantStore
         , SignalStore
         )

-- |New package with scope
newPackage :: ScopeStore -> Package
newPackage scope = Package scope MapS.empty MapS.empty MapS.empty MapS.empty MapS.empty

-- |Convert package to generalised store for use in low level conversions
convertPackageToGenericUnit :: Package -> (ScopeStore,UnitStore)
convertPackageToGenericUnit (Package scope funcs types subtypes consts signals) =
   let unit = UnitStore funcs types subtypes consts signals
   in (scope,unit)

-- |Convert package to scope object
convertPackageToScope :: Package -> ScopeStore
convertPackageToScope (Package _ funcs types subtypes consts signals) =
   ScopeStore funcs types subtypes consts signals

-- |Check if a name exists in the unit
isNameInUnit :: UnitStore -> String -> Bool
isNameInUnit = isNameDeclaredInUnit True

-- |Check if free to use enum name
-- Ignores already defined unit names because overloading is permitted
isEnumNameInUnit :: UnitStore -> String -> Bool
isEnumNameInUnit = isNameDeclaredInUnit False

-- |Check if an identifier has already been defined in the current unit
isNameDeclaredInUnit :: Bool -> UnitStore -> String -> Bool
isNameDeclaredInUnit includeEnums (UnitStore funcs types subtypes consts signals) name =
   let allNames =
         getFunctionNames funcs
         ++ (getSomeTypeNames includeEnums types)
         ++ getSubtypeNames subtypes
         ++ getConstantNames consts
         ++ getSignalNames signals
   in elem name allNames

-- |Get names of identifiable functions
-- (Non-operator functions)
getFunctionNames :: FunctionStore -> [String]
getFunctionNames = nub . (map extractFuncName) . (filter filterFuncs) . MapS.keys
   where filterFuncs (Function (Designator_Operator _) _ _) = False
         filterFuncs _ = True
         extractFuncName (Function (Designator_Identifier str) _ _) = str

-- |Get names from types (including enumerates and physical types)
getAllTypeNames :: TypeStore -> [String]
getAllTypeNames = getSomeTypeNames True

-- |Get names from types (including physical types, but no enumerates)
getTypeNamesNoEnums :: TypeStore -> [String]
getTypeNamesNoEnums = getSomeTypeNames False

-- |Get names from types (including optional enumerates, and physical types)
getSomeTypeNames :: Bool -> TypeStore -> [String]
getSomeTypeNames includeEnums = concat . (map convertTypes) . MapS.toList
   where convertTypes :: (String,Type) -> [String]
         convertTypes (name,EnumerationType enums) =
            let enumNames = nub $ map (\(Just str) -> str) $ filter (\s -> isJust s) $ map extractEnumNames enums
            in if includeEnums
                  then (name:enumNames)
                  else [name]
         convertTypes (name,PhysicalType _ baseUnit otherUnits) = (name:baseUnit:MapS.keys otherUnits)
         convertTypes (name,_) = [name]
         extractEnumNames :: Enumerate -> Maybe String
         extractEnumNames (Enum_Identifier str) = Just str
         extractEnumNames _ = Nothing

-- |Get names of subtypes
getSubtypeNames :: SubtypeStore -> [String]
getSubtypeNames = MapS.keys

-- |Get names of constants
getConstantNames :: ConstantStore -> [String]
getConstantNames = MapS.keys

-- |Get names of signals
getSignalNames :: SignalStore -> [String]
getSignalNames = MapS.keys
