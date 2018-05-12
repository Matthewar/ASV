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
import Data.Maybe
         ( isJust
         , fromJust
         )

import Parser.Netlist.Types.Representation
         ( NetlistName
         , Function(..)
         , FunctionBody
         , Designator(..)
         , Type(..)
         , Subtype
         , Enumerate(..)
         , Constant
         , Signal(..)
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
convertPackageToScope :: Package -> NetlistName -> ScopeStore
convertPackageToScope (Package _ funcs types subtypes consts signals) packageName =
   ScopeStore
      funcs
      (newExtraScope funcs)
      types
      (newExtraScope types)
      subtypes
      (newExtraScope subtypes)
      consts
      (newExtraScope consts)
      signals
      (newExtraScope signals)
   where newExtraScope = MapS.map (\_ -> packageName)

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
   where filterFuncs (Function (Designator_Operator _) _ _ _) = False
         filterFuncs _ = True
         extractFuncName (Function (Designator_Identifier str) _ _ _) = str

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
         convertTypes (name,PhysicalType baseUnit otherUnits) = (name:baseUnit:MapS.keys otherUnits)
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

-- |Check if type name is within scope
-- If not, returns Nothing
-- If it is, returns tuple of:
-- - Type data
-- - Package that the type exists in
--    - Nothing if directly visible
--    - package name/library if visible by selection
matchTypeNameInScope :: ScopeStore -> UnitStore -> String -> Maybe (Type,Maybe NetlistName)
matchTypeNameInScope scope unit name =
   case MapS.lookup name $ unitTypes unit of
      Just typ -> Just (typ,Nothing)
      Nothing ->
         case MapS.lookup name $ scopeTypes scope of
            Just typ -> Just (typ,Just $ (scopeTypePackage scope) MapS.! name)
            Nothing -> Nothing

-- |Check if subtype name is within scope
-- If not, returns Nothing
-- If it is, returns tuple of:
-- - Subtype data
-- - Package that the subtype exists in
--    - Nothing if directly visible
--    - package name/library if visible by selection
matchSubtypeNameInScope :: ScopeStore -> UnitStore -> String -> Maybe (Subtype,Maybe NetlistName)
matchSubtypeNameInScope scope unit name =
   case MapS.lookup name $ unitSubtypes unit of
      Just subtype -> Just (subtype,Nothing)
      Nothing ->
         case MapS.lookup name $ scopeSubtypes scope of
            Just subtype -> Just (subtype,Just $ (scopeSubtypePackage scope) MapS.! name)
            Nothing -> Nothing

-- |Check if function name is within scope (ignoring types/arguments)
-- If not, returns Nothing
-- If it is, returns customised function map:
-- - Function keys (as normal)
-- - Value is tuple of
--    - Function body (normal function store value)
--    - Package that the function exists in
--       - Nothing if directly visible
--       - package name/library if visible by selection
matchFunctionNameInScope :: ScopeStore -> UnitStore -> String -> Maybe (MapS.Map Function (Maybe FunctionBody,Maybe NetlistName))
matchFunctionNameInScope scope unit name =
   let validUnitFuncs =
         let filterFunction (Function (Designator_Identifier str) _ _ _) _ = name == str
             filterFunction _ _ = False
         in MapS.map (\body -> (body,Nothing)) $ MapS.filterWithKey filterFunction $ unitFunctions unit
       validScopeFuncs =
         let zipFunction (function,body) packageName =
               case function of
                  Function (Designator_Identifier str) _ _ _ | name == str ->
                     Just (function,(body,Just packageName))
                  _ -> Nothing
             scopeDataList = MapS.toList $ scopeFunctions scope
             scopePackageList = MapS.elems $ scopeFunctionPackage scope
             filterFunction Nothing = False
             filterFunction (Just _) = True
         in MapS.fromList $ map fromJust $ filter filterFunction $ zipWith zipFunction scopeDataList scopePackageList
   in case MapS.union validUnitFuncs validScopeFuncs of
         emptyMap | MapS.null emptyMap -> Nothing
         nonEmpty -> Just nonEmpty

-- |Check if enumerate name is within scope
-- If not, returns Nothing
-- If it is, returns customised type store:
-- - Type name keys (as normal)
-- - Value is tuple of
--    - Type data (normal type store value)
--    - Package that the enum type exists in
--       - Nothing if directly visible
--       - package name/library if visible by selection
matchEnumNameInScope :: ScopeStore -> UnitStore -> String -> Maybe (MapS.Map String (Type,Maybe NetlistName))
matchEnumNameInScope scope unit name =
   let enumName = Enum_Identifier name
       isEnumNameInType (EnumerationType enums) = elem enumName enums
       isEnumNameInType _ = False
       runFilter = MapS.filter isEnumNameInType
       validUnitEnums = MapS.map (\typ -> (typ,Nothing)) $ runFilter $ unitTypes unit
       validScopeEnums =
         let mapFunction typeName typ = (typ,Just $ (scopeTypePackage scope) MapS.! typeName)
         in MapS.mapWithKey mapFunction $ runFilter $ scopeTypes scope
   in case MapS.union validUnitEnums validScopeEnums of
         emptyMap | MapS.null emptyMap -> Nothing
         nonEmpty -> Just nonEmpty

-- |Check if constant name is within scope
-- If not, returns Nothing
-- If it is, returns constant data
-- 
-- Constants are static ?? so don't need package name
matchConstantNameInScope :: ScopeStore -> UnitStore -> String -> Maybe Constant
matchConstantNameInScope scope unit name =
   case MapS.lookup name $ unitConstants unit of
      Just const -> Just const
      Nothing ->
         case MapS.lookup name $ scopeConstants scope of
            Just const -> Just const
            Nothing -> Nothing

-- |Check if signal name is within scope
-- If not, returns Nothing
-- If it is, returns tuple of:
-- - Signal data
-- - Package that the type exists in
--    - Nothing if directly visible
--    - package name/library if visible by selection
matchSignalNameInScope :: ScopeStore -> UnitStore -> String -> Maybe (Signal,Maybe NetlistName)
matchSignalNameInScope scope unit name =
   case MapS.lookup name $ unitSignals unit of
      Just sig -> Just (sig,Nothing)
      Nothing ->
         case MapS.lookup name $ scopeSignals scope of
            Just sig -> Just (sig,Just $ (scopeSignalPackage scope) MapS.! name)
            Nothing -> Nothing

--matchVariableNameInScope :: ScopeStore -> UnitStore -> String -> Maybe (Type,VariableValue)
