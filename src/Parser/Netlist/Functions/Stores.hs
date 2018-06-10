{-|
   Module      : Parser.Netlist.Convert.Stores
   Description : Functions for storing the representation of netlists
-}
module Parser.Netlist.Functions.Stores
   ( newPackage
   , newEntity
   , newArchitecture
   , convertPackageToGenericUnit
   , convertEntityToGenericUnit
   , convertArchitectureToGenericUnit
   , convertProcessToGenericUnit
   , convertPackageToScope
   , isNameInUnit
   , isEnumNameInUnit
   , isNameDeclaredInUnit
   , matchTypeNameInScope
   , matchSubtypeNameInScope
   , matchFunctionInScope
   , matchFunctionNameInScope
   , matchEnumNameInScope
   , matchEnumCharInScope
   , matchPhysicalUnitInScope
   , matchConstantNameInScope
   , matchSignalNameInScope
   , matchPortNameInScope
   , mergeUnits
   , mergeScopes
   ) where

import qualified Data.Map.Strict as MapS
import Data.List
         ( nub
         , find
         )
import Data.Maybe (isJust)

import Parser.Netlist.Types.Operators (Operator)
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
         , Generic(..)
         , Port(..)
         )
import Parser.Netlist.Types.Stores
         ( Package(..)
         , emptyPackage
         , Entity(..)
         , emptyEntity
         , Architecture(..)
         , emptyArchitecture
         , Process(..)
         , ScopeStore(..)
         , UnitStore(..)
         , emptyUnitStore
         , TypeStore
         , SubtypeStore
         , FunctionStore
         , ConstantStore
         , SignalStore
         , GenericStore
         , PortStore
         , ProcessStore
         )

-- |New package with scope
newPackage :: ScopeStore -> Package
newPackage scope = emptyPackage { packageScope = scope }

-- |New entity with scope
newEntity :: ScopeStore -> Entity
newEntity scope = emptyEntity { entityScope = scope }

-- |New architecture with with associated entity
newArchitecture :: ScopeStore -> Entity -> Architecture
newArchitecture newScope (Entity entityScope generics ports funcs types subtypes consts signals processes) =
   emptyArchitecture
      { archScope = mergeScopes newScope entityScope
      , archGenerics = generics
      , archPorts = ports
      , archFunctions = funcs
      , archTypes = types
      , archSubtypes = subtypes
      , archConstants = consts
      , archSignals = signals
      , archProcesses = processes
      }

-- |Convert package to generalised store for use in low level conversions
convertPackageToGenericUnit :: Package -> (ScopeStore,UnitStore)
convertPackageToGenericUnit (Package scope funcs types subtypes consts signals) =
   let unit = emptyUnitStore
               { unitFunctions = funcs
               , unitTypes = types
               , unitSubtypes = subtypes
               , unitConstants = consts
               , unitSignals = signals
               }
   in (scope,unit)

-- |Convert package to generalised store for use in low level conversions
convertEntityToGenericUnit :: Entity -> (ScopeStore,UnitStore)
convertEntityToGenericUnit (Entity scope generics ports funcs types subtypes consts signals processes) =
   let unit = UnitStore generics ports funcs types subtypes consts signals processes
   in (scope,unit)

-- |Convert architecture to generalised store for use in low level conversions
convertArchitectureToGenericUnit :: Architecture -> (ScopeStore,UnitStore)
convertArchitectureToGenericUnit (Architecture scope generics ports funcs types subtypes consts signals processes) =
   let unit = UnitStore generics ports funcs types subtypes consts signals processes
   in (scope,unit)

convertProcessToGenericUnit :: Process -> UnitStore
convertProcessToGenericUnit (Process funcs types subtypes consts _) =
   emptyUnitStore
      { unitFunctions = funcs
      , unitTypes = types
      , unitSubtypes = subtypes
      , unitConstants = consts
      }

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
isNameDeclaredInUnit includeEnums (UnitStore generics ports funcs types subtypes consts signals processes) name =
   let allNames =
         getGenericNames generics
         ++ getPortNames ports
         ++ getFunctionNames funcs
         ++ (getSomeTypeNames includeEnums types)
         ++ getSubtypeNames subtypes
         ++ getConstantNames consts
         ++ getSignalNames signals
         ++ getProcessNames processes
   in elem name allNames

-- |Get names of generics
getGenericNames :: GenericStore -> [String]
getGenericNames = map generic_name

-- |Get names of ports
getPortNames :: PortStore -> [String]
getPortNames = map port_name

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

-- |Get names of processes
getProcessNames :: ProcessStore -> [String]
getProcessNames = MapS.keys

-- |Check if type name is within scope
-- If not, returns Nothing
-- If it is, returns tuple of:
-- - Type data
-- - Package that the type exists in
matchTypeNameInScope :: ScopeStore -> UnitStore -> NetlistName -> String -> Maybe (Type,NetlistName)
matchTypeNameInScope scope unit unitName name =
   case MapS.lookup name $ unitTypes unit of
      Just typ -> Just (typ,unitName)
      Nothing ->
         case MapS.lookup name $ scopeTypes scope of
            Just typ -> Just (typ,(scopeTypePackage scope) MapS.! name)
            Nothing -> Nothing

-- |Check if subtype name is within scope
-- If not, returns Nothing
-- If it is, returns tuple of:
-- - Subtype data
-- - Package that the subtype exists in
matchSubtypeNameInScope :: ScopeStore -> UnitStore -> NetlistName -> String -> Maybe (Subtype,NetlistName)
matchSubtypeNameInScope scope unit unitName name =
   case MapS.lookup name $ unitSubtypes unit of
      Just subtype -> Just (subtype,unitName)
      Nothing ->
         case MapS.lookup name $ scopeSubtypes scope of
            Just subtype -> Just (subtype,(scopeSubtypePackage scope) MapS.! name)
            Nothing -> Nothing

-- |Check if function is within scope
-- If not, returns Nothing
-- If it is, returns customised function map:
-- - Function keys (as normal)
-- - Value is tuple of
--    - Function body (normal function store value)
--    - Package that the function exists in
-- Pass in search function to check 'Function' key.
matchFunctionInScope :: (Function -> Bool) -> ScopeStore -> UnitStore -> NetlistName -> Maybe (MapS.Map Function (Maybe FunctionBody,NetlistName))
matchFunctionInScope funcCheck scope unit unitName =
   let filterFunction function _ = funcCheck function
       validUnitFuncs = MapS.map (\body -> (body,unitName)) $ MapS.filterWithKey filterFunction $ unitFunctions unit
       validScopeFuncs =
         let zipFunction (function,body) packageName = (function,(body,packageName))
             scopeDataList = MapS.toList $ scopeFunctions scope
             scopePackageList = MapS.elems $ scopeFunctionPackage scope
         in MapS.filterWithKey filterFunction $ MapS.fromList $ zipWith zipFunction scopeDataList scopePackageList
   in case MapS.union validUnitFuncs validScopeFuncs of
         emptyMap | MapS.null emptyMap -> Nothing
         nonEmpty -> Just nonEmpty

-- |Check if function name is within scope (ignoring types/arguments)
-- If not, returns Nothing
-- If it is, returns customised function map:
-- - Function keys (as normal)
-- - Value is tuple of
--    - Function body (normal function store value)
--    - Package that the function exists in
matchFunctionNameInScope :: ScopeStore -> UnitStore -> NetlistName -> String -> Maybe (MapS.Map Function (Maybe FunctionBody,NetlistName))
matchFunctionNameInScope scope unit unitName name =
   let functionFinder (Function (Designator_Identifier str) _ _ _) = name == str
       functionFinder _ = False
   in matchFunctionInScope functionFinder scope unit unitName

-- |Check if enumerate value is within scope
-- If not, returns Nothing
-- If it is, returns customised type store:
-- - Type name keys (as normal)
-- - Value is tuple of
--    - Type data (normal type store value)
--    - Package that the enum type exists in
matchEnumValueInScope :: Enumerate -> ScopeStore -> UnitStore -> NetlistName -> Maybe (MapS.Map String (Type,NetlistName))
matchEnumValueInScope enumValue scope unit unitName =
   let isEnumNameInType (EnumerationType enums) = elem enumValue enums
       isEnumNameInType _ = False
       runFilter = MapS.filter isEnumNameInType
       validUnitEnums = MapS.map (\typ -> (typ,unitName)) $ runFilter $ unitTypes unit
       validScopeEnums =
         let mapFunction typeName typ = (typ,(scopeTypePackage scope) MapS.! typeName)
         in MapS.mapWithKey mapFunction $ runFilter $ scopeTypes scope
   in case MapS.union validUnitEnums validScopeEnums of
         emptyMap | MapS.null emptyMap -> Nothing
         nonEmpty -> Just nonEmpty

matchEnumNameInScope :: String -> ScopeStore -> UnitStore -> NetlistName -> Maybe (MapS.Map String (Type,NetlistName))
matchEnumNameInScope name = matchEnumValueInScope (Enum_Identifier name)
matchEnumCharInScope :: Char -> ScopeStore -> UnitStore -> NetlistName -> Maybe (MapS.Map String (Type,NetlistName))
matchEnumCharInScope char = matchEnumValueInScope (Enum_Char char)

matchPhysicalUnitInScope :: ScopeStore -> UnitStore -> NetlistName -> String -> Maybe ((String,Type),NetlistName)
matchPhysicalUnitInScope scope unit unitName name =
   case filter filterTypes $ MapS.toList $ unitTypes unit of
      [(typeName,typeData)] -> Just ((typeName,typeData),unitName)
      [] ->
         case filter filterTypes $ MapS.toList $ scopeTypes scope of
            [(typeName,typeData)] -> Just ((typeName,typeData),(scopeTypePackage scope) MapS.! typeName)
            [] -> Nothing
   where filterTypes :: (String,Type) -> Bool
         filterTypes (typeName,PhysicalType baseUnit otherUnits) = baseUnit == name || MapS.member name otherUnits
         filterTypes _ = False

-- |Check if constant name is within scope
-- If not, returns Nothing
-- If it is, returns constant data
-- 
-- Constants are static ?? so don't need package name
matchConstantNameInScope :: ScopeStore -> UnitStore -> NetlistName -> String -> Maybe (Constant,NetlistName)
matchConstantNameInScope scope unit unitName name =
   case MapS.lookup name $ unitConstants unit of
      Just const -> Just (const,unitName)
      Nothing ->
         case MapS.lookup name $ scopeConstants scope of
            Just const -> Just (const,(scopeConstantPackage scope) MapS.! name)
            Nothing -> Nothing

-- |Check if signal name is within scope
-- If not, returns Nothing
-- If it is, returns tuple of:
-- - Signal data
-- - Package that the signal exists in
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

-- |Check if port name is within scope
-- If not, returns Nothing
-- If it is, returns port data
matchPortNameInScope :: UnitStore -> String -> Maybe Port
matchPortNameInScope unit name = findPort $ unitPorts unit
   where portCheck port = name == port_name port
         findPort = find portCheck

--matchVariableNameInScope :: ScopeStore -> UnitStore -> String -> Maybe (Type,VariableValue)

-- | Merge unit stores
mergeUnits :: UnitStore -> UnitStore -> UnitStore
mergeUnits unit1 unit2 = -- No overlap checks required
   let unionStore unionFunc = MapS.union (unionFunc unit1) (unionFunc unit2)
       unionList unionFunc = (++) (unionFunc unit1) (unionFunc unit2)
   in UnitStore
         (unionList unitGenerics)
         (unionList unitPorts)
         (unionStore unitFunctions)
         (unionStore unitTypes)
         (unionStore unitSubtypes)
         (unionStore unitConstants)
         (unionStore unitSignals)
         (unionStore unitProcesses)

-- | Merge scope stores
mergeScopes :: ScopeStore -> ScopeStore -> ScopeStore
mergeScopes scope1 scope2 =
   let unionStore unionFunc = MapS.union (unionFunc scope1) (unionFunc scope2)
   in ScopeStore
         (unionStore scopeFunctions)
         (unionStore scopeFunctionPackage)
         (unionStore scopeTypes)
         (unionStore scopeTypePackage)
         (unionStore scopeSubtypes)
         (unionStore scopeSubtypePackage)
         (unionStore scopeConstants)
         (unionStore scopeConstantPackage)
         (unionStore scopeSignals)
         (unionStore scopeSignalPackage)
