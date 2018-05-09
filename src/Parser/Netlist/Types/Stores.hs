{-|
   Module      : Parser.Netlist.Types.Stores
   Description : Types for storing the representation of netlists
-}
module Parser.Netlist.Types.Stores
   ( NetlistStore(..)
   , TypeStore
   , SubtypeStore
   , FunctionStore
   , ConstantStore
   , SignalStore
   , PackageStore
   , Package(..)
   , emptyPackage
   , ScopeStore(..)
   , emptyScopeStore
   , UnitStore(..)
   ) where

import qualified Data.Map.Strict as MapS

import Parser.Netlist.Types.Representation
         ( NetlistName
         , Type
         , Subtype
         , Function(..)
         , FunctionBody(..)
         , Constant
         , Signal
         )

-- |Store of all netlisted data
-- Parse tree is converted into netlist
data NetlistStore =
   NetlistStore
      --{ entities :: MapS.Map NetlistName Entity -- ?? ports :: [Port], generics
      --, configurations :: MapS.Map NetlistName Configuration
      { packages :: PackageStore
      --, architectures :: MapS.Map NetlistName Architecture
      }
   deriving (Show)

-- |Store of type data
type TypeStore = MapS.Map String Type

-- |Store of subtype data
type SubtypeStore = MapS.Map String Subtype

-- |Store of function data
-- Reference using name, input types and output type
-- Contains potential body (if defined)
type FunctionStore = MapS.Map Function (Maybe FunctionBody)

-- |Store of constant data
type ConstantStore = MapS.Map String Constant

-- |Store of signal data
type SignalStore = MapS.Map String Signal

-- |Store of package data
type PackageStore = MapS.Map NetlistName Package

-- |Package data
-- All declares held in a package
data Package =
   Package
      { packageScope :: ScopeStore -- ^ Used to link package header scope to package body
      --ProcedureStore
      , packageFunctions :: FunctionStore
      , packageTypes :: TypeStore
      , packageSubtypes :: SubtypeStore
      , packageConstants :: ConstantStore
      , packageSignals :: SignalStore
      --FileStore
      --AliasStore
      --ComponentStore
      --AttributeStore
      --DisconnectStore
      --UseStore

   -- ?? Maybe PackageBodyStore
      --Subprogram
      --Type
      --Subtype
      --Constant
      --File
      --Alias
      --Use
      }
   deriving (Show)

-- |Empty package
emptyPackage :: Package
emptyPackage =
   Package
      emptyScopeStore
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty

-- |Special stores for scoped declarations
-- Scoped items need to associate their package entity
-- This is used to import the correct libraries
type ScopeExtraStore a = MapS.Map a NetlistName

-- Extra stores
type ScopeFunctionStore = ScopeExtraStore Function
type ScopeTypeStore = ScopeExtraStore String
type ScopeSubtypeStore = ScopeExtraStore String
type ScopeConstantStore = ScopeExtraStore String
type ScopeSignalStore = ScopeExtraStore String

-- |Store of scoped declarations
data ScopeStore =
   ScopeStore
      { scopeFunctions :: FunctionStore
      , scopeFunctionPackage :: ScopeFunctionStore
      , scopeTypes :: TypeStore
      , scopeTypePackage :: ScopeTypeStore
      , scopeSubtypes :: SubtypeStore
      , scopeSubtypePackage :: ScopeSubtypeStore
      , scopeConstants :: ConstantStore
      , scopeConstantPackage :: ScopeConstantStore
      , scopeSignals :: SignalStore
      , scopeSignalPackage :: ScopeSignalStore
      }
   deriving (Show)

-- |Empty real scope store
emptyScopeStore :: ScopeStore
emptyScopeStore =
   ScopeStore
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty

-- |Store of unit declarations
data UnitStore =
   UnitStore
      { unitFunctions :: FunctionStore
      , unitTypes :: TypeStore
      , unitSubtypes :: SubtypeStore
      , unitConstants :: ConstantStore
      , unitSignals :: SignalStore
      }