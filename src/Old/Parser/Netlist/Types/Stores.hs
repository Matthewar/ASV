{-|
   Module      : Parser.Netlist.Types.Stores
   Description : Types for storing the representation of netlists
-}
module Parser.Netlist.Types.Stores
   ( NetlistStore(..)
   , emptyNetlistStore
   , TypeStore
   , SubtypeStore
   , FunctionStore
   , ConstantStore
   , SignalStore
   , GenericStore
   , PortStore
   , EntityStore
   , Entity(..)
   , emptyEntity
   , PackageStore
   , Package(..)
   , emptyPackage
   , ProcessStore
   , Process(..)
   , emptyProcess
   , ArchitectureStore
   , Architecture(..)
   , emptyArchitecture
   , ScopeStore(..)
   , emptyScopeStore
   , UnitStore(..)
   , emptyUnitStore
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
         , Generic
         , Port
         , SequentialStatement
         )

-- |Store of all netlisted data
-- Parse tree is converted into netlist
data NetlistStore =
   NetlistStore
      { entities :: EntityStore
      --, configurations :: MapS.Map NetlistName Configuration
      , packages :: PackageStore
      , architectures :: ArchitectureStore
      }
   deriving (Show)

emptyNetlistStore :: NetlistStore
emptyNetlistStore =
   NetlistStore
      MapS.empty
      MapS.empty
      MapS.empty

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

-- |Store of generics
-- ?? How to deal with positional and named keying
type GenericStore = [Generic]

-- |Store of ports
-- ?? How to deal with positional and named keying
type PortStore = [Port]

-- |Store of entity data
type EntityStore = MapS.Map NetlistName Entity

-- |Entity data
-- All declares and statements held in an entity along with its interface
data Entity =
   Entity
      { entityScope :: ScopeStore -- ^ Used to link entity scope to architecture body
      , entityGenerics :: GenericStore
      , entityPorts :: PortStore
      --ProcedureStore
      , entityFunctions :: FunctionStore
      , entityTypes :: TypeStore
      , entitySubtypes :: SubtypeStore
      , entityConstants :: ConstantStore
      , entitySignals :: SignalStore
      --FileStore
      --AliasStore
      --AttributeStore
      --DisconnectStore
      --UseStore

      --Concurrent assertion statement
      --Passive concurrent procedure call
      , entityProcesses :: ProcessStore
      }
   deriving (Show)

-- |Empty package
emptyEntity :: Entity
emptyEntity =
   Entity
      emptyScopeStore
      []
      []
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty

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

-- |Store of process data
type ProcessStore = MapS.Map String Process

-- |Process data
-- All declares held in a process
data Process =
   Process
      --{ processScope
      --ProcedureStore
      { processFunctions :: FunctionStore
      , processTypes :: TypeStore
      , processSubtypes :: SubtypeStore
      , processConstants :: ConstantStore
      --VariableStore
      --FileStore
      --AliasStore
      --AttributeStore
      --UseStore
      , processStatements :: [SequentialStatement]
      }
   deriving (Show)

emptyProcess :: Process
emptyProcess =
   Process
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      []

-- |Store of architecture data
-- (String,String,String)
-- - Library Name
-- - Entity Name
-- - Architecture Name
type ArchitectureStore = MapS.Map (String,String,String) Architecture

-- |Architecture data
-- All declares and statements held in an architecture along with its interface
data Architecture =
   Architecture
      { archScope :: ScopeStore
      , archGenerics :: GenericStore
      , archPorts :: PortStore
      --ProcedureStore
      , archFunctions :: FunctionStore
      , archTypes :: TypeStore
      , archSubtypes :: SubtypeStore
      , archConstants :: ConstantStore
      , archSignals :: SignalStore
      --FileStore
      --AliasStore
      --ComponentStore
      --AttributeStore
      --ConfigurationStore
      --DisconnectStore
      --UseStore

      --block statement
      , archProcesses :: ProcessStore
      --concurrent procedure call
      --concurrent assertion statement
      --concurrent signal assignment statement
      --component instantiation statement
      --generate statement
      }
   deriving (Show)

-- |Empty package
emptyArchitecture :: Architecture
emptyArchitecture =
   Architecture
      emptyScopeStore
      []
      []
      MapS.empty
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
      { unitGenerics :: GenericStore
      , unitPorts :: PortStore
      , unitFunctions :: FunctionStore
      , unitTypes :: TypeStore
      , unitSubtypes :: SubtypeStore
      , unitConstants :: ConstantStore
      , unitSignals :: SignalStore
      , unitProcesses :: ProcessStore
      }

-- |Empty unit store
emptyUnitStore :: UnitStore
emptyUnitStore =
   UnitStore
      []
      []
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
      MapS.empty
