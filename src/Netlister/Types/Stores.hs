{-|
   Module      : Netlister.Types.Stores
   Description : Types for storing the representation of netlists
-}
module Netlister.Types.Stores
   ( NetlistName(..)
   , NetlistStore(..)
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

import Netlister.Types.Representation
         ( Type
         , Subtype
         , Function(..)
         , FunctionBody(..)
         )
import Netlister.Types.Objects
         ( Constant
         , Signal
         )

-- |Netlist name representation
-- Made up of the library the unit is in and its name
data NetlistName = NetlistName { libraryName :: String, unitName :: String }
                 deriving (Eq,Ord)

instance (Show NetlistName) where
   show (NetlistName lib unit) = lib ++ "." ++ unit

-- |Store of all netlisted data
-- Parse tree is converted into netlist
data NetlistStore =
   NetlistStore
      --{ entities :: MapS.Map NetlistName Entity -- ?? ports :: [Port], generics
      --, configurations :: MapS.Map NetlistName Configuration
      { packages :: PackageStore
      --, architectures :: MapS.Map NetlistName Architecture
      }

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
      -- |Subprogram declaration
      --SubprogramHeaderStore ?? Instead do separately
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

-- |Store of scoped declarations
data ScopeStore =
   ScopeStore
      { scopeFunctions :: FunctionStore
      , scopeTypes :: TypeStore
      , scopeSubtypes :: SubtypeStore
      , scopeConstants :: ConstantStore
      , scopeSignals :: SignalStore
      }

-- |Empty real scope store
emptyScopeStore :: ScopeStore
emptyScopeStore =
   ScopeStore
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
