{-|
   Module      : Netlister.Types.Stores
   Description : Types for storing the representation of netlists
-}
module Netlister.Types.Stores
   ( NetlistName(..)
   , NetlistStore(..)
   , TypeStore
   , FunctionStore
   , PackageStore
   , Package(..)
   ) where

import qualified Data.Map.Strict as MapS

import Netlister.Types.Representation
         ( Type
         , Function(..)
         , FunctionBody(..)
         )
import Netlister.Types.Scope (Scope)

-- |Netlist name representation
-- Made up of the library the unit is in and its name
data NetlistName = NetlistName { libraryName :: String, unitName :: String }
                 deriving (Eq,Ord)

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

-- |Store of function data
-- Reference using name, input types and output type
-- Contains potential body (if defined)
type FunctionStore = MapS.Map Function (Maybe FunctionBody)

-- |Store of package data
type PackageStore = MapS.Map NetlistName Package

-- |Package data
-- All declares held in a package
data Package =
   Package
      Scope -- ^ Used to link package header scope to package body
      -- |Subprogram declaration
      --SubprogramHeaderStore ?? Instead do separately
      --ProcedureStore
      FunctionStore
      TypeStore
      --ConstantStore
      --SignalStore
      --FileStore
      --AliasStore
      --ComponentStore
      --AttributeStore
      --DisconnectStore
      --UseStore

   -- ?? Maybe PackageBodyStore
      --Subprogram
      --Type
      --Constant
      --File
      --Alias
      --Use
