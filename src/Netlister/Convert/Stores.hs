{-|
   Module      : Netlister.Convert.Stores
   Description : Functions for storing the representation of netlists
-}
module Netlister.Convert.Stores
   ( newPackage
   , convertPackageToGenericUnit
   , convertPackageToScope
   , isNameDeclaredInUnit
   ) where

import qualified Data.Map.Strict as MapS

import Netlister.Types.Representation
         ( Function(..)
         , Designator(..)
         )
import Netlister.Types.Stores
         ( Package(..)
         , ScopeStore(..)
         , UnitStore(..)
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

-- |Check if an identifier has already been defined in the current unit
isNameDeclaredInUnit :: UnitStore -> String -> Bool
isNameDeclaredInUnit (UnitStore funcs types subtypes consts signals) name =
   let filterFuncs (Function (Designator_Operator _) _ _) = False
       filterFuncs _ = True
       extractFuncName (Function (Designator_Identifier str) _ _) = str
       allNames =
         map extractFuncName (filter filterFuncs $ MapS.keys funcs)
         ++ MapS.keys types
         ++ MapS.keys subtypes
         ++ MapS.keys consts
         ++ MapS.keys signals
   in elem name allNames
