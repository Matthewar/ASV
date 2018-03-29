{-|
   Module      : Netlister.Types.Scope
   Description : Types for the scope of declarations
-}
module Netlister.Types.Scope
   ( Scope
   , UnitScope
   , DeclarationScope(..)
   ) where

import qualified Data.Map.Strict as MapS

-- |Current scope
-- Library Name, Included Objects
type Scope = MapS.Map String UnitScope

-- |Unit scope
-- Unit name, included elements
type UnitScope = MapS.Map String DeclarationScope

data DeclarationScope =
   -- |List of declarations included from unit
   IncludedDeclares [String]
   -- |All declares included from unit
   | AllDeclares
