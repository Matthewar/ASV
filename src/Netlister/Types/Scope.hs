{-|
   Module      : Netlister.Types.Scope
   Description : Types for the scope of declarations

   Scope details for declarations.
   Used for includes (library and use statements).
-}
module Netlister.Types.Scope
   ( Scope
   , UnitScope
   , DeclarationScope(..)
   , DeclarationNames(..)
   , NewDeclarationScope(..)
   , ScopeConverterError(..)
   , WrappedScopeConverterError
   , ScopeReturn
   ) where

import Netlister.Types.Operators (Operator)
import Parser.PositionWrapper
import Parser.Happy.Types
         ( SelectedName
         )

import qualified Data.Map.Strict as MapS
import Control.Monad.Trans.State (StateT)

-- |Current scope
-- Library Name, Included Objects
type Scope = MapS.Map String UnitScope

-- |Unit scope
-- Unit name, included elements
type UnitScope = MapS.Map String DeclarationScope

-- |Declaration scope
-- Declarations to be included from the package
data DeclarationScope =
   -- |List of declarations included from unit
   IncludedDeclares [DeclarationNames]
   -- |All declares included from unit
   | AllDeclares

-- |Single declaration to be included from the package
data DeclarationNames =
   -- |Single named declaration
   Declare_Identifier String
   -- |Single operator declaration
   | Declare_Operator Operator
   deriving (Eq)

-- |New declaration to be added to scope
data NewDeclarationScope =
   -- |Add named declaration to scope
   NewDeclare_Identifier String
   -- |Add operator declaration to scope
   | NewDeclare_Operator Operator
   -- |Add all declarations to scope
   | NewDeclare_All

data ScopeConverterError =
   -- |Invalid library
   -- Currently only IEEE is supported
   -- Eventually have a list of accepted libraries
   ScopeConverterError_InvalidLibrary String
   -- |Invalid use clause
   -- Cannot read selected name
   | ScopeConverterError_UseClause SelectedName
   -- |Correct format of selected name but final suffix is wrong type
   -- In this case this only occurs if the suffix is a character
   | ScopeConverterError_SuffixChar Char
   -- |Invalid operator included
   -- Final suffix is of type operator, but operator stated does not exist
   | ScopeConverterError_InvalidOperator String
   -- |Library is not in current scope
   | ScopeConverterError_LibNoScope String

instance (Show ScopeConverterError) where
   show (ScopeConverterError_InvalidLibrary libName) =
      "invalid library: "
      ++ libName
   show (ScopeConverterError_UseClause selectedName) =
      "invalid name in use clause"
   show (ScopeConverterError_SuffixChar char) =
      "invalid suffix in use clause"
   show (ScopeConverterError_InvalidOperator opStr) =
      "invalid operator in use clause: \""
      ++ opStr
      ++ "\""
   show (ScopeConverterError_LibNoScope libName) =
      "the library is not in the current scope: "
      ++ libName

type WrappedScopeConverterError = PosnWrapper ScopeConverterError

type ScopeReturn a = StateT Scope (Either WrappedScopeConverterError) a
