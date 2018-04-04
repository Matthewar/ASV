{-|
   Module      : Netlister.Types.Scope
   Description : Types for the scope of declarations

   Scope details for declarations.
   Used for includes (library and use statements).
-}
module Netlister.Types.Scope
   ( Scope(..)
   , UnitScope
   , DeclarationScopeItem(..)
   , WrappedDeclarationScopeItem
   , NewScopeDeclares(..)
   , WrappedNewScopeDeclares
   , ScopeConverterError(..)
   , WrappedScopeConverterError
   , ScopeReturn
   ) where

import Netlister.Types.Operators (Operator)
import Parser.PositionWrapper
import Parser.Alex.BaseTypes (AlexPosn)
import Parser.Happy.Types
         ( SelectedName
         )
import Netlister.Types.Stores
         ( NetlistName
         , FunctionStore
         , ScopeStore
         )
import Netlister.Types.Representation (Type)

import qualified Data.Map.Strict as MapS
import Control.Monad.Trans.State (StateT)

-- |Current scope
-- Library Name, Included Objects
data Scope = Scope { scopeLibraries :: [String], scopeDeclarations :: [UnitScope] }

-- |Unit scope
-- Unit name, included element(s)
type UnitScope = (NetlistName,WrappedDeclarationScopeItem)

-- |Wrapped declaration
type WrappedDeclarationScopeItem = PosnWrapper DeclarationScopeItem

-- |Single declaration to be included from the package
data DeclarationScopeItem =
   -- |Single named declaration
   Declare_Identifier String
   -- |Single operator declaration
   | Declare_Operator Operator
   -- |All declares included from unit
   | Declare_All
   deriving (Eq)

-- |Declaration(s) to be added to the scope
data NewScopeDeclares =
   -- |Functions to be added
   NewScopeDeclare_Functions FunctionStore
   -- |Type to be added
   | NewScopeDeclare_Type String Type
   -- |Entire package ('Declare_All' has occured) to be added
   | NewScopeDeclare_Package ScopeStore

-- |Wrapped new scope declarations
type WrappedNewScopeDeclares = PosnWrapper NewScopeDeclares

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
   -- |Declare is not in included package
   -- ?? Need package name in this error as well
   | ScopeConverterError_InvalidDeclare String
   -- |Operator declare is not in included package
   -- ?? Need package name in this error as well
   | ScopeConverterError_InvalidOpDeclare Operator

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
   show (ScopeConverterError_InvalidDeclare declareName) =
      "the declare: \""
      ++ declareName
      ++ "\" can not be found in the package: \""
      -- ?? Insert package name here
      ++ "\""
   show (ScopeConverterError_InvalidOpDeclare op) =
      "the operator declare: \""
      ++ show op
      ++ "\" can not be found in the package: \""
      -- ?? Insert package name here
      ++ "\""

-- |'ScopeConverterError' with position
type WrappedScopeConverterError = PosnWrapper ScopeConverterError

-- |Scope return type
type ScopeReturn a = StateT Scope (Either WrappedScopeConverterError) a
