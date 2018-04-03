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
   , WrappedNewDeclarationScope
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
   -- Also information on positioning used for error messages
   IncludedDeclares [(DeclarationNames,AlexPosn)]
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

-- |Wrapped new declaration
type WrappedNewDeclarationScope = PosnWrapper NewDeclarationScope

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
