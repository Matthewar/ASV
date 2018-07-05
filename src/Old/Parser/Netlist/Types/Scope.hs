{-|
   Module      : Parser.Netlist.Types.Scope
   Description : Types for the scope of declarations

   Scope details for declarations.
   Used for includes (library and use statements).
-}
module Parser.Netlist.Types.Scope
   ( Scope(..)
   , UnitScope
   , DeclarationScopeItem(..)
   , WrappedDeclarationScopeItem
   , NewScopeDeclares(..)
   , WrappedNewScopeDeclares
   , ScopeConverterError(..)
   , WrappedScopeConverterError
   ) where

import qualified Data.Map.Strict as MapS
import Control.Monad.Trans.State (StateT)
import Data.List (intersperse)

import Lexer.Types.PositionWrapper
--import Parser.Happy.Types
--         ( SelectedName
--         )
import Parser.Netlist.Types.Representation
         ( NetlistName
         , Type
         , Subtype
         )
import Parser.Netlist.Types.Operators (Operator)
import Parser.Netlist.Types.Stores
         ( FunctionStore
         , ScopeStore
         )

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
   -- |Subtype to be added
   | NewScopeDeclare_Subtype String Subtype
   -- |Entire package ('Declare_All' has occured) to be added
   | NewScopeDeclare_Package ScopeStore

-- |Wrapped new scope declarations
type WrappedNewScopeDeclares = PosnWrapper NewScopeDeclares

data ScopeConverterError =
   -- |Invalid library
   -- Currently only IEEE is supported
   -- Eventually have a list of accepted libraries
   ScopeConverterError_InvalidLibrary String
--   -- |Invalid use clause
--   -- Cannot read selected name
--   | ScopeConverterError_UseClause SelectedName
--   -- |Correct format of selected name but final suffix is wrong type
--   -- In this case this only occurs if the suffix is a character
--   | ScopeConverterError_SuffixChar Char
   -- |Invalid operator included
   -- Final suffix is of type operator, but operator stated does not exist
   | ScopeConverterError_InvalidOperator String
   -- |Library is not in current scope
   | ScopeConverterError_LibNoScope String
   -- |Declare is not in included package
   | ScopeConverterError_InvalidDeclare String NetlistName
   -- |Operator declare is not in included package
   | ScopeConverterError_InvalidOpDeclare Operator NetlistName
   -- |Cyclic dependency between two (or more) modules
   -- Module that is already in chain, module chain
   | ScopeConverterError_CyclicDependency NetlistName [NetlistName]
   deriving (Eq)

instance (Show ScopeConverterError) where
   show (ScopeConverterError_InvalidLibrary libName) =
      "invalid library: "
      ++ libName
   --show (ScopeConverterError_UseClause selectedName) =
   --   "invalid name in use clause"
   --show (ScopeConverterError_SuffixChar char) =
   --   "invalid suffix in use clause"
   --show (ScopeConverterError_InvalidOperator opStr) =
   --   "invalid operator in use clause: \""
   --   ++ opStr
   --   ++ "\""
   --show (ScopeConverterError_LibNoScope libName) =
   --   "the library is not in the current scope: "
   --   ++ libName
   --show (ScopeConverterError_InvalidDeclare declareName packageName) =
   --   "the declare: \""
   --   ++ declareName
   --   ++ "\" can not be found in the package: \""
   --   ++ show packageName
   --   ++ "\""
   --show (ScopeConverterError_InvalidOpDeclare op packageName) =
   --   "the operator declare: \""
   --   ++ show op
   --   ++ "\" can not be found in the package: \""
   --   ++ show packageName
   --   ++ "\""
   --show (ScopeConverterError_CyclicDependency inCycle cycle) =
   --   "a cyclic dependency was detected, "
   --   ++ show (head cycle)
   --   ++ " tried to import "
   --   ++ show inCycle
   --   ++ ". But the dependency chain prevents this: "
   --   ++ (getChain inCycle cycle)

getChain :: NetlistName -> [NetlistName] -> String
getChain inCycle = (findStart inCycle) . reverse
   where findStart inCycle (lib:libs) =
            if lib == inCycle
               then printCycle (lib:libs)
               else findStart inCycle libs
         findStart _ [] = "ERROR (THIS SHOULDN'T BE HERE): Somehow getChain has gone wrong, please report."
         printCycle = concat . (intersperse " -> ") . (map show)


-- |'ScopeConverterError' with position
type WrappedScopeConverterError = PosnWrapper ScopeConverterError
