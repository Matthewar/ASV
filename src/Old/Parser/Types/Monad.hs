{-|
   Module      : Parser.Types.Monad
   Description : Parser stack types

   Includes parser state
|-}
module Parser.Types.Monad
   ( ParserState
   , ParserStack
   , ScopeStack
   , BuildScopeStack
   ) where

import Control.Monad.Trans.State (StateT)
import Control.Monad.Except (ExceptT)

import Lexer.Types.Token (WrappedToken)
import Lexer.Alex.Types (AlexState)
import Parser.Netlist.Types.Stores
         ( NetlistStore
         , ScopeStore
         )
import Parser.Netlist.Types.Scope (Scope)
import Manager.Types.Error (ConverterError)

-- |Current state of parser
type ParserState = [WrappedToken]

-- |Parser monad
-- Contains access to tokens in play
type ParserStack a = StateT ParserState (StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO))) a

-- |Scope monad
-- Temporary access to scope names being parsed
type ScopeStack a = StateT Scope (StateT ParserState (StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO)))) a

-- |Build scope monad
-- Temporary access to real scope being built from stores
-- Scoped conversions are converted modules required by the scope
-- Secondary state stores scoped conversions
type BuildScopeStack = StateT ScopeStore (StateT NetlistStore (ExceptT ConverterError IO)) ()
