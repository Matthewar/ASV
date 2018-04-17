{-|
   Module      : Parser.Alex.Monad
   Description : Alex specific lexer monad type

   Adapted from Alex default wrapper file.
   See [Alex](https://www.haskell.org/alex/)
|-}
module Lexer.Types.Monad where

import Control.Monad.Trans.State (StateT)
import Control.Monad.Except (ExceptT)

import Lexer.Types.Token (WrappedToken)
import Lexer.Alex.Types
         ( AlexState
         , AlexInput
         )
import Parser.Netlist.Types.Stores (NetlistStore)
import Manager.Types.Error (ConverterError)

type Alex a = StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO)) a

-- |Type for Alex actions
type AlexAction result = AlexInput -> Int -> Alex result
