{-|
   Module      : Parser.Alex.Monad
   Description : Alex specific lexer monad type

   Adapted from Alex default wrapper file.
   See [Alex](https://www.haskell.org/alex/)
|-}
module Lexer.Types.Monad where

import Control.Monad.Trans.State (StateT)
import Control.Monad.Except (ExceptT)

import Lexer.Types.Error (WrappedParserError)
import Lexer.Types.Token (WrappedToken)
import Lexer.Alex.Types
         ( AlexState
         , AlexInput
         )

type Alex a = StateT AlexState (Either WrappedParserError) a

-- |Type for Alex actions
type AlexAction result = AlexInput -> Int -> Alex result
