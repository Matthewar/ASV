{-|
   Module      : Parser.Alex.Monad
   Description : Alex specific lexer monad type

   Adapted from Alex default wrapper file.
   See [Alex](https://www.haskell.org/alex/)
|-}
module Parser.Alex.Monad where

import qualified Control.Applicative as App (Applicative (..))
import Control.Monad.Trans.State (StateT)

import Parser.Alex.BaseTypes
import Parser.ErrorTypes (WrappedParserError)
import Parser.TokenTypes (WrappedToken)

type Alex a = StateT AlexState (Either WrappedParserError) a

-- |Type for Alex actions
type AlexAction result = AlexInput -> Int -> Alex result
