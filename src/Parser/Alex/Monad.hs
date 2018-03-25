{-|
   Module      : Parser.Alex.Monad
   Description : Alex specific lexer monad type

   Adapted from Alex default wrapper file.
   See [Alex](https://www.haskell.org/alex/)
|-}
module Parser.Alex.Monad where

import qualified Control.Applicative as App (Applicative (..))

import Parser.Alex.BaseTypes
import Parser.ErrorTypes (WrappedParserError)

newtype Alex a = Alex { unAlex :: AlexState -> Either WrappedParserError (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left err -> Left err
                            Right (s', a') -> Right (s', f a')

instance App.Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left err -> Left err
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left err -> Left err
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

-- |Type for Alex actions
type AlexAction result = AlexInput -> Int -> Alex result
