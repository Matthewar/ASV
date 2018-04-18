{-|
   Module      : Parser.Functions.Monad
   Description : Parser stack manipulation functions
|-}
module Parser.Functions.Monad
   ( getToken
   , saveToken
   ) where

import Control.Monad.Trans.State
         ( get
         , modify
         )
import Control.Monad.Except (lift)

import Lexer.Lexer (alexMonadScan)
import Lexer.Types.Token (WrappedToken)
import Parser.Types.Monad
         ( ParserState
         , ParserStack
         )

-- |Get a new token
getToken :: ParserStack WrappedToken
getToken = do
   tokensInPlay <- get
   if null tokensInPlay
      then lift alexMonadScan
      else do
         modify tail
         return $ head tokensInPlay

saveToken :: WrappedToken -> ParserStack ()
saveToken token = modify (\list -> token:list)
