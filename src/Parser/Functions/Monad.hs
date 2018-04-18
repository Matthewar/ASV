{-|
   Module      : Parser.Functions.Monad
   Description : Parser stack manipulation functions
|-}
module Parser.Functions.Monad
   ( getToken
   , saveToken
   , accessNetlist
   , modifyNetlist
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
import Parser.Netlist.Types.Stores (NetlistStore)
import Parser.Netlist.Types.Monad (NetlistStack)

-- |Get a new token
getToken :: ParserStack WrappedToken
getToken = do
   tokensInPlay <- get
   if null tokensInPlay
      then lift alexMonadScan
      else do
         modify tail
         return $ head tokensInPlay

-- |Save token that has been evaluated but will need reading again
saveToken :: WrappedToken -> ParserStack ()
saveToken token = modify (\list -> token:list)

accessNetlist :: NetlistStack a -> ParserStack a
accessNetlist = lift . lift

-- |Modify netlist
modifyNetlist :: (NetlistStore -> NetlistStore) -> ParserStack ()
modifyNetlist = accessNetlist . modify
