module Lexer.Functions
   ( getLexResult
   , compareBasicUnit
   ) where

import Test.Tasty.HUnit
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.State (evalStateT)

import Lexer.Lexer (lexerList)
import Lexer.Types.Token

import Parser.Netlist.Types.Stores (emptyNetlistStore)
import Manager.Types.Error (ConverterError(ConverterError_Parse))

-- |Run the lexer and get the result
getLexResult :: String -> IO (Either ConverterError [Token])
getLexResult input = runExceptT $ evalStateT (lexerList input) emptyNetlistStore

-- |Compare a single unit input
compareBasicUnit :: String -> Token -> Assertion
compareBasicUnit input output = do
   result <- getLexResult input
   let expectedOutput = Right [output]
   result @?= expectedOutput
