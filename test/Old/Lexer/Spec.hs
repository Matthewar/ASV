module Lexer.Spec (tests) where

import Test.Tasty

import Lexer.SingleWords (singleWords)
import Lexer.Sentences (sentences)

-- |All lexer tests
tests :: TestTree
tests = testGroup "Lexer Tests"
   [ singleWords
   , sentences
   ]
