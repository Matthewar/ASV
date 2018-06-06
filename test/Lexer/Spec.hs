module Lexer.Spec (tests) where

import Test.Tasty

import Lexer.SingleWords (singleWords)

-- |All lexer tests
tests :: TestTree
tests = testGroup "Lexer Tests"
   [singleWords]
