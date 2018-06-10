module Main (main) where

import Test.Tasty

import qualified Lexer.Spec
import qualified Parser.Spec

main :: IO ()
main = defaultMain tests

-- |Top level test group
tests :: TestTree
tests = testGroup "Top level test group"
   [ Lexer.Spec.tests
   , Parser.Spec.tests
   ]
