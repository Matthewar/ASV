module Main (main) where

import Test.Tasty

import qualified LexerSpec
--import qualified ParserSpec

main :: IO ()
main = defaultMain tests

-- |Top level test group
tests :: TestTree
tests = testGroup "Top level test group"
   [ LexerSpec.tests
   --, ParserSpec.tests
   ]
