module Main (main) where

import Test.Tasty

import qualified LexerSpec

main :: IO ()
main = defaultMain tests

-- |Top level test group
tests :: TestTree
tests = testGroup "Top level test group" [LexerSpec.tests]
