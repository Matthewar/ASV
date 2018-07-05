module Main (main) where

import Test.Tasty

import qualified Spec.Parser.Types.Token
import qualified Spec.Parser.Combinators.ReservedWords
import qualified Spec.Parser.Combinators.Lex

main :: IO ()
main = defaultMain tests

-- |Top level test group
tests :: TestTree
tests = testGroup "Top level test group"
   [ parserTests
   ]

-- |Tests for the "Parser" module
parserTests :: TestTree
parserTests = testGroup "Parser tests"
   [ Spec.Parser.Types.Token.tests
   , Spec.Parser.Combinators.ReservedWords.tests
   , Spec.Parser.Combinators.Lex.tests
   ]
