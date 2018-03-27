module ParserSpec (tests) where

import Parser.Alex.Functions (runAlex)
import qualified Parser.Parser as Parser

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Data.Function ((&))

-- |Simple command to run string in parser
runParse :: String -> Either WrappedParserError DesignFile
runParse str = runAlex str Parser.v1987

-- |All parser tests
tests :: TestTree
tests = testGroup "Parser Tests"
   [ justContext
   , doubleJustContext
   , emptyLibs
   ]

-- |Only library clauses
justContext :: TestTree
justContext = testGroup "Design file with just context clauses"
   [ justLibraryClause
   , basicUseClauses
   ]

-- |Test for multiple design units with only library clauses
doubleJustContext :: TestTree
doubleJustContext = testGroup "Design file with multiple sets of context clauses"
   [ twoLibraryClauses
   , libraryAndUseClauseMix
   ]

-- |Basic library unit tests (library units empty)
-- Shell lib units IE no contents
emptyLibs :: TestTree
emptyLibs = testGroup "Empty library unit tests"
   [ justLib
   , singleContextAndLib
   , doubleJustLib
   , doubleContextAndLib
   , doubleContextThenLib
   , doubleLibThenContext
   ]
