module ParserSpec (tests) where

import Parser.Alex.Functions (runAlex)
import qualified Parser.Parser as Parser
import Parser.ErrorTypes (WrappedParserError)
import Parser.Happy.Types (DesignFile)
import Parser.Alex.BaseTypes (AlexPosn(..))

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
   --[ justContext
   --, doubleJustContext
   [ emptyLibs
   ]

---- |Only library clauses
--justContext :: TestTree
--justContext = testGroup "Design file with just context clauses"
--   [ justLibraryClause
--   , basicUseClauses
--   ]
--
---- |Only a library statement
--justLibraryClause :: TestTree
--justLibraryClause = testGroup "Design file with a single library statement"
--   [ testCase "Standard library \"library std;\"" $
--      runParse "library std;" @?= Right DesignFile
--         [PosnWrapper { getPos = AlexPn 0 1 0, unPos =
--            DesignUnit
--               
--
---- |Test for multiple design units with only library clauses
--doubleJustContext :: TestTree
--doubleJustContext = testGroup "Design file with multiple sets of context clauses"
--   [ twoLibraryClauses
--   , libraryAndUseClauseMix
--   ]

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
