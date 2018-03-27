module ParserSpec (tests) where

import Parser.Alex.Functions (runAlex)
import qualified Parser.Parser as Parser
import Parser.ErrorTypes (WrappedParserError)
import Parser.Happy.Types
   ( DesignFile(..)
   , DesignUnit(..)
   , LibraryUnit(..)
   , PrimaryUnit(..)
   , EntityDeclaration(..)
   , EntityHeader(..)
   )
import Parser.Alex.BaseTypes (AlexPosn(..))
import Parser.PositionWrapper

import Generators.LexElements (genIdentifier)
import Generators.ParseElements (genEmptyEntity)

import Test.Tasty
--import Test.Tasty.HUnit (testCase)
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
   --, singleContextAndLib
   --, doubleJustLib
   --, doubleContextAndLib
   --, doubleContextThenLib
   --, doubleLibThenContext
   ]

-- |Single empty library units
justLib :: TestTree
justLib = testGroup "Only a library unit"
   [ emptyEntities
   --, emptyConfigurations
   --, emptyPackages
   --, emptyArchitectureBodies
   --, emptyPackageBodies
   ]

emptyEntities :: TestTree
emptyEntities = testGroup "Entity unit"
   [ compareFunc "No statement part, no label" False False
   , compareFunc "Statement part, no label" True False
   , compareFunc "No statement part, label" False True
   , compareFunc "Statement part, label" True True
   ]
   where compareFunc :: String -> Bool -> Bool -> TestTree
         compareFunc details cond1 cond2 =
            QC.testProperty details $
               QC.forAll (genEmptyEntity_wName cond1 cond2) $ \(name,statement) ->
                  case runParse statement of
                     Right tree -> matchEmptyEntity cond1 cond2 name tree
                     _ -> False
         genEmptyEntity_wName :: Bool -> Bool -> QC.Gen (String,String)
         genEmptyEntity_wName statement label = do
            name <- genIdentifier 1 30
            statement <- genEmptyEntity name statement label
            return (name,statement)
         matchEmptyEntity :: Bool -> Bool -> String -> DesignFile -> Bool
         matchEmptyEntity cond1 cond2 entityName
            (DesignFile
               [PosnWrapper
                  { unPos =
                     DesignUnit
                        []
                        PosnWrapper { getPos = _, unPos =
                           Library_PrimaryUnit
                              ( PrimaryUnit_EntityDeclaration
                                 ( EntityDeclaration
                                    PosnWrapper { unPos = name1 }
                                    (EntityHeader Nothing Nothing)
                                    []
                                    statementPart
                                    name2
                                 )
                              )
                        }
                  }
               ]
            ) = let checkOne = statementPart == if cond1 then Just [] else Nothing
                    checkTwo =
                        case name2 of
                           Just unwrappedName -> cond2 && entityName == unPos unwrappedName
                           Nothing -> not cond2
                in checkOne && checkTwo && entityName == name1
   --[ testCase "Entity Declaration: No statement part" $
   --   runParse "entity entity_name is entity_header entity_declarative_part end entity_name;"
   --, testCase "Entity Declaration: With statement part" $
   --   runParse "entity entity_name is entity_header entity_declarative_part begin entity_statement_part end entity_name;"
   --, testCase "Configuration Declaration"
   --, testCase "Package Declaration"
   --, testCase "Architecture Body"
   --, testCase "Package Body"
   --]
