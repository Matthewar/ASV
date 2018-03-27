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

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Data.Function ((&))
import Control.Monad (replicateM)

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

-- |Generate an empty VHDL entity declaration
-- Pass name, whether optional parts are included
genEmptyEntity :: String -> Bool -> Bool -> QC.Gen String
genEmptyEntity entityName statementPart secondLabel = do
   let space = do
         numSpaces <- QC.elements [1..10]
         replicateM numSpaces $ QC.elements "\n\t "
   space1 <- space
   space2 <- space
   space3 <- space
   space4 <- space
   let part1 = "entity" ++ space1 ++ entityName ++ space2 ++ "is" ++ space3
       part3 = "end" ++ space4
       part5 = ";"
   part2 <- if statementPart then do
               space5 <- space
               return $ "begin" ++ space5
            else return ""
   part4 <- if secondLabel then do
               space6 <- space
               return $ entityName ++ space6
            else return ""
   return $ part1 ++ part2 ++ part3 ++ part4 ++ part5

-- |Generate a VHDL specific identifier
-- > identifier ::= letter { [ underline ] letter_or_digit }
-- Implemented as:
-- @
--    'genIdentifier' ::= letter { 'genUnderscoreLetterOrDigit' }
--    'genUnderscoreLetterOrDigit' ::= [ underline ] letter_or_digit
-- @
genIdentifier :: Int -> Int -> QC.Gen String
genIdentifier fromLength toLength = do
   letter <- QC.elements letters
   lengthStr <- QC.elements [fromLength..toLength]
   otherLetters <- replicateM lengthStr genUnderscoreLetterOrDigit
   return (letter:concat otherLetters)
   where letters = ['a'..'z'] ++ ['A'..'Z']
         letters_or_digits = ['0'..'9'] ++ letters
         genUnderscoreLetterOrDigit :: QC.Gen String
         genUnderscoreLetterOrDigit = do
            optionalUnderscore <- QC.elements [True,False]
            letter_or_digit <- QC.elements letters_or_digits
            return $
               if optionalUnderscore then ['_',letter_or_digit]
               else [letter_or_digit]
