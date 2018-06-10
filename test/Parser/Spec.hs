module Parser.Spec
   ( tests
   ) where

import Test.Tasty
import Test.Tasty.HUnit
         ( Assertion
         , assertBool
         , assertFailure
         , testCase
         )
import qualified Data.Map.Strict as MapS
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.State
         ( execStateT
         , evalStateT
         )

import Lexer.Alex.Functions (runAlex)
import Parser.Functions.Parse.DesignFile (parseDesignFile)
import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores (NetlistStore(..))
import qualified Parser.Netlist.Builtin.Netlist as InitialNetlist
import Manager.NewDesignUnit (create)
import Manager.Types.Error (ConverterError)

-- |All parser tests
tests :: TestTree
tests = testGroup "Parser Tests"
   [ emptyDesignEntities
   ]

emptyDesignEntities :: TestTree
emptyDesignEntities = testGroup "Empty design entities"
   [ emptyEntityDeclarations
   , emptyPackageDeclarations
   , emptyArchitectureBodies
   ]

checkDesignEntity :: (NetlistStore -> a) -> (a -> Bool) -> String -> Assertion
checkDesignEntity extractEntity checkEntity input = do
   result <- runExceptT $ execStateT (runAlex input $ parseDesignFile (create "" "" []) "WORK" []) InitialNetlist.netlist
   case result of
      Right netlist ->
         let entityType = extractEntity netlist
         in assertBool "No entity found" $ checkEntity entityType
      Left err -> assertFailure $ "Failed with error: " ++ show err

emptyEntityDeclarations :: TestTree
emptyEntityDeclarations = testGroup "Empty entities"
   [ testCase "Empty entity with no statement part and final label" $
      checkDesignEntity entities (MapS.member $ NetlistName "WORK" "NAME") "entity name is end name;"
   , testCase "Empty entity with no statement part and no final label" $
      checkDesignEntity entities (MapS.member $ NetlistName "WORK" "NAME") "entity name is end;"
   , testCase "Empty entity with empty statement part and final label" $
      checkDesignEntity entities (MapS.member $ NetlistName "WORK" "NAME") "entity name is begin end name;"
   , testCase "Empty entity with empty statement part and no final label" $
      checkDesignEntity entities (MapS.member $ NetlistName "WORK" "NAME") "entity name is begin end;"
   ]

emptyPackageDeclarations :: TestTree
emptyPackageDeclarations = testGroup "Empty package headers"
   [ testCase "Empty package header with final label" $
      checkDesignEntity packages (MapS.member $ NetlistName "WORK" "NAME") "package name is end name;"
   , testCase "Empty package header with no final label" $
      checkDesignEntity packages (MapS.member $ NetlistName "WORK" "NAME") "package name is end;"
   ]

emptyArchitectureBodies :: TestTree
emptyArchitectureBodies = testGroup "Empty architecture bodies"
   [ testCase "Empty architecture body with final label" $
      checkDesignEntity architectures (MapS.member ("WORK","NAME1","NAME2")) "entity name1 is end; architecture name2 of name1 is begin end name2;"
   , testCase "Empty architecture body with no final label" $
      checkDesignEntity architectures (MapS.member ("WORK","NAME1","NAME2")) "entity name1 is end; architecture name2 of name1 is begin end;"
   ]
