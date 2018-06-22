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
import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Enumerate(..)
         , IntegerRange(..)
         , FloatRange(..)
         , RangeDirection(..)
         , Subtype(..)
         )
import Parser.Netlist.Types.Stores
         ( NetlistStore(..)
         , Entity(..)
         )
import qualified Parser.Netlist.Builtin.Netlist as InitialNetlist
import Manager.NewDesignUnit (create)
import Manager.Types.Error (ConverterError)

-- |All parser tests
tests :: TestTree
tests = testGroup "Parser Tests"
   [ emptyDesignEntities
   , typeDeclarations
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

typeDeclarations :: TestTree
typeDeclarations = testGroup "Type declarations"
   [ scalarTypes
   --, compositeTypes
   --, accessTypes
   --, fileTypes
   ]

scalarTypes :: TestTree
scalarTypes = testGroup "Scalar type declarations"
   [ enumTypes
   , integerTypes
   , floatingTypes
   , physicalTypes
   ]

checkEnumerateType :: String -> [Enumerate] -> String -> Assertion
checkEnumerateType subtypeName expectedEnums input = do
   result <- runExceptT $ execStateT (runAlex input $ parseDesignFile (create "" "" []) "WORK" []) InitialNetlist.netlist
   case result of
      Right netlist ->
         let entity = (entities netlist) MapS.! (NetlistName "WORK" "NAME1")
             subtype = entitySubtypes entity
         in case MapS.lookup subtypeName subtype of
               Nothing -> assertFailure $ "No enumeration type found"
               Just (EnumerationSubtype _ _ enums _) -> assertBool "Incorrect enums found" $ enums == expectedEnums
               _ -> assertFailure $ "Incorrect subtype"
      Left err -> assertFailure $ "Failed with error: " ++ show err

enumTypes :: TestTree
enumTypes = testGroup "Enumeration type declarations"
   [ testCase "Identifier enumeration literals" $
      checkEnumerateType
         "MULTI_LEVEL_LOGIC"
         [ Enum_Identifier "LOW"
         , Enum_Identifier "HIGH"
         , Enum_Identifier "RISING"
         , Enum_Identifier "FALLING"
         , Enum_Identifier "AMBIGUOUS"
         ]
         "entity name1 is type MULTI_LEVEL_LOGIC is (LOW,HIGH,RISING,FALLING,AMBIGUOUS) ; end;"
   , testCase "Character enumeration literals" $
      checkEnumerateType
         "SWITCH_LEVEL"
         [ Enum_Char '0'
         , Enum_Char '1'
         , Enum_Char 'X'
         ]
         "entity name1 is type SWITCH_LEVEL is ('0','1','X') ; end;"
   , testCase "Identifer and character enumeration literals" $
      checkEnumerateType
         "SOME_TYPE"
         [ Enum_Char '0'
         , Enum_Char '1'
         , Enum_Identifier "TESTELEMENT"
         ]
         "entity name1 is type some_type is ('0','1',testelement) ; end;"
   ]

checkIntegerType :: String -> IntegerRange -> String -> Assertion
checkIntegerType subtypeName expectedRange input = do
   result <- runExceptT $ execStateT (runAlex input $ parseDesignFile (create "" "" []) "WORK" []) InitialNetlist.netlist
   case result of
      Right netlist ->
         let entity = (entities netlist) MapS.! (NetlistName "WORK" "NAME1")
             subtype = entitySubtypes entity
         in case MapS.lookup subtypeName subtype of
               Nothing -> assertFailure $ "No integer type found"
               Just (IntegerSubtype _ _ range) -> assertBool "Incorrect integer range found" $ range == expectedRange
               _ -> assertFailure $ "Incorrect subtype"
      Left err -> assertFailure $ "Failed with error: " ++ show err

integerTypes :: TestTree
integerTypes = testGroup "Integer type declarations"
   [ testCase "1076-1987 Example 1" $
      checkIntegerType
         "TWOS_COMPLEMENT_INTEGER"
         (IntegerRange (-32768) 32767 To)
         "entity name1 is type TWOS_COMPLEMENT_INTEGER is range -32768 to 32767; end;"
   , testCase "1076-1987 Example 2" $
      checkIntegerType
         "BYTE_LENGTH_INTEGER"
         (IntegerRange 0 255 To)
         "entity name1 is type BYTE_LENGTH_INTEGER is range 0 to 255; end;"
   , testCase "1076-1987 Example 3" $
      checkIntegerType
         "WORD_INDEX"
         (IntegerRange 31 0 Downto)
      "entity name1 is type WORD_INDEX is range 31 downto 0; end;"
   , testCase "Integer type with abs" $
      checkIntegerType
         "ABSTYPE"
         (IntegerRange 0 31 To)
      "entity name1 is type abstype is range 0 to abs ( -31 ) ; end;"
   ]

checkFloatingType :: String -> FloatRange -> String -> Assertion
checkFloatingType subtypeName expectedRange input = do
   result <- runExceptT $ execStateT (runAlex input $ parseDesignFile (create "" "" []) "WORK" []) InitialNetlist.netlist
   case result of
      Right netlist ->
         let entity = (entities netlist) MapS.! (NetlistName "WORK" "NAME1")
             subtype = entitySubtypes entity
         in case MapS.lookup subtypeName subtype of
               Nothing -> assertFailure $ "No floating type found"
               Just (FloatingSubtype _ _ range) -> assertBool "Incorrect floating range found" $ range == expectedRange
               _ -> assertFailure $ "Incorrect subtype"
      Left err -> assertFailure $ "Failed with error: " ++ show err

floatingTypes :: TestTree
floatingTypes = testGroup "Floating type declarations"
   [ testCase "Test 1" $
      checkFloatingType
         "TWOS_COMPLEMENT_REAL"
         (FloatRange (-32768.0) 32767.0 To)
         "entity name1 is type TWOS_COMPLEMENT_real is range -32768.0 to 32767.0; end;"
   , testCase "Test 2" $
      checkFloatingType
         "TEST2"
         (FloatRange 0.3 255.7 To)
         "entity name1 is type test2 is range 0.3 to 255.7; end;"
   , testCase "Test 3" $
      checkFloatingType
         "TEST3"
         (FloatRange 31.9 0.1 Downto)
      "entity name1 is type test3 is range 31.9 downto 0.1; end;"
   , testCase "Floating type with abs" $
      checkFloatingType
         "ABSTYPE"
         (FloatRange 0.0 31.0 To)
      "entity name1 is type abstype is range 0.0 to abs ( -31.0 ) ; end;"
   ]

physicalTypes :: TestTree
physicalTypes = testGroup "Physical type declarations"
   [ testCase "1076-1987 Example 2" $ do
      let input =
            "entity name1 is\n\
            \type DISTANCE is range 0 to 1E16\n\
            \   units\n\
            \      A;\n\
            \      nm = 10 A;\n\
            \      um = 1000 nm;\n\
            \      mm = 1000 um;\n\
            \      cm = 10 mm;\n\
            \      m = 1000 mm;\n\
            \      km = 1000 m;\n\
            \      mil = 254000 A;\n\
            \      inch = 1000 mil;\n\
            \      ft = 12 inch;\n\
            \      yd = 3 ft;\n\
            \      fm = 6 ft;\n\
            \      mi = 5280 ft;\n\
            \      lg = 3 mi;\n\
            \   end units;\n\
            \end;"
          expectedUnits = MapS.fromList
            [ ("NM",10)
            , ("UM",10000)
            , ("MM",10000000)
            , ("CM",100000000)
            , ("M",10000000000)
            , ("KM",10000000000000)
            , ("MIL",254000)
            , ("INCH",254000000)
            , ("FT",3048000000)
            , ("YD",9144000000)
            , ("FM",18288000000)
            , ("MI",16093440000000)
            , ("LG",48280320000000)
            ]
      result <- runExceptT $ execStateT (runAlex input $ parseDesignFile (create "" "" []) "WORK" []) InitialNetlist.netlist
      case result of
         Right netlist ->
            let entity = (entities netlist) MapS.! (NetlistName "WORK" "NAME1")
                subtype = entitySubtypes entity
            in case MapS.lookup "DISTANCE" subtype of
                  Nothing -> assertFailure $ "No physical type found"
                  Just (PhysicalSubtype _ _ baseUnit _ _) | baseUnit /= "A" -> assertFailure "Incorrect base unit"
                  Just (PhysicalSubtype _ _ _ secondUnits _) | secondUnits /= expectedUnits -> assertFailure "Incorrect secondary units"
                  Just (PhysicalSubtype _ _ _ _ range) -> assertBool "Non-matching range" $ range == (IntegerRange 0 (10^16) To)
                  _ -> assertFailure $ "Incorrect subtype"
         Left err -> assertFailure $ "Failed with error: " ++ show err
   ]

--subtypeDeclarations :: TestTree
--subtypeDeclarations = testGroup "Subtype declarations"
--   [ 
