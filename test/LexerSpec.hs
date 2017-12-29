module LexerSpec (tests) where

import Parser.Lexer (Token(..), lexerList)
import Parser.Alex.Types (AlexPosn(..))
import Parser.TokenTypes (ReservedWord(..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Lexer Tests"
   [singleWords]

singleWords :: TestTree
singleWords = testGroup "Single word tests for lexer"
   [ singleKeywords ]
   --, singleOperators
   --, singleIdentifiers
   --, singleLiterals ]

singleKeywords :: TestTree
singleKeywords = testGroup "Single keyword tests for lexer"
   [ singleKeywordsUpper ]
   --, singleKeywordsLower ]

singleKeywordsUpper :: TestTree
singleKeywordsUpper = testGroup "Single upper case keywords"
   [ testCase "Single upper case keyword Abs" $
         lexerList "ABS" @?= Right [Keyword Abs]
   , testCase "Single upper case keyword Access" $
         lexerList "ACCESS" @?= Right [Keyword Access]
   , testCase "Single upper case keyword After" $
         lexerList "AFTER" @?= Right [Keyword After]
   , testCase "Single upper case keyword Alias" $
         lexerList "ALIAS" @?= Right [Keyword Alias]
   , testCase "Single upper case keyword All" $
         lexerList "ALL" @?= Right [Keyword All]
   , testCase "Single upper case keyword And" $
         lexerList "AND" @?= Right [Keyword And]
   , testCase "Single upper case keyword Architecture" $
         lexerList "ARCHITECTURE" @?= Right [Keyword Architecture]
   , testCase "Single upper case keyword Array" $
         lexerList "ARRAY" @?= Right [Keyword Array]
   , testCase "Single upper case keyword Assert" $
         lexerList "ASSERT" @?= Right [Keyword Assert]
   , testCase "Single upper case keyword Attribute" $
         lexerList "ATTRIBUTE" @?= Right [Keyword Attribute]
   , testCase "Single upper case keyword Begin" $
         lexerList "BEGIN" @?= Right [Keyword Begin]
   , testCase "Single upper case keyword Block" $
         lexerList "BLOCK" @?= Right [Keyword Block]
   , testCase "Single upper case keyword Body" $
         lexerList "BODY" @?= Right [Keyword Body]
   , testCase "Single upper case keyword Buffer" $
         lexerList "BUFFER" @?= Right [Keyword Buffer]
   , testCase "Single upper case keyword Bus" $
         lexerList "BUS" @?= Right [Keyword Bus]
   , testCase "Single upper case keyword Case" $
         lexerList "CASE" @?= Right [Keyword Case]
   , testCase "Single upper case keyword Component" $
         lexerList "COMPONENT" @?= Right [Keyword Component]
   , testCase "Single upper case keyword Configuration" $
         lexerList "CONFIGURATION" @?= Right [Keyword Configuration]
   , testCase "Single upper case keyword Constant" $
         lexerList "CONSTANT" @?= Right [Keyword Constant]
   , testCase "Single upper case keyword Disconnect" $
         lexerList "DISCONNECT" @?= Right [Keyword Disconnect]
   , testCase "Single upper case keyword Downto" $
         lexerList "DOWNTO" @?= Right [Keyword Downto]
   , testCase "Single upper case keyword Else" $
         lexerList "ELSE" @?= Right [Keyword Else]
   , testCase "Single upper case keyword Elsif" $
         lexerList "ELSIF" @?= Right [Keyword Elsif]
   , testCase "Single upper case keyword End" $
         lexerList "END" @?= Right [Keyword End]
   , testCase "Single upper case keyword Entity" $
         lexerList "ENTITY" @?= Right [Keyword Entity]
   , testCase "Single upper case keyword Exit" $
         lexerList "EXIT" @?= Right [Keyword Exit]
   , testCase "Single upper case keyword File" $
         lexerList "FILE" @?= Right [Keyword File]
   , testCase "Single upper case keyword For" $
         lexerList "FOR" @?= Right [Keyword For]
   , testCase "Single upper case keyword Function" $
         lexerList "FUNCTION" @?= Right [Keyword Function]
   , testCase "Single upper case keyword Generate" $
         lexerList "GENERATE" @?= Right [Keyword Generate]
   , testCase "Single upper case keyword Generic" $
         lexerList "GENERIC" @?= Right [Keyword Generic]
   , testCase "Single upper case keyword Guarded" $
         lexerList "GUARDED" @?= Right [Keyword Guarded]
   , testCase "Single upper case keyword If" $
         lexerList "IF" @?= Right [Keyword If]
   , testCase "Single upper case keyword In" $
         lexerList "IN" @?= Right [Keyword In]
   , testCase "Single upper case keyword Inout" $
         lexerList "INOUT" @?= Right [Keyword Inout]
   , testCase "Single upper case keyword Is" $
         lexerList "IS" @?= Right [Keyword Is]
   , testCase "Single upper case keyword Label" $
         lexerList "LABEL" @?= Right [Keyword Label]
   , testCase "Single upper case keyword Library" $
         lexerList "LIBRARY" @?= Right [Keyword Library]
   , testCase "Single upper case keyword Linkage" $
         lexerList "LINKAGE" @?= Right [Keyword Linkage]
   , testCase "Single upper case keyword Loop" $
         lexerList "LOOP" @?= Right [Keyword Loop]
   , testCase "Single upper case keyword Map" $
         lexerList "MAP" @?= Right [Keyword Map]
   , testCase "Single upper case keyword Mod" $
         lexerList "MOD" @?= Right [Keyword Mod]
   , testCase "Single upper case keyword Nand" $
         lexerList "NAND" @?= Right [Keyword Nand]
   , testCase "Single upper case keyword New" $
         lexerList "NEW" @?= Right [Keyword New]
   , testCase "Single upper case keyword Next" $
         lexerList "NEXT" @?= Right [Keyword Next]
   , testCase "Single upper case keyword Nor" $
         lexerList "NOR" @?= Right [Keyword Nor]
   , testCase "Single upper case keyword Not" $
         lexerList "NOT" @?= Right [Keyword Not]
   , testCase "Single upper case keyword Null" $
         lexerList "NULL" @?= Right [Keyword Null]
   , testCase "Single upper case keyword Of" $
         lexerList "OF" @?= Right [Keyword Of]
   , testCase "Single upper case keyword On" $
         lexerList "ON" @?= Right [Keyword On]
   , testCase "Single upper case keyword Open" $
         lexerList "OPEN" @?= Right [Keyword Open]
   , testCase "Single upper case keyword Or" $
         lexerList "OR" @?= Right [Keyword Or]
   , testCase "Single upper case keyword Others" $
         lexerList "OTHERS" @?= Right [Keyword Others]
   , testCase "Single upper case keyword Out" $
         lexerList "OUT" @?= Right [Keyword Out]
   , testCase "Single upper case keyword Package" $
         lexerList "PACKAGE" @?= Right [Keyword Package]
   , testCase "Single upper case keyword Port" $
         lexerList "PORT" @?= Right [Keyword Port]
   , testCase "Single upper case keyword Procedure" $
         lexerList "PROCEDURE" @?= Right [Keyword Procedure]
   , testCase "Single upper case keyword Process" $
         lexerList "PROCESS" @?= Right [Keyword Process]
   , testCase "Single upper case keyword Range" $
         lexerList "RANGE" @?= Right [Keyword Range]
   , testCase "Single upper case keyword Record" $
         lexerList "RECORD" @?= Right [Keyword Record]
   , testCase "Single upper case keyword Register" $
         lexerList "REGISTER" @?= Right [Keyword Register]
   , testCase "Single upper case keyword Rem" $
         lexerList "REM" @?= Right [Keyword Rem]
   , testCase "Single upper case keyword Report" $
         lexerList "REPORT" @?= Right [Keyword Report]
   , testCase "Single upper case keyword Return" $
         lexerList "RETURN" @?= Right [Keyword Return]
   , testCase "Single upper case keyword Select" $
         lexerList "SELECT" @?= Right [Keyword Select]
   , testCase "Single upper case keyword Severity" $
         lexerList "SEVERITY" @?= Right [Keyword Severity]
   , testCase "Single upper case keyword Signal" $
         lexerList "SIGNAL" @?= Right [Keyword Signal]
   , testCase "Single upper case keyword Subtype" $
         lexerList "SUBTYPE" @?= Right [Keyword Subtype]
   , testCase "Single upper case keyword Then" $
         lexerList "THEN" @?= Right [Keyword Then]
   , testCase "Single upper case keyword To" $
         lexerList "TO" @?= Right [Keyword To]
   , testCase "Single upper case keyword Transport" $
         lexerList "TRANSPORT" @?= Right [Keyword Transport]
   , testCase "Single upper case keyword Type" $
         lexerList "TYPE" @?= Right [Keyword Type]
   , testCase "Single upper case keyword Units" $
         lexerList "UNITS" @?= Right [Keyword Units]
   , testCase "Single upper case keyword Until" $
         lexerList "UNTIL" @?= Right [Keyword Until]
   , testCase "Single upper case keyword Use" $
         lexerList "USE" @?= Right [Keyword Use]
   , testCase "Single upper case keyword Variable" $
         lexerList "VARIABLE" @?= Right [Keyword Variable]
   , testCase "Single upper case keyword Wait" $
         lexerList "WAIT" @?= Right [Keyword Wait]
   , testCase "Single upper case keyword When" $
         lexerList "WHEN" @?= Right [Keyword When]
   , testCase "Single upper case keyword While" $
         lexerList "WHILE" @?= Right [Keyword While]
   , testCase "Single upper case keyword With" $
         lexerList "WITH" @?= Right [Keyword With]
   , testCase "Single upper case keyword Xor" $
         lexerList "XOR" @?= Right [Keyword Xor]
   ]
--singleOperators :: TestTree
--singleIdentifiers :: TestTree
--singleLiterals :: TestTree
