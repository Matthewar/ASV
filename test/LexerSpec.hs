module LexerSpec (tests) where

import Parser.Lexer (Token(..), lexerList)
import Parser.Alex.Types (AlexPosn(..))
import Parser.TokenTypes (ReservedWord(..), OperatorType(..))
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Lexer Tests"
   [singleWords]

singleWords :: TestTree
singleWords = testGroup "Single word tests for lexer"
   [ singleKeywords
   , singleOperators ]
   --, singleIdentifiers
   --, singleLiterals ]

singleKeywords :: TestTree
singleKeywords = testGroup "Single keyword tests for lexer"
   [ singleKeywordsUpper
   , singleKeywordsLower ]

singleKeywordsUpper :: TestTree
singleKeywordsUpper = testGroup "Single upper case keywords"
   [ testCase "\"ABS\" == Keyword Abs" $
         lexerList "ABS" @?= Right [Keyword Abs]
   , testCase "\"ACCESS\" == Keyword Access" $
         lexerList "ACCESS" @?= Right [Keyword Access]
   , testCase "\"AFTER\" == Keyword After" $
         lexerList "AFTER" @?= Right [Keyword After]
   , testCase "\"ALIAS\" == Keyword Alias" $
         lexerList "ALIAS" @?= Right [Keyword Alias]
   , testCase "\"ALL\" == Keyword All" $
         lexerList "ALL" @?= Right [Keyword All]
   , testCase "\"AND\" == Keyword And" $
         lexerList "AND" @?= Right [Keyword And]
   , testCase "\"ARCHITECTURE\" == Keyword Architecture" $
         lexerList "ARCHITECTURE" @?= Right [Keyword Architecture]
   , testCase "\"ARRAY\" == Keyword Array" $
         lexerList "ARRAY" @?= Right [Keyword Array]
   , testCase "\"ASSERT\" == Keyword Assert" $
         lexerList "ASSERT" @?= Right [Keyword Assert]
   , testCase "\"ATTRIBUTE\" == Keyword Attribute" $
         lexerList "ATTRIBUTE" @?= Right [Keyword Attribute]
   , testCase "\"BEGIN\" == Keyword Begin" $
         lexerList "BEGIN" @?= Right [Keyword Begin]
   , testCase "\"BLOCK\" == Keyword Block" $
         lexerList "BLOCK" @?= Right [Keyword Block]
   , testCase "\"BODY\" == Keyword Body" $
         lexerList "BODY" @?= Right [Keyword Body]
   , testCase "\"BUFFER\" == Keyword Buffer" $
         lexerList "BUFFER" @?= Right [Keyword Buffer]
   , testCase "\"BUS\" == Keyword Bus" $
         lexerList "BUS" @?= Right [Keyword Bus]
   , testCase "\"CASE\" == Keyword Case" $
         lexerList "CASE" @?= Right [Keyword Case]
   , testCase "\"COMPONENT\" == Keyword Component" $
         lexerList "COMPONENT" @?= Right [Keyword Component]
   , testCase "\"CONFIGURATION\" == Keyword Configuration" $
         lexerList "CONFIGURATION" @?= Right [Keyword Configuration]
   , testCase "\"CONSTANT\" == Keyword Constant" $
         lexerList "CONSTANT" @?= Right [Keyword Constant]
   , testCase "\"DISCONNECT\" == Keyword Disconnect" $
         lexerList "DISCONNECT" @?= Right [Keyword Disconnect]
   , testCase "\"DOWNTO\" == Keyword Downto" $
         lexerList "DOWNTO" @?= Right [Keyword Downto]
   , testCase "\"ELSE\" == Keyword Else" $
         lexerList "ELSE" @?= Right [Keyword Else]
   , testCase "\"ELSIF\" == Keyword Elsif" $
         lexerList "ELSIF" @?= Right [Keyword Elsif]
   , testCase "\"END\" == Keyword End" $
         lexerList "END" @?= Right [Keyword End]
   , testCase "\"ENTITY\" == Keyword Entity" $
         lexerList "ENTITY" @?= Right [Keyword Entity]
   , testCase "\"EXIT\" == Keyword Exit" $
         lexerList "EXIT" @?= Right [Keyword Exit]
   , testCase "\"FILE\" == Keyword File" $
         lexerList "FILE" @?= Right [Keyword File]
   , testCase "\"FOR\" == Keyword For" $
         lexerList "FOR" @?= Right [Keyword For]
   , testCase "\"FUNCTION\" == Keyword Function" $
         lexerList "FUNCTION" @?= Right [Keyword Function]
   , testCase "\"GENERATE\" == Keyword Generate" $
         lexerList "GENERATE" @?= Right [Keyword Generate]
   , testCase "\"GENERIC\" == Keyword Generic" $
         lexerList "GENERIC" @?= Right [Keyword Generic]
   , testCase "\"GUARDED\" == Keyword Guarded" $
         lexerList "GUARDED" @?= Right [Keyword Guarded]
   , testCase "\"IF\" == Keyword If" $
         lexerList "IF" @?= Right [Keyword If]
   , testCase "\"IN\" == Keyword In" $
         lexerList "IN" @?= Right [Keyword In]
   , testCase "\"INOUT\" == Keyword Inout" $
         lexerList "INOUT" @?= Right [Keyword Inout]
   , testCase "\"IS\" == Keyword Is" $
         lexerList "IS" @?= Right [Keyword Is]
   , testCase "\"LABEL\" == Keyword Label" $
         lexerList "LABEL" @?= Right [Keyword Label]
   , testCase "\"LIBRARY\" == Keyword Library" $
         lexerList "LIBRARY" @?= Right [Keyword Library]
   , testCase "\"LINKAGE\" == Keyword Linkage" $
         lexerList "LINKAGE" @?= Right [Keyword Linkage]
   , testCase "\"LOOP\" == Keyword Loop" $
         lexerList "LOOP" @?= Right [Keyword Loop]
   , testCase "\"MAP\" == Keyword Map" $
         lexerList "MAP" @?= Right [Keyword Map]
   , testCase "\"MOD\" == Keyword Mod" $
         lexerList "MOD" @?= Right [Keyword Mod]
   , testCase "\"NAND\" == Keyword Nand" $
         lexerList "NAND" @?= Right [Keyword Nand]
   , testCase "\"NEW\" == Keyword New" $
         lexerList "NEW" @?= Right [Keyword New]
   , testCase "\"NEXT\" == Keyword Next" $
         lexerList "NEXT" @?= Right [Keyword Next]
   , testCase "\"NOR\" == Keyword Nor" $
         lexerList "NOR" @?= Right [Keyword Nor]
   , testCase "\"NOT\" == Keyword Not" $
         lexerList "NOT" @?= Right [Keyword Not]
   , testCase "\"NULL\" == Keyword Null" $
         lexerList "NULL" @?= Right [Keyword Null]
   , testCase "\"OF\" == Keyword Of" $
         lexerList "OF" @?= Right [Keyword Of]
   , testCase "\"ON\" == Keyword On" $
         lexerList "ON" @?= Right [Keyword On]
   , testCase "\"OPEN\" == Keyword Open" $
         lexerList "OPEN" @?= Right [Keyword Open]
   , testCase "\"OR\" == Keyword Or" $
         lexerList "OR" @?= Right [Keyword Or]
   , testCase "\"OTHERS\" == Keyword Others" $
         lexerList "OTHERS" @?= Right [Keyword Others]
   , testCase "\"OUT\" == Keyword Out" $
         lexerList "OUT" @?= Right [Keyword Out]
   , testCase "\"PACKAGE\" == Keyword Package" $
         lexerList "PACKAGE" @?= Right [Keyword Package]
   , testCase "\"PORT\" == Keyword Port" $
         lexerList "PORT" @?= Right [Keyword Port]
   , testCase "\"PROCEDURE\" == Keyword Procedure" $
         lexerList "PROCEDURE" @?= Right [Keyword Procedure]
   , testCase "\"PROCESS\" == Keyword Process" $
         lexerList "PROCESS" @?= Right [Keyword Process]
   , testCase "\"RANGE\" == Keyword Range" $
         lexerList "RANGE" @?= Right [Keyword Range]
   , testCase "\"RECORD\" == Keyword Record" $
         lexerList "RECORD" @?= Right [Keyword Record]
   , testCase "\"REGISTER\" == Keyword Register" $
         lexerList "REGISTER" @?= Right [Keyword Register]
   , testCase "\"REM\" == Keyword Rem" $
         lexerList "REM" @?= Right [Keyword Rem]
   , testCase "\"REPORT\" == Keyword Report" $
         lexerList "REPORT" @?= Right [Keyword Report]
   , testCase "\"RETURN\" == Keyword Return" $
         lexerList "RETURN" @?= Right [Keyword Return]
   , testCase "\"SELECT\" == Keyword Select" $
         lexerList "SELECT" @?= Right [Keyword Select]
   , testCase "\"SEVERITY\" == Keyword Severity" $
         lexerList "SEVERITY" @?= Right [Keyword Severity]
   , testCase "\"SIGNAL\" == Keyword Signal" $
         lexerList "SIGNAL" @?= Right [Keyword Signal]
   , testCase "\"SUBTYPE\" == Keyword Subtype" $
         lexerList "SUBTYPE" @?= Right [Keyword Subtype]
   , testCase "\"THEN\" == Keyword Then" $
         lexerList "THEN" @?= Right [Keyword Then]
   , testCase "\"TO\" == Keyword To" $
         lexerList "TO" @?= Right [Keyword To]
   , testCase "\"TRANSPORT\" == Keyword Transport" $
         lexerList "TRANSPORT" @?= Right [Keyword Transport]
   , testCase "\"TYPE\" == Keyword Type" $
         lexerList "TYPE" @?= Right [Keyword Type]
   , testCase "\"UNITS\" == Keyword Units" $
         lexerList "UNITS" @?= Right [Keyword Units]
   , testCase "\"UNTIL\" == Keyword Until" $
         lexerList "UNTIL" @?= Right [Keyword Until]
   , testCase "\"USE\" == Keyword Use" $
         lexerList "USE" @?= Right [Keyword Use]
   , testCase "\"VARIABLE\" == Keyword Variable" $
         lexerList "VARIABLE" @?= Right [Keyword Variable]
   , testCase "\"WAIT\" == Keyword Wait" $
         lexerList "WAIT" @?= Right [Keyword Wait]
   , testCase "\"WHEN\" == Keyword When" $
         lexerList "WHEN" @?= Right [Keyword When]
   , testCase "\"WHILE\" == Keyword While" $
         lexerList "WHILE" @?= Right [Keyword While]
   , testCase "\"WITH\" == Keyword With" $
         lexerList "WITH" @?= Right [Keyword With]
   , testCase "\"XOR\" == Keyword Xor" $
         lexerList "XOR" @?= Right [Keyword Xor]
   ]

singleKeywordsLower :: TestTree
singleKeywordsLower = testGroup "Single lower case keywords"
   [ testCase "\"abs\" == Keyword Abs" $
         lexerList "abs" @?= Right [Keyword Abs]
   , testCase "\"access\" == Keyword Access" $
         lexerList "access" @?= Right [Keyword Access]
   , testCase "\"after\" == Keyword After" $
         lexerList "after" @?= Right [Keyword After]
   , testCase "\"alias\" == Keyword Alias" $
         lexerList "alias" @?= Right [Keyword Alias]
   , testCase "\"all\" == Keyword All" $
         lexerList "all" @?= Right [Keyword All]
   , testCase "\"and\" == Keyword And" $
         lexerList "and" @?= Right [Keyword And]
   , testCase "\"architecture\" == Keyword Architecture" $
         lexerList "architecture" @?= Right [Keyword Architecture]
   , testCase "\"array\" == Keyword Array" $
         lexerList "array" @?= Right [Keyword Array]
   , testCase "\"assert\" == Keyword Assert" $
         lexerList "assert" @?= Right [Keyword Assert]
   , testCase "\"attribute\" == Keyword Attribute" $
         lexerList "attribute" @?= Right [Keyword Attribute]
   , testCase "\"begin\" == Keyword Begin" $
         lexerList "begin" @?= Right [Keyword Begin]
   , testCase "\"block\" == Keyword Block" $
         lexerList "block" @?= Right [Keyword Block]
   , testCase "\"body\" == Keyword Body" $
         lexerList "body" @?= Right [Keyword Body]
   , testCase "\"buffer\" == Keyword Buffer" $
         lexerList "buffer" @?= Right [Keyword Buffer]
   , testCase "\"bus\" == Keyword Bus" $
         lexerList "bus" @?= Right [Keyword Bus]
   , testCase "\"case\" == Keyword Case" $
         lexerList "case" @?= Right [Keyword Case]
   , testCase "\"component\" == Keyword Component" $
         lexerList "component" @?= Right [Keyword Component]
   , testCase "\"configuration\" == Keyword Configuration" $
         lexerList "configuration" @?= Right [Keyword Configuration]
   , testCase "\"constant\" == Keyword Constant" $
         lexerList "constant" @?= Right [Keyword Constant]
   , testCase "\"disconnect\" == Keyword Disconnect" $
         lexerList "disconnect" @?= Right [Keyword Disconnect]
   , testCase "\"downto\" == Keyword Downto" $
         lexerList "downto" @?= Right [Keyword Downto]
   , testCase "\"else\" == Keyword Else" $
         lexerList "else" @?= Right [Keyword Else]
   , testCase "\"elsif\" == Keyword Elsif" $
         lexerList "elsif" @?= Right [Keyword Elsif]
   , testCase "\"end\" == Keyword End" $
         lexerList "end" @?= Right [Keyword End]
   , testCase "\"entity\" == Keyword Entity" $
         lexerList "entity" @?= Right [Keyword Entity]
   , testCase "\"exit\" == Keyword Exit" $
         lexerList "exit" @?= Right [Keyword Exit]
   , testCase "\"file\" == Keyword File" $
         lexerList "file" @?= Right [Keyword File]
   , testCase "\"for\" == Keyword For" $
         lexerList "for" @?= Right [Keyword For]
   , testCase "\"function\" == Keyword Function" $
         lexerList "function" @?= Right [Keyword Function]
   , testCase "\"generate\" == Keyword Generate" $
         lexerList "generate" @?= Right [Keyword Generate]
   , testCase "\"generic\" == Keyword Generic" $
         lexerList "generic" @?= Right [Keyword Generic]
   , testCase "\"guarded\" == Keyword Guarded" $
         lexerList "guarded" @?= Right [Keyword Guarded]
   , testCase "\"if\" == Keyword If" $
         lexerList "if" @?= Right [Keyword If]
   , testCase "\"in\" == Keyword In" $
         lexerList "in" @?= Right [Keyword In]
   , testCase "\"inout\" == Keyword Inout" $
         lexerList "inout" @?= Right [Keyword Inout]
   , testCase "\"is\" == Keyword Is" $
         lexerList "is" @?= Right [Keyword Is]
   , testCase "\"label\" == Keyword Label" $
         lexerList "label" @?= Right [Keyword Label]
   , testCase "\"library\" == Keyword Library" $
         lexerList "library" @?= Right [Keyword Library]
   , testCase "\"linkage\" == Keyword Linkage" $
         lexerList "linkage" @?= Right [Keyword Linkage]
   , testCase "\"loop\" == Keyword Loop" $
         lexerList "loop" @?= Right [Keyword Loop]
   , testCase "\"map\" == Keyword Map" $
         lexerList "map" @?= Right [Keyword Map]
   , testCase "\"mod\" == Keyword Mod" $
         lexerList "mod" @?= Right [Keyword Mod]
   , testCase "\"nand\" == Keyword Nand" $
         lexerList "nand" @?= Right [Keyword Nand]
   , testCase "\"new\" == Keyword New" $
         lexerList "new" @?= Right [Keyword New]
   , testCase "\"next\" == Keyword Next" $
         lexerList "next" @?= Right [Keyword Next]
   , testCase "\"nor\" == Keyword Nor" $
         lexerList "nor" @?= Right [Keyword Nor]
   , testCase "\"not\" == Keyword Not" $
         lexerList "not" @?= Right [Keyword Not]
   , testCase "\"null\" == Keyword Null" $
         lexerList "null" @?= Right [Keyword Null]
   , testCase "\"of\" == Keyword Of" $
         lexerList "of" @?= Right [Keyword Of]
   , testCase "\"on\" == Keyword On" $
         lexerList "on" @?= Right [Keyword On]
   , testCase "\"open\" == Keyword Open" $
         lexerList "open" @?= Right [Keyword Open]
   , testCase "\"or\" == Keyword Or" $
         lexerList "or" @?= Right [Keyword Or]
   , testCase "\"others\" == Keyword Others" $
         lexerList "others" @?= Right [Keyword Others]
   , testCase "\"out\" == Keyword Out" $
         lexerList "out" @?= Right [Keyword Out]
   , testCase "\"package\" == Keyword Package" $
         lexerList "package" @?= Right [Keyword Package]
   , testCase "\"port\" == Keyword Port" $
         lexerList "port" @?= Right [Keyword Port]
   , testCase "\"procedure\" == Keyword Procedure" $
         lexerList "procedure" @?= Right [Keyword Procedure]
   , testCase "\"process\" == Keyword Process" $
         lexerList "process" @?= Right [Keyword Process]
   , testCase "\"range\" == Keyword Range" $
         lexerList "range" @?= Right [Keyword Range]
   , testCase "\"record\" == Keyword Record" $
         lexerList "record" @?= Right [Keyword Record]
   , testCase "\"register\" == Keyword Register" $
         lexerList "register" @?= Right [Keyword Register]
   , testCase "\"rem\" == Keyword Rem" $
         lexerList "rem" @?= Right [Keyword Rem]
   , testCase "\"report\" == Keyword Report" $
         lexerList "report" @?= Right [Keyword Report]
   , testCase "\"return\" == Keyword Return" $
         lexerList "return" @?= Right [Keyword Return]
   , testCase "\"select\" == Keyword Select" $
         lexerList "select" @?= Right [Keyword Select]
   , testCase "\"severity\" == Keyword Severity" $
         lexerList "severity" @?= Right [Keyword Severity]
   , testCase "\"signal\" == Keyword Signal" $
         lexerList "signal" @?= Right [Keyword Signal]
   , testCase "\"subtype\" == Keyword Subtype" $
         lexerList "subtype" @?= Right [Keyword Subtype]
   , testCase "\"then\" == Keyword Then" $
         lexerList "then" @?= Right [Keyword Then]
   , testCase "\"to\" == Keyword To" $
         lexerList "to" @?= Right [Keyword To]
   , testCase "\"transport\" == Keyword Transport" $
         lexerList "transport" @?= Right [Keyword Transport]
   , testCase "\"type\" == Keyword Type" $
         lexerList "type" @?= Right [Keyword Type]
   , testCase "\"units\" == Keyword Units" $
         lexerList "units" @?= Right [Keyword Units]
   , testCase "\"until\" == Keyword Until" $
         lexerList "until" @?= Right [Keyword Until]
   , testCase "\"use\" == Keyword Use" $
         lexerList "use" @?= Right [Keyword Use]
   , testCase "\"variable\" == Keyword Variable" $
         lexerList "variable" @?= Right [Keyword Variable]
   , testCase "\"wait\" == Keyword Wait" $
         lexerList "wait" @?= Right [Keyword Wait]
   , testCase "\"when\" == Keyword When" $
         lexerList "when" @?= Right [Keyword When]
   , testCase "\"while\" == Keyword While" $
         lexerList "while" @?= Right [Keyword While]
   , testCase "\"with\" == Keyword With" $
         lexerList "with" @?= Right [Keyword With]
   , testCase "\"xor\" == Keyword Xor" $
         lexerList "xor" @?= Right [Keyword Xor]
   ]

singleOperators :: TestTree
singleOperators = testGroup "Single operators"
   [ testCase "\"=>\" == Arrow" $
         lexerList "=>" @?= Right [Operator Arrow]
   , testCase "\"**\" == DoubleStar" $
         lexerList "**" @?= Right [Operator DoubleStar]
   , testCase "\":=\" == VarAssign" $
         lexerList ":=" @?= Right [Operator VarAssign]
   , testCase "\"/=\" == Inequality" $
         lexerList "/=" @?= Right [Operator Inequality]
   , testCase "\">=\" == GreaterThanOrEqual" $
         lexerList ">=" @?= Right [Operator GreaterThanOrEqual]
   , testCase "\"<=\" == SignAssign" $
         lexerList "<=" @?= Right [Operator SignAssign]
   , testCase "\"<>\" == Box" $
         lexerList "<>" @?= Right [Operator Box]
   , testCase "\"&\" == Ampersand" $
         lexerList "&" @?= Right [Operator Ampersand]
   , testCase "\"'\" == Apostrophe" $
         lexerList "'" @?= Right [Operator Apostrophe]
   , testCase "\"(\" == LeftParen" $
         lexerList "(" @?= Right [Operator LeftParen]
   , testCase "\")\" == RightParen" $
         lexerList ")" @?= Right [Operator RightParen]
   , testCase "\"*\" == Star" $
         lexerList "*" @?= Right [Operator Star]
   , testCase "\"+\" == Plus" $
         lexerList "+" @?= Right [Operator Plus]
   , testCase "\",\" == Comma" $
         lexerList "," @?= Right [Operator Comma]
   , testCase "\"-\" == Hyphen" $
         lexerList "-" @?= Right [Operator Hyphen]
   , testCase "\".\" == Period" $
         lexerList "." @?= Right [Operator Period]
   , testCase "\"/\" == Slash" $
         lexerList "/" @?= Right [Operator Slash]
   , testCase "\":\" == Colon" $
         lexerList ":" @?= Right [Operator Colon]
   , testCase "\";\" == Semicolon" $
         lexerList ";" @?= Right [Operator Semicolon]
   , testCase "\"<\" == LessThan" $
         lexerList "<" @?= Right [Operator LessThan]
   , testCase "\"=\" == Equal" $
         lexerList "=" @?= Right [Operator Equal]
   , testCase "\">\" == GreaterThan" $
         lexerList ">" @?= Right [Operator GreaterThan]
   , testCase "\"|\" == Bar" $
         lexerList "|" @?= Right [Operator Bar]
   ]
--singleIdentifiers :: TestTree
--singleLiterals :: TestTree
