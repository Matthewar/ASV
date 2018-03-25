module LexerSpec (tests) where

import Parser.Alex.BaseTypes (AlexPosn(..))
import Parser.Lexer (lexerList)
import Parser.ErrorTypes
import Parser.TokenTypes
import Parser.PositionWrapper

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Control.Monad
import Data.Function ((&))
import qualified Data.Map.Strict as MapS
import Data.List.Split (splitOneOf,splitOn)
import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.Char (toUpper,digitToInt)
import Data.List (findIndex)
import Numeric (readInt)

-- |All lexer tests
tests :: TestTree
tests = testGroup "Lexer Tests"
   [singleWords]

-- |Single word lexer tests
-- Consist of a string that will lex to a single token
singleWords :: TestTree
singleWords = testGroup "Single word tests for lexer"
   [ singleKeywords
   , singleOperators
   , singleLiterals
   , singleIdentifiers ]

-- |Single keyword lexer tests
-- A single string that lexes to a keyword token
singleKeywords :: TestTree
singleKeywords = testGroup "Single keyword tests for lexer"
   [ singleKeywordsUpper
   , singleKeywordsLower ]

-- |Single upper case keyword lexer unit tests
-- Unit tests with single upper case strings that lex to keyword tokens
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

-- |Single lower case keyword lexer unit tests
-- Unit tests with single lower case strings that lex to keyword tokens
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

-- |Single operator lexer unit tests
-- Unit tests with single strings that lex to operator tokens
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

-- |Single literal lexer tests
-- A single string that lexes to a literal token
singleLiterals :: TestTree
singleLiterals = testGroup "Single literals"
   [ singleBasedLiterals
   , singleDecimalLiterals
   , singleBitStrLiterals
   , singleStrLiterals
   , singleCharLiterals
   ]

-- |Single based literal lexer tests
-- A single string that lexes to a literal token containing a based literal
-- Has tests for both container types
singleBasedLiterals :: TestTree
singleBasedLiterals = testGroup "Single based values"
   [ QC.testProperty "with # containers" $ singleBasedLiterals_cont '#'
   , QC.testProperty "with : containers" $ singleBasedLiterals_cont ':'
   ]

-- |Generator for a single based literal with specified containers
singleBasedLiterals_cont :: Char -> QC.Property
singleBasedLiterals_cont container =
   QC.forAll (generateBasedStr container) $ \basedStr ->
      let lexRun = lexerList basedStr
          filteredBasedStr = filter (\char -> char /= '_') basedStr
          (baseChars,valueStr,exponentStr) = case splitOn [container] filteredBasedStr of
            (base:val:('E':exp):[]) -> (base,val,exp)
            (base:val:('e':exp):[]) -> (base,val,exp)
            (base:val:"":[]) -> (base,val,"0")
          baseVal = read baseChars
          shiftedValue = case splitOn "." valueStr of
                           (units:decimals:[]) -> units ++ decimals
                           (units:[]) -> units
          expVal = let exponentAdjust = case findIndex (\char -> char == '.') valueStr of
                                             Just index -> (length valueStr) - (index+1)
                                             Nothing -> 0
                       expStrNoPlus = case exponentStr of
                                       ('+':val) -> val
                                       val -> val
                   in (read expStrNoPlus) - exponentAdjust
          convertedValue = (fromIntegral $ fromBase (toInteger $ floor baseVal) shiftedValue) * (baseVal ^^ expVal)
          expectedOutput =
            if isInfinite convertedValue then
               let errorType =
                     if elem '.' filteredBasedStr then
                        LexErr_UniversalReal_OutOfBounds
                     else
                        LexErr_UniversalInt_OutOfBounds
               in Left $ PosnWrapper { getPos = AlexPn 0 1 0, unPos = errorType basedStr }
            else
               Right $
                  [ Literal $ convertedValue
                  & if floor convertedValue == ceiling convertedValue then Univ_Int . floor else Univ_Real
                  ]
      in lexRun == expectedOutput
   where fromBase :: Integer -> String -> Integer
         fromBase base = fst . head . readInt base ((<base).digitToInteger) digitToInt
         digitToInteger :: Char -> Integer
         digitToInteger chr = let charMap =
                                    [ ('0',0)
                                    , ('1',1)
                                    , ('2',2)
                                    , ('3',3)
                                    , ('4',4)
                                    , ('5',5)
                                    , ('6',6)
                                    , ('7',7)
                                    , ('8',8)
                                    , ('9',9)
                                    , ('A',10)
                                    , ('B',11)
                                    , ('C',12)
                                    , ('D',13)
                                    , ('E',14)
                                    , ('F',15)
                                    ]
                                    & MapS.fromList
                              in charMap MapS.! (toUpper chr)

-- |Single decimal literal lexer tests
-- A single string that lexes to a literal token containing a decimal literal
-- Has tests for univeral real ('Univ_Real') and universal integer ('Univ_Int') types.
-- Different tests for literals with and without exponents, and explicitly test zero values.
singleDecimalLiterals :: TestTree
singleDecimalLiterals = testGroup "Single decimal values"
   [ singleDecLit_int
   , singleDecLit_real
   , singleDecLit_int_exp
   , singleDecLit_real_exp
   , singleDecLit_zeroes
   ]

-- |Decimal literal token test type
-- Tests the universal integer ('Univ_Int') type without an exponent
singleDecLit_int :: TestTree
singleDecLit_int = QC.testProperty "Integer value without exponent" $
   let expression :: Int -> Bool
       expression value =
         let lexRun = lexerList $ show value
             expectedOutput = Right [Literal $ Univ_Int $ fromIntegral value]
         in lexRun == expectedOutput
   in expression . abs

-- |Decimal literal token test type
-- Tests the universal real ('Univ_Real') type without an exponent
singleDecLit_real :: TestTree
singleDecLit_real = QC.testProperty "Real value without exponent" $
   let expression :: Double -> Bool
       expression value =
         let lexRun = lexerList $ show value
             expectedOutput = Right [Literal $ Univ_Real value]
         in lexRun == expectedOutput
   in expression . abs

-- |Decimal literal token test type
-- Tests the universal integer ('Univ_Int') type with an exponent
singleDecLit_int_exp :: TestTree
singleDecLit_int_exp = QC.testProperty "Integer value with exponent" $
   QC.forAll genVal $ \value ->
      let [base,exp] =
            filter (\char -> not $ elem char "+_") value
            & splitOneOf "Ee"
            & fmap read
          doubleValue :: Double -- ?? Probably needs changing to deal with numbers too large for double, potentially use unbounded Integer instead
          doubleValue = base * 10 ** exp
          expectedValue =
            if isInfinite doubleValue then
                  Left $ PosnWrapper { getPos = AlexPn 0 1 0, unPos = LexErr_UniversalInt_OutOfBounds value }
            else Right [Literal $ Univ_Int $ floor doubleValue]
          lexRun = lexerList value
      in lexRun == expectedValue
   where genVal = do
            intStr <- genInteger 1 10
            expStr <- genExponent
            return $ intStr ++ expStr

-- |Generate a VHDL specific integer
-- > integer ::= digit { [ underline ] digit }
-- Implemented as:
-- > 'genInteger' ::= digit { 'genUnderscoreDigit' }
genInteger fromLength toLength = do
   firstInt <- QC.elements ['0'..'9']
   lengthStr <- QC.elements [fromLength..toLength]
   otherInts <- replicateM lengthStr genUnderscoreDigit
   return $ [firstInt] ++ (concat otherInts)

-- |Generate latter part of a VHDL specific integer
-- > integer ::= digit { [ underline ] digit }
-- This function generates the latter part IE
-- > 'genUnderscoreDigit' ::= [ underline ] digit
genUnderscoreDigit = do
   optionalUnderscore <- QC.elements [True,False]
   otherDigit <- QC.elements ['0'..'9']
   return $
      if optionalUnderscore then ['_',otherDigit]
      else [otherDigit]

-- |Generate a VHDL specific exponent
-- > exponent ::= E [ + ] integer  | E - integer
-- Note: case insensitive \\'E\\'
-- Implemented as:
-- @
--    'genExponent' ::= exponent_marker [ exponent_sign ] 'genInteger'
--    exponent_marker ::= E | e
--    exponent_sign ::= + | -
-- @
genExponent = do
   expChar <- QC.elements "Ee"
   expSign <- QC.elements ["+","-",""]
   expVal <- genInteger 0 1
   return $ (expChar:expSign) ++ expVal

-- |Generate a VHDL specific based literal
-- @
--    based_literal ::=
--       base # based_integer [ . based_integer ] # [ exponent ]
--    base ::= integer
--    based_integer ::=
--       extended_digit { [ underline ] extended_digit }
--    extended_digit ::= digit | letter
-- @
-- Note: \\'#\\' container can instead be \\':\\'
-- Implemented as:
-- @
--    'generateBasedStr' ::=
--       base based_container 'generateExtendedBasedStr' based_container [ 'genExponent' ]
--    base ::= HASKELL( [2..16] )
--    based_container ::= # | :
-- @
-- Note: based_container must be same on both sides
generateBasedStr :: Char -> QC.Gen String
generateBasedStr container = do
   base <- QC.elements [2..16]
   let baseChars = show base
       basedStrGen = generateExtendedBasedStr base
   optionalFractional <- QC.elements [True,False]
   expStr <- genExponent
   unitBasedStr <- basedStrGen
   fullBasedStr <-
         if optionalFractional then do
            decBasedStr <- basedStrGen
            return $ unitBasedStr ++ "." ++ decBasedStr
         else
            return unitBasedStr
   return $ baseChars ++ [container] ++ fullBasedStr ++ [container] ++ expStr

-- |Generate latter part of a VHDL specific based literal
-- @
--    based_literal ::=
--       base # based_integer [ . based_integer ] # [ exponent ]
--    base ::= integer
--    based_integer ::=
--       extended_digit { [ underline ] extended_digit }
--    extended_digit ::= digit | letter
-- @
-- Note: \\'#\\' container can instead be \\':\\'
-- This function generates the latter part IE
-- > 'generateExtendedBasedStr' ::= based_integer [ . based_integer ]
-- Implemented as:
-- @
--    base_integer ::= REGEX( [0-9a-fA-F] )
-- @
-- Note: Relevant base restricts range of regex (only full range for hex/base16)
generateExtendedBasedStr :: Int -> QC.Gen String
generateExtendedBasedStr base = do
   let allowedChars = charMap !! (base-2)
   firstChar <- QC.elements allowedChars
   lengthStr <- QC.elements [0..25]
   otherChars <- replicateM lengthStr $ generateUnderscoreExtendedChar allowedChars
   return $ [firstChar] ++ (concat otherChars)
   where charMap =
            [ "01"
            , ['0'..'2']
            , ['0'..'3']
            , ['0'..'4']
            , ['0'..'5']
            , ['0'..'6']
            , ['0'..'7']
            , ['0'..'8']
            , ['0'..'9']
            , ['0'..'9'] ++ ['a'] ++ ['A']
            , ['0'..'9'] ++ ['a'..'b'] ++ ['A'..'B']
            , ['0'..'9'] ++ ['a'..'c'] ++ ['A'..'C']
            , ['0'..'9'] ++ ['a'..'d'] ++ ['A'..'D']
            , ['0'..'9'] ++ ['a'..'e'] ++ ['A'..'E']
            , ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
            ]

-- |Generate a VHDL specific bit string literal
-- @
--    bit_string_literal ::= base_specifier " bit_value "
--    bit_value ::= extended_digit { [ underline ] extended_digit }
--    base_specified ::= B | O | X
-- @
-- Note: B, O, X are case insensitive.
--       \\'%\\' can be used as a container instead of \\'\\"\\'
-- Implemented as:
-- @
--    'generateBitStr' ::= base_specifier base_container 'generateExtendedStr' base_container
--    base_specifier ::= B | b | O | o | X | x
--    base_container ::= " | %
-- @
-- Note: Base containers must match
generateBitStr :: Char -> QC.Gen String
generateBitStr container = do
   base <- QC.elements [BinBased,OctBased,HexBased]
   baseChar <- QC.elements $ baseCharMap MapS.! base
   bitStr <- generateExtendedStr base
   return $ [baseChar,container] ++ bitStr ++ [container]
   where baseCharMap =
            [ (BinBased,"Bb")
            , (OctBased,"Oo")
            , (HexBased,"Xx")
            ]
            & MapS.fromList

-- |Generate a VHDL specific bit_value
-- Part of VHDL bit string literal
-- > bit_value ::= extended_digit { [ underline ] extended_digit }
-- Implemented as:
-- @
--    'generateExtendedStr' ::= extended_digit { 'generateUnderscoreExtendedChar' }
--    extended_digit ::= REGEX( [0-9a-fA-F] )
-- @
-- Note: Relevant base restricts range of regex (only full range for hex/base16)
generateExtendedStr :: LiteralBase -> QC.Gen String
generateExtendedStr base = do
   let allowedChars = charMap MapS.! base
   firstChar <- QC.elements allowedChars
   lengthStr <- QC.elements [0..100]
   otherChars <- replicateM lengthStr $ generateUnderscoreExtendedChar allowedChars
   return $ [firstChar] ++ (concat otherChars)
   where charMap =
            [ (BinBased,"01")
            , (OctBased,['0'..'7'])
            , (HexBased,['0'..'9']++['A'..'F']++['a'..'f'])
            ]
            & MapS.fromList

-- |Generate latter part of VHDL specific bit_value
-- Part of VHDL bit string literal
-- > bit_value ::= extended_digit { [ underline ] extended_digit }
-- Implemented as:
-- @
--    'generateUnderscoreExtendedChar' ::= [ _ ] extended_digit
--    extended_digit ::= REGEX( [0-9a-fA-F] )
-- @
-- Note: Relevant base restricts range of regex (only full range for hex/base16)
generateUnderscoreExtendedChar :: [Char] -> QC.Gen String
generateUnderscoreExtendedChar allowedChars = do
   optionalUnderscore <- QC.elements [True,False]
   otherChar <- QC.elements allowedChars
   return $
      if optionalUnderscore then ['_',otherChar]
      else [otherChar]

-- |Decimal literal token test type
-- Tests the universal integer ('Univ_Real') type with an exponent
singleDecLit_real_exp :: TestTree
singleDecLit_real_exp = QC.testProperty "Real value with exponent" $
   QC.forAll genVal $ \value ->
      let expectedValue = Right [Literal $ Univ_Real $ read $ filter (/= '_') value]
          lexRun = lexerList value
      in lexRun == expectedValue
   where genVal = do
            intStr <- genInteger 1 10
            expStr <- genExponent
            return $ intStr ++ "." ++ intStr ++ expStr

-- |Decimal literal token test type
-- Tests for 'Univ_Int' and 'Univ_Real' (universal integer and real respectively) zero values:
-- * Unit tests for values without exponent
-- * Random test for values with (random) exponents
singleDecLit_zeroes :: TestTree
singleDecLit_zeroes = testGroup "Zero values"
   [ testCase "0" $ lexerList "0" @?= Right [Literal $ Univ_Int 0]
   , testCase "0.0" $ lexerList "0.0" @?= Right [Literal $ Univ_Real 0.0]
   , QC.testProperty "0[Ee][+-]?[0-9]+" $
         QC.forAll (genExp "0") $ \input -> compareFunc input $ Univ_Int 0
   , QC.testProperty "0.0[Ee][+-]?[0-9]+" $
         QC.forAll (genExp "0.0") $ \input -> compareFunc input $ Univ_Real 0.0
   ]
   where genExp :: String -> QC.Gen String
         genExp start = do
            expStr <- genExponent
            return $ start ++ expStr
         compareFunc :: String -> LitType -> Bool
         compareFunc input value =
            lexerList input == Right [Literal $ value]

-- |Single bit string literal lexer tests
-- A single string that lexes to a literal token containing a bit string literal
-- Has tests for both container types
singleBitStrLiterals :: TestTree
singleBitStrLiterals = testGroup "Single bit strings"
   [ QC.testProperty "with \" containers" $ singleBitStrLiterals_cont '"'
   , QC.testProperty "with % containers" $ singleBitStrLiterals_cont '%'
   ]

-- |Generator for a single bit string literal with specified containers
singleBitStrLiterals_cont :: Char -> QC.Property
singleBitStrLiterals_cont container =
   QC.forAll (generateBitStr container) $ \bitStr ->
      let lexRun = lexerList bitStr
          (baseChar:_:strNoBase) = bitStr
          base = baseMap MapS.! baseChar
          unformattedStr =
            strNoBase
            & init
            & filter (\char -> char /= '_')
            & ByteString.pack
          expectedOutput = Right [Literal $ BitStr base unformattedStr]
      in lexRun == expectedOutput
   where baseMap =
            [ ('B', BinBased)
            , ('b', BinBased)
            , ('O', OctBased)
            , ('o', OctBased)
            , ('X', HexBased)
            , ('x', HexBased)
            ]
            & MapS.fromList

-- |All valid characters that can appear in a VHDL string
validTestStringCharacters :: [Char]
validTestStringCharacters = ['A'..'Z'] ++ ['0'..'9'] ++ ['a'..'z'] ++
   "#&'()*+,.-/:;<=>_!$%@?[\\]^`{}~"

-- |Single string literal lexer tests
-- A single string that lexes to a literal token containing a string literal
-- Has test for empty and random contents of strings, both with different container types
singleStrLiterals :: TestTree
singleStrLiterals = testGroup "Singular strings"
   [ singleEmptyString
   , singleEmptyString_diffCont
   , singleRandomString
   , singleRandomString_diffCont
   ]

-- |Single empty string literal lexer test
-- Unit test for:
-- > ""
singleEmptyString :: TestTree
singleEmptyString = testCase "\"\" == Literal Str \"\"" $
         lexerList "\"\"" @?= Right [Literal $ Str ""]

-- |Single empty string literal lexer test
-- Unit test for:
-- > %%
singleEmptyString_diffCont :: TestTree
singleEmptyString_diffCont = testCase "%% == Literal Str \"\"" $
         lexerList "%%" @?= Right [Literal $ Str ""]

-- |Single random string literal lexer test
-- Random string with \\" containers
singleRandomString :: TestTree
singleRandomString = QC.testProperty "Single random string with \" containers" $
   QC.forAll generateRandomString $ \stringContents ->
      let lexRun =
            replicateConts stringContents []
            & \lexInput -> "\"" ++ lexInput ++ "\""
            & lexerList
          expectedAnswer = Right [Literal $ Str stringContents]
      in lexRun == expectedAnswer
   where generateRandomString = do
            stringLength <- QC.elements [1..200]
            let charGenerator = QC.elements validTestCharacters
            replicateM stringLength charGenerator
         replicateConts (head:tail) output =
            if head == '"' then replicateConts tail ("\"\"" ++ output)
            else replicateConts tail (head:output)
         replicateConts [] output = reverse output

-- |Single random string literal lexer test
-- Random string with \\% containers
singleRandomString_diffCont :: TestTree
singleRandomString_diffCont = QC.testProperty "Single random string with % containers" $
   QC.forAll generateRandomString $ \stringContents ->
      let lexRun =
            replicateConts stringContents []
            & \lexInput -> "%" ++ lexInput ++ "%"
            & lexerList
          expectedAnswer = Right [Literal $ Str stringContents]
      in lexRun == expectedAnswer
   where generateRandomString = do
            stringLength <- QC.elements [1..200]
            let charGenerator = QC.elements validTestStringCharacters
            replicateM stringLength charGenerator
         replicateConts (head:tail) output =
            if head == '%' then replicateConts tail ('%':'%':output)
            else replicateConts tail (head:output)
         replicateConts [] output = reverse output

-- |All valid characters that can appear in a VHDL character literals
validTestCharacters :: [Char]
validTestCharacters = ['A'..'Z'] ++ ['0'..'9'] ++ ['a'..'z'] ++
   "\"#&'()*+,.-/:;<=>_!$%@?[\\]^`{}~"

-- |Single character literal lexer tests
-- A single string that lexes to a character literal token
singleCharLiterals :: TestTree
singleCharLiterals = QC.testProperty "Single random character" $
   QC.forAll (QC.elements validTestCharacters) $ \selectedChar ->
      let lexRun = lexerList $ "'" ++ [selectedChar] ++ "'"
          expectedAnswer = Right [Literal $ Character selectedChar]
      in lexRun == expectedAnswer

-- |Single identifier literal lexer tests
-- A single string that lexes to a identifier token
singleIdentifiers :: TestTree
singleIdentifiers = QC.testProperty "Single identifier" $
   QC.forAll generateIdentifier $ \identifierStr ->
      let lexRun = lexerList identifierStr
          expectedAnswer = Right [Identifier identifierStr]
      in lexRun == expectedAnswer
   where generateIdentifier = do
            stringLength <- QC.elements [0..200]
            fstChar <- QC.elements validStartChar
            let genOtherChars = QC.elements validOtherChar
            otherChars <- replicateM stringLength genOtherChars
            return $ fstChar:otherChars
         validStartChar = ['a'..'z'] ++ ['A'..'Z']
         validOtherChar = validStartChar ++ ['0'..'9'] ++ ['_']
