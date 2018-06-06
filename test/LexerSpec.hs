module LexerSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Control.Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.State (evalStateT)
import Data.Function ((&))
import qualified Data.Map.Strict as MapS
import Data.List.Split (splitOneOf,splitOn)
import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.Char (toUpper,digitToInt)
import Data.List (findIndex)
import Numeric (readInt)

import Lexer.Lexer (lexerList)
import Lexer.Types.Token
import Lexer.Types.Error
import Lexer.Types.PositionWrapper
import Lexer.Alex.Types (AlexPosn(..))
import Parser.Netlist.Types.Stores (emptyNetlistStore)
import Manager.Types.Error (ConverterError(ConverterError_Parse))

import Generators.LexElements
         ( genInteger
         , genExponent
         , genBasedStr
         , genBitStr
         , genIdentifier
         , genDecimal
         )

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
   , singleIdentifiers
   ]

-- |Single keyword lexer tests
-- A single string that lexes to a keyword token
singleKeywords :: TestTree
singleKeywords = testGroup "Single keyword tests for lexer"
   [ singleKeywordsUpper
   , singleKeywordsLower
   ]

-- |Run the lexer and get the result
getLexResult :: String -> IO (Either ConverterError [Token])
getLexResult input = runExceptT $ evalStateT (lexerList input) emptyNetlistStore

-- |Compare a single unit input
compareBasicUnit :: String -> Token -> Assertion
compareBasicUnit input output = do
   result <- getLexResult input
   let expectedOutput = Right [output]
   result @?= expectedOutput

-- |Single upper case keyword lexer unit tests
-- Unit tests with single upper case strings that lex to keyword tokens
singleKeywordsUpper :: TestTree
singleKeywordsUpper = testGroup "Single upper case keywords"
   [ testCase "\"ABS\" == Keyword Abs" $
         compareBasicUnit "ABS" $ Keyword Abs
   , testCase "\"ACCESS\" == Keyword Access" $
         compareBasicUnit "ACCESS" $ Keyword Access
   , testCase "\"AFTER\" == Keyword After" $
         compareBasicUnit "AFTER" $ Keyword After
   , testCase "\"ALIAS\" == Keyword Alias" $
         compareBasicUnit "ALIAS" $ Keyword Alias
   , testCase "\"ALL\" == Keyword All" $
         compareBasicUnit "ALL" $ Keyword All
   , testCase "\"AND\" == Keyword And" $
         compareBasicUnit "AND" $ Keyword And
   , testCase "\"ARCHITECTURE\" == Keyword Architecture" $
         compareBasicUnit "ARCHITECTURE" $ Keyword Architecture
   , testCase "\"ARRAY\" == Keyword Array" $
         compareBasicUnit "ARRAY" $ Keyword Array
   , testCase "\"ASSERT\" == Keyword Assert" $
         compareBasicUnit "ASSERT" $ Keyword Assert
   , testCase "\"ATTRIBUTE\" == Keyword Attribute" $
         compareBasicUnit "ATTRIBUTE" $ Keyword Attribute
   , testCase "\"BEGIN\" == Keyword Begin" $
         compareBasicUnit "BEGIN" $ Keyword Begin
   , testCase "\"BLOCK\" == Keyword Block" $
         compareBasicUnit "BLOCK" $ Keyword Block
   , testCase "\"BODY\" == Keyword Body" $
         compareBasicUnit "BODY" $ Keyword Body
   , testCase "\"BUFFER\" == Keyword Buffer" $
         compareBasicUnit "BUFFER" $ Keyword Buffer
   , testCase "\"BUS\" == Keyword Bus" $
         compareBasicUnit "BUS" $ Keyword Bus
   , testCase "\"CASE\" == Keyword Case" $
         compareBasicUnit "CASE" $ Keyword Case
   , testCase "\"COMPONENT\" == Keyword Component" $
         compareBasicUnit "COMPONENT" $ Keyword Component
   , testCase "\"CONFIGURATION\" == Keyword Configuration" $
         compareBasicUnit "CONFIGURATION" $ Keyword Configuration
   , testCase "\"CONSTANT\" == Keyword Constant" $
         compareBasicUnit "CONSTANT" $ Keyword Constant
   , testCase "\"DISCONNECT\" == Keyword Disconnect" $
         compareBasicUnit "DISCONNECT" $ Keyword Disconnect
   , testCase "\"DOWNTO\" == Keyword Downto" $
         compareBasicUnit "DOWNTO" $ Keyword Downto
   , testCase "\"ELSE\" == Keyword Else" $
         compareBasicUnit "ELSE" $ Keyword Else
   , testCase "\"ELSIF\" == Keyword Elsif" $
         compareBasicUnit "ELSIF" $ Keyword Elsif
   , testCase "\"END\" == Keyword End" $
         compareBasicUnit "END" $ Keyword End
   , testCase "\"ENTITY\" == Keyword Entity" $
         compareBasicUnit "ENTITY" $ Keyword Entity
   , testCase "\"EXIT\" == Keyword Exit" $
         compareBasicUnit "EXIT" $ Keyword Exit
   , testCase "\"FILE\" == Keyword File" $
         compareBasicUnit "FILE" $ Keyword File
   , testCase "\"FOR\" == Keyword For" $
         compareBasicUnit "FOR" $ Keyword For
   , testCase "\"FUNCTION\" == Keyword Function" $
         compareBasicUnit "FUNCTION" $ Keyword Function
   , testCase "\"GENERATE\" == Keyword Generate" $
         compareBasicUnit "GENERATE" $ Keyword Generate
   , testCase "\"GENERIC\" == Keyword Generic" $
         compareBasicUnit "GENERIC" $ Keyword Generic
   , testCase "\"GUARDED\" == Keyword Guarded" $
         compareBasicUnit "GUARDED" $ Keyword Guarded
   , testCase "\"IF\" == Keyword If" $
         compareBasicUnit "IF" $ Keyword If
   , testCase "\"IN\" == Keyword In" $
         compareBasicUnit "IN" $ Keyword In
   , testCase "\"INOUT\" == Keyword Inout" $
         compareBasicUnit "INOUT" $ Keyword Inout
   , testCase "\"IS\" == Keyword Is" $
         compareBasicUnit "IS" $ Keyword Is
   , testCase "\"LABEL\" == Keyword Label" $
         compareBasicUnit "LABEL" $ Keyword Label
   , testCase "\"LIBRARY\" == Keyword Library" $
         compareBasicUnit "LIBRARY" $ Keyword Library
   , testCase "\"LINKAGE\" == Keyword Linkage" $
         compareBasicUnit "LINKAGE" $ Keyword Linkage
   , testCase "\"LOOP\" == Keyword Loop" $
         compareBasicUnit "LOOP" $ Keyword Loop
   , testCase "\"MAP\" == Keyword Map" $
         compareBasicUnit "MAP" $ Keyword Map
   , testCase "\"MOD\" == Keyword Mod" $
         compareBasicUnit "MOD" $ Keyword Mod
   , testCase "\"NAND\" == Keyword Nand" $
         compareBasicUnit "NAND" $ Keyword Nand
   , testCase "\"NEW\" == Keyword New" $
         compareBasicUnit "NEW" $ Keyword New
   , testCase "\"NEXT\" == Keyword Next" $
         compareBasicUnit "NEXT" $ Keyword Next
   , testCase "\"NOR\" == Keyword Nor" $
         compareBasicUnit "NOR" $ Keyword Nor
   , testCase "\"NOT\" == Keyword Not" $
         compareBasicUnit "NOT" $ Keyword Not
   , testCase "\"NULL\" == Keyword Null" $
         compareBasicUnit "NULL" $ Keyword Null
   , testCase "\"OF\" == Keyword Of" $
         compareBasicUnit "OF" $ Keyword Of
   , testCase "\"ON\" == Keyword On" $
         compareBasicUnit "ON" $ Keyword On
   , testCase "\"OPEN\" == Keyword Open" $
         compareBasicUnit "OPEN" $ Keyword Open
   , testCase "\"OR\" == Keyword Or" $
         compareBasicUnit "OR" $ Keyword Or
   , testCase "\"OTHERS\" == Keyword Others" $
         compareBasicUnit "OTHERS" $ Keyword Others
   , testCase "\"OUT\" == Keyword Out" $
         compareBasicUnit "OUT" $ Keyword Out
   , testCase "\"PACKAGE\" == Keyword Package" $
         compareBasicUnit "PACKAGE" $ Keyword Package
   , testCase "\"PORT\" == Keyword Port" $
         compareBasicUnit "PORT" $ Keyword Port
   , testCase "\"PROCEDURE\" == Keyword Procedure" $
         compareBasicUnit "PROCEDURE" $ Keyword Procedure
   , testCase "\"PROCESS\" == Keyword Process" $
         compareBasicUnit "PROCESS" $ Keyword Process
   , testCase "\"RANGE\" == Keyword Range" $
         compareBasicUnit "RANGE" $ Keyword Range
   , testCase "\"RECORD\" == Keyword Record" $
         compareBasicUnit "RECORD" $ Keyword Record
   , testCase "\"REGISTER\" == Keyword Register" $
         compareBasicUnit "REGISTER" $ Keyword Register
   , testCase "\"REM\" == Keyword Rem" $
         compareBasicUnit "REM" $ Keyword Rem
   , testCase "\"REPORT\" == Keyword Report" $
         compareBasicUnit "REPORT" $ Keyword Report
   , testCase "\"RETURN\" == Keyword Return" $
         compareBasicUnit "RETURN" $ Keyword Return
   , testCase "\"SELECT\" == Keyword Select" $
         compareBasicUnit "SELECT" $ Keyword Select
   , testCase "\"SEVERITY\" == Keyword Severity" $
         compareBasicUnit "SEVERITY" $ Keyword Severity
   , testCase "\"SIGNAL\" == Keyword Signal" $
         compareBasicUnit "SIGNAL" $ Keyword Signal
   , testCase "\"SUBTYPE\" == Keyword Subtype" $
         compareBasicUnit "SUBTYPE" $ Keyword Subtype
   , testCase "\"THEN\" == Keyword Then" $
         compareBasicUnit "THEN" $ Keyword Then
   , testCase "\"TO\" == Keyword To" $
         compareBasicUnit "TO" $ Keyword To
   , testCase "\"TRANSPORT\" == Keyword Transport" $
         compareBasicUnit "TRANSPORT" $ Keyword Transport
   , testCase "\"TYPE\" == Keyword Type" $
         compareBasicUnit "TYPE" $ Keyword Type
   , testCase "\"UNITS\" == Keyword Units" $
         compareBasicUnit "UNITS" $ Keyword Units
   , testCase "\"UNTIL\" == Keyword Until" $
         compareBasicUnit "UNTIL" $ Keyword Until
   , testCase "\"USE\" == Keyword Use" $
         compareBasicUnit "USE" $ Keyword Use
   , testCase "\"VARIABLE\" == Keyword Variable" $
         compareBasicUnit "VARIABLE" $ Keyword Variable
   , testCase "\"WAIT\" == Keyword Wait" $
         compareBasicUnit "WAIT" $ Keyword Wait
   , testCase "\"WHEN\" == Keyword When" $
         compareBasicUnit "WHEN" $ Keyword When
   , testCase "\"WHILE\" == Keyword While" $
         compareBasicUnit "WHILE" $ Keyword While
   , testCase "\"WITH\" == Keyword With" $
         compareBasicUnit "WITH" $ Keyword With
   , testCase "\"XOR\" == Keyword Xor" $
         compareBasicUnit "XOR" $ Keyword Xor
   ]

-- |Single lower case keyword lexer unit tests
-- Unit tests with single lower case strings that lex to keyword tokens
singleKeywordsLower :: TestTree
singleKeywordsLower = testGroup "Single lower case keywords"
   [ testCase "\"abs\" == Keyword Abs" $
         compareBasicUnit "abs" $ Keyword Abs
   , testCase "\"access\" == Keyword Access" $
         compareBasicUnit "access" $ Keyword Access
   , testCase "\"after\" == Keyword After" $
         compareBasicUnit "after" $ Keyword After
   , testCase "\"alias\" == Keyword Alias" $
         compareBasicUnit "alias" $ Keyword Alias
   , testCase "\"all\" == Keyword All" $
         compareBasicUnit "all" $ Keyword All
   , testCase "\"and\" == Keyword And" $
         compareBasicUnit "and" $ Keyword And
   , testCase "\"architecture\" == Keyword Architecture" $
         compareBasicUnit "architecture" $ Keyword Architecture
   , testCase "\"array\" == Keyword Array" $
         compareBasicUnit "array" $ Keyword Array
   , testCase "\"assert\" == Keyword Assert" $
         compareBasicUnit "assert" $ Keyword Assert
   , testCase "\"attribute\" == Keyword Attribute" $
         compareBasicUnit "attribute" $ Keyword Attribute
   , testCase "\"begin\" == Keyword Begin" $
         compareBasicUnit "begin" $ Keyword Begin
   , testCase "\"block\" == Keyword Block" $
         compareBasicUnit "block" $ Keyword Block
   , testCase "\"body\" == Keyword Body" $
         compareBasicUnit "body" $ Keyword Body
   , testCase "\"buffer\" == Keyword Buffer" $
         compareBasicUnit "buffer" $ Keyword Buffer
   , testCase "\"bus\" == Keyword Bus" $
         compareBasicUnit "bus" $ Keyword Bus
   , testCase "\"case\" == Keyword Case" $
         compareBasicUnit "case" $ Keyword Case
   , testCase "\"component\" == Keyword Component" $
         compareBasicUnit "component" $ Keyword Component
   , testCase "\"configuration\" == Keyword Configuration" $
         compareBasicUnit "configuration" $ Keyword Configuration
   , testCase "\"constant\" == Keyword Constant" $
         compareBasicUnit "constant" $ Keyword Constant
   , testCase "\"disconnect\" == Keyword Disconnect" $
         compareBasicUnit "disconnect" $ Keyword Disconnect
   , testCase "\"downto\" == Keyword Downto" $
         compareBasicUnit "downto" $ Keyword Downto
   , testCase "\"else\" == Keyword Else" $
         compareBasicUnit "else" $ Keyword Else
   , testCase "\"elsif\" == Keyword Elsif" $
         compareBasicUnit "elsif" $ Keyword Elsif
   , testCase "\"end\" == Keyword End" $
         compareBasicUnit "end" $ Keyword End
   , testCase "\"entity\" == Keyword Entity" $
         compareBasicUnit "entity" $ Keyword Entity
   , testCase "\"exit\" == Keyword Exit" $
         compareBasicUnit "exit" $ Keyword Exit
   , testCase "\"file\" == Keyword File" $
         compareBasicUnit "file" $ Keyword File
   , testCase "\"for\" == Keyword For" $
         compareBasicUnit "for" $ Keyword For
   , testCase "\"function\" == Keyword Function" $
         compareBasicUnit "function" $ Keyword Function
   , testCase "\"generate\" == Keyword Generate" $
         compareBasicUnit "generate" $ Keyword Generate
   , testCase "\"generic\" == Keyword Generic" $
         compareBasicUnit "generic" $ Keyword Generic
   , testCase "\"guarded\" == Keyword Guarded" $
         compareBasicUnit "guarded" $ Keyword Guarded
   , testCase "\"if\" == Keyword If" $
         compareBasicUnit "if" $ Keyword If
   , testCase "\"in\" == Keyword In" $
         compareBasicUnit "in" $ Keyword In
   , testCase "\"inout\" == Keyword Inout" $
         compareBasicUnit "inout" $ Keyword Inout
   , testCase "\"is\" == Keyword Is" $
         compareBasicUnit "is" $ Keyword Is
   , testCase "\"label\" == Keyword Label" $
         compareBasicUnit "label" $ Keyword Label
   , testCase "\"library\" == Keyword Library" $
         compareBasicUnit "library" $ Keyword Library
   , testCase "\"linkage\" == Keyword Linkage" $
         compareBasicUnit "linkage" $ Keyword Linkage
   , testCase "\"loop\" == Keyword Loop" $
         compareBasicUnit "loop" $ Keyword Loop
   , testCase "\"map\" == Keyword Map" $
         compareBasicUnit "map" $ Keyword Map
   , testCase "\"mod\" == Keyword Mod" $
         compareBasicUnit "mod" $ Keyword Mod
   , testCase "\"nand\" == Keyword Nand" $
         compareBasicUnit "nand" $ Keyword Nand
   , testCase "\"new\" == Keyword New" $
         compareBasicUnit "new" $ Keyword New
   , testCase "\"next\" == Keyword Next" $
         compareBasicUnit "next" $ Keyword Next
   , testCase "\"nor\" == Keyword Nor" $
         compareBasicUnit "nor" $ Keyword Nor
   , testCase "\"not\" == Keyword Not" $
         compareBasicUnit "not" $ Keyword Not
   , testCase "\"null\" == Keyword Null" $
         compareBasicUnit "null" $ Keyword Null
   , testCase "\"of\" == Keyword Of" $
         compareBasicUnit "of" $ Keyword Of
   , testCase "\"on\" == Keyword On" $
         compareBasicUnit "on" $ Keyword On
   , testCase "\"open\" == Keyword Open" $
         compareBasicUnit "open" $ Keyword Open
   , testCase "\"or\" == Keyword Or" $
         compareBasicUnit "or" $ Keyword Or
   , testCase "\"others\" == Keyword Others" $
         compareBasicUnit "others" $ Keyword Others
   , testCase "\"out\" == Keyword Out" $
         compareBasicUnit "out" $ Keyword Out
   , testCase "\"package\" == Keyword Package" $
         compareBasicUnit "package" $ Keyword Package
   , testCase "\"port\" == Keyword Port" $
         compareBasicUnit "port" $ Keyword Port
   , testCase "\"procedure\" == Keyword Procedure" $
         compareBasicUnit "procedure" $ Keyword Procedure
   , testCase "\"process\" == Keyword Process" $
         compareBasicUnit "process" $ Keyword Process
   , testCase "\"range\" == Keyword Range" $
         compareBasicUnit "range" $ Keyword Range
   , testCase "\"record\" == Keyword Record" $
         compareBasicUnit "record" $ Keyword Record
   , testCase "\"register\" == Keyword Register" $
         compareBasicUnit "register" $ Keyword Register
   , testCase "\"rem\" == Keyword Rem" $
         compareBasicUnit "rem" $ Keyword Rem
   , testCase "\"report\" == Keyword Report" $
         compareBasicUnit "report" $ Keyword Report
   , testCase "\"return\" == Keyword Return" $
         compareBasicUnit "return" $ Keyword Return
   , testCase "\"select\" == Keyword Select" $
         compareBasicUnit "select" $ Keyword Select
   , testCase "\"severity\" == Keyword Severity" $
         compareBasicUnit "severity" $ Keyword Severity
   , testCase "\"signal\" == Keyword Signal" $
         compareBasicUnit "signal" $ Keyword Signal
   , testCase "\"subtype\" == Keyword Subtype" $
         compareBasicUnit "subtype" $ Keyword Subtype
   , testCase "\"then\" == Keyword Then" $
         compareBasicUnit "then" $ Keyword Then
   , testCase "\"to\" == Keyword To" $
         compareBasicUnit "to" $ Keyword To
   , testCase "\"transport\" == Keyword Transport" $
         compareBasicUnit "transport" $ Keyword Transport
   , testCase "\"type\" == Keyword Type" $
         compareBasicUnit "type" $ Keyword Type
   , testCase "\"units\" == Keyword Units" $
         compareBasicUnit "units" $ Keyword Units
   , testCase "\"until\" == Keyword Until" $
         compareBasicUnit "until" $ Keyword Until
   , testCase "\"use\" == Keyword Use" $
         compareBasicUnit "use" $ Keyword Use
   , testCase "\"variable\" == Keyword Variable" $
         compareBasicUnit "variable" $ Keyword Variable
   , testCase "\"wait\" == Keyword Wait" $
         compareBasicUnit "wait" $ Keyword Wait
   , testCase "\"when\" == Keyword When" $
         compareBasicUnit "when" $ Keyword When
   , testCase "\"while\" == Keyword While" $
         compareBasicUnit "while" $ Keyword While
   , testCase "\"with\" == Keyword With" $
         compareBasicUnit "with" $ Keyword With
   , testCase "\"xor\" == Keyword Xor" $
         compareBasicUnit "xor" $ Keyword Xor
   ]

-- |Single operator lexer unit tests
-- Unit tests with single strings that lex to operator tokens
singleOperators :: TestTree
singleOperators = testGroup "Single operators"
   [ testCase "\"=>\" == Arrow" $
         compareBasicUnit "=>" $ Operator Arrow
   , testCase "\"**\" == DoubleStar" $
         compareBasicUnit "**" $ Operator DoubleStar
   , testCase "\":=\" == VarAssign" $
         compareBasicUnit ":=" $ Operator VarAssign
   , testCase "\"/=\" == Inequality" $
         compareBasicUnit "/=" $ Operator Inequality
   , testCase "\">=\" == GreaterThanOrEqual" $
         compareBasicUnit ">=" $ Operator GreaterThanOrEqual
   , testCase "\"<=\" == SignAssign" $
         compareBasicUnit "<=" $ Operator SignAssign
   , testCase "\"<>\" == Box" $
         compareBasicUnit "<>" $ Operator Box
   , testCase "\"&\" == Ampersand" $
         compareBasicUnit "&" $ Operator Ampersand
   , testCase "\"'\" == Apostrophe" $
         compareBasicUnit "'" $ Operator Apostrophe
   , testCase "\"(\" == LeftParen" $
         compareBasicUnit "(" $ Operator LeftParen
   , testCase "\")\" == RightParen" $
         compareBasicUnit ")" $ Operator RightParen
   , testCase "\"*\" == Star" $
         compareBasicUnit "*" $ Operator Star
   , testCase "\"+\" == Plus" $
         compareBasicUnit "+" $ Operator Plus
   , testCase "\",\" == Comma" $
         compareBasicUnit "," $ Operator Comma
   , testCase "\"-\" == Hyphen" $
         compareBasicUnit "-" $ Operator Hyphen
   , testCase "\".\" == Period" $
         compareBasicUnit "." $ Operator Period
   , testCase "\"/\" == Slash" $
         compareBasicUnit "/" $ Operator Slash
   , testCase "\":\" == Colon" $
         compareBasicUnit ":" $ Operator Colon
   , testCase "\";\" == Semicolon" $
         compareBasicUnit ";" $ Operator Semicolon
   , testCase "\"<\" == LessThan" $
         compareBasicUnit "<" $ Operator LessThan
   , testCase "\"=\" == Equal" $
         compareBasicUnit "=" $ Operator Equal
   , testCase "\">\" == GreaterThan" $
         compareBasicUnit ">" $ Operator GreaterThan
   , testCase "\"|\" == Bar" $
         compareBasicUnit "|" $ Operator Bar
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
   QC.ioProperty $ do
      basedStr <- QC.generate $ genBasedStr container
      lexRun <- getLexResult basedStr
      let filteredBasedStr = filter (\char -> char /= '_') basedStr
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
               in Left $ ConverterError_Parse $ PosnWrapper { getPos = AlexPn 0 1 0, unPos = errorType basedStr }
            else
               Right $
                  [ Literal $ convertedValue
                  & if floor convertedValue == ceiling convertedValue then Univ_Int . floor else Univ_Real
                  ]
      return $ lexRun == expectedOutput
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
   let expression :: Int -> IO Bool
       expression value = do
         lexRun <- getLexResult $ show value
         let expectedOutput = Right [Literal $ Univ_Int $ fromIntegral value]
         return $ lexRun == expectedOutput
   in QC.ioProperty $ do
         val <- QC.generate QC.arbitrary
         expression $ abs val

-- |Decimal literal token test type
-- Tests the universal real ('Univ_Real') type without an exponent
singleDecLit_real :: TestTree
singleDecLit_real = QC.testProperty "Real value without exponent" $
   let expression :: Double -> IO Bool
       expression value = do
         lexRun <- getLexResult $ show value
         let expectedOutput = Right [Literal $ Univ_Real value]
         return $ lexRun == expectedOutput
   in QC.ioProperty $ do
         val <- QC.generate QC.arbitrary
         expression $ abs val

-- |Decimal literal token test type
-- Tests the universal integer ('Univ_Int') type with an exponent
singleDecLit_int_exp :: TestTree
singleDecLit_int_exp = QC.testProperty "Integer value with exponent" $
   QC.ioProperty $ do
      value <- QC.generate $ genDecimal (1,10) Nothing True
      let [base,exp] =
            filter (\char -> not $ elem char "+_") value
            & splitOneOf "Ee"
            & fmap read
          doubleValue :: Double -- ?? Probably needs changing to deal with numbers too large for double, potentially use unbounded Integer instead
          doubleValue = base * 10 ** exp
      expectedValue <- return $
         if isInfinite doubleValue then
               Left $ ConverterError_Parse $ PosnWrapper { getPos = AlexPn 0 1 0, unPos = LexErr_UniversalInt_OutOfBounds value }
         else Right [Literal $ Univ_Int $ floor doubleValue]
      lexRun <- getLexResult value
      return $ lexRun == expectedValue

-- |Decimal literal token test type
-- Tests the universal integer ('Univ_Real') type with an exponent
singleDecLit_real_exp :: TestTree
singleDecLit_real_exp = QC.testProperty "Real value with exponent" $
   QC.ioProperty $ do
      value <- QC.generate $ genDecimal (1,10) (Just (1,10)) True
      expectedValue <- return $ Right [Literal $ Univ_Real $ read $ filter (/= '_') value]
      lexRun <- getLexResult value
      return $ lexRun == expectedValue

-- |Decimal literal token test type
-- Tests for 'Univ_Int' and 'Univ_Real' (universal integer and real respectively) zero values:
-- * Unit tests for values without exponent
-- * Random test for values with (random) exponents
singleDecLit_zeroes :: TestTree
singleDecLit_zeroes = testGroup "Zero values"
   [ testCase "0" $ compareBasicUnit "0" $ Literal $ Univ_Int 0
   , testCase "0.0" $ compareBasicUnit "0.0" $ Literal $ Univ_Real 0.0
   , QC.testProperty "0[Ee][+-]?[0-9]+" $
         QC.ioProperty $ do
            input <- QC.generate $ genExp "0"
            compareFunc input $ Univ_Int 0
   , QC.testProperty "0.0[Ee][+-]?[0-9]+" $
         QC.ioProperty $ do
            input <- QC.generate $ genExp "0.0"
            compareFunc input $ Univ_Real 0.0
   ]
   where genExp :: String -> QC.Gen String
         genExp start = do
            expStr <- genExponent
            return $ start ++ expStr
         compareFunc :: String -> LitType -> IO Bool
         compareFunc input value = do
            lexRun <- getLexResult input
            let expectedOutput = Right [Literal value]
            return $ lexRun == expectedOutput

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
   QC.ioProperty $ do
      bitStr <- QC.generate $ genBitStr container
      lexRun <- getLexResult bitStr
      let (baseChar:_:strNoBase) = bitStr
          base = baseMap MapS.! baseChar
          unformattedStr =
            strNoBase
            & init
            & filter (\char -> char /= '_')
            & ByteString.pack
          expectedOutput = Right [Literal $ BitStr base unformattedStr]
      return $ lexRun == expectedOutput
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
         compareBasicUnit "\"\"" $ Literal $ Str ""

-- |Single empty string literal lexer test
-- Unit test for:
-- > %%
singleEmptyString_diffCont :: TestTree
singleEmptyString_diffCont = testCase "%% == Literal Str \"\"" $
         compareBasicUnit "%%" $ Literal $ Str ""

-- |Single random string literal lexer test
-- Random string with \\" containers
singleRandomString :: TestTree
singleRandomString = QC.testProperty "Single random string with \" containers" $
   QC.ioProperty $ do
      stringContents <- QC.generate generateRandomString
      lexRun <- getLexResult $
         replicateConts stringContents []
         & \lexInput -> "\"" ++ lexInput ++ "\""
      expectedAnswer <- return $ Right [Literal $ Str stringContents]
      return $ lexRun == expectedAnswer
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
   QC.ioProperty $ do
      stringContents <- QC.generate generateRandomString
      lexRun <- getLexResult $
         replicateConts stringContents []
         & \lexInput -> "%" ++ lexInput ++ "%"
      let expectedAnswer = Right [Literal $ Str stringContents]
      return $ lexRun == expectedAnswer
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
   QC.ioProperty $ do
      selectedChar <- QC.generate $ QC.elements validTestCharacters
      lexRun <- getLexResult $ "'" ++ [selectedChar] ++ "'"
      let expectedAnswer = Right [Literal $ Character selectedChar]
      return $ lexRun == expectedAnswer

-- |Single identifier literal lexer tests
-- A single string that lexes to a identifier token
singleIdentifiers :: TestTree
singleIdentifiers = QC.testProperty "Single identifier" $
   QC.ioProperty $ do
      identifierStr <- QC.generate $ genIdentifier 0 100
      lexRun <- getLexResult identifierStr
      let expectedAnswer = Right [Identifier identifierStr]
      return $ lexRun == expectedAnswer
