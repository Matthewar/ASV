{-|
   Module      : Spec.Parser.Combinators.Lex
   Description : Tests for lexical element combinators
|-}
module Spec.Parser.Combinators.Lex (tests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import Control.Monad (replicateM)
import Control.Applicative (liftA2)
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Data.Int
         ( Int16
         , Int64
         )
import Numeric
         ( showFFloat
         , showEFloat
         , showFFloatAlt
         )
import Text.Parsec (parse)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator
         ( eof
         , manyTill
         )
import Text.Parsec.String (Parser)
import Text.Regex
         ( matchRegex
         , mkRegexWithOpts
         )

import Parser.Combinators.Lex
import Parser.Combinators.Lex.Internal
import Parser.Types.Token
         ( mkUpperString
         , AbstractLiteral(..)
         )

import Types
         ( ExpectedOutput(..)
         , ParserExpectedOutput
         )
import Spec.Generators.LexElements
         ( genInteger
         , genExponent
         , genBitStr
         , genIdentifier
         , genComment
         , intersperseUnderscores
         )

-- |All tests for the module "Parser.Combinators.Lex"
-- Includes tests for "Parser.Combinators.Lex.Internal"
tests :: TestTree
tests = testGroup "Lexical element combinator tests"
   [ identifiers
   , abstractLiterals
   , characters
   , strings
   , bitStrings
   , comments
   , internals
   ]

-- |Tests for identifiers
identifiers :: TestTree
identifiers = testGroup "Identifiers"
   [ validIdentifiers
   , invalidIdentifiers
   ]

-- |Valid identifier tests
validIdentifiers :: TestTree
validIdentifiers = QC.testProperty "Valid identifiers" $
   QC.forAll (genIdentifier 1 200) $ \iden -> (parse identifier "TEST" iden) == Right (mkUpperString iden)

-- |Invalid identifier tests
invalidIdentifiers :: TestTree
invalidIdentifiers = QC.testProperty "Invalid identifiers" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ \notIden -> isLeft $ parse identifier "TEST" notIden
   where checkInvalid = isNothing . (matchRegex $ mkRegexWithOpts "^[a-zA-Z](_?[a-zA-Z0-9])*" True True)

-- |Tests for abstract literals
abstractLiterals :: TestTree
abstractLiterals = testGroup "Abstract literals"
   [ decimalLiterals
   --, basedLiterals
   , invalidAbstractFormatting
   ]

-- |Tests for decimal literals
decimalLiterals :: TestTree
decimalLiterals = testGroup "Decimal literals"
   [ validDecimals
   , invalidDecimals
   ]

-- |Tests for valid decimal literals
-- In this case valid is taken to not only include values that successfully parse, but values that successfully parse without rounding
validDecimals :: TestTree
validDecimals = testGroup "Valid decimal literals"
   [ validDecimalIntegers
   , validDecimalReals
   ]

-- |Tests for decimal literals that parse to integers
-- Any decimal without a decimal point
validDecimalIntegers :: TestTree
validDecimalIntegers = QC.testProperty "Valid integer-kind decimal literals" $
   QC.forAll genDecimalInteger $ \(ExpectedOutput input expectedOutput) -> (parse abstractLiteral "TEST" input) == Right (UniversalInteger expectedOutput)
   where genDecimalInteger :: QC.Gen (ParserExpectedOutput Int64)
         genDecimalInteger = QC.oneof
                              [ noExponent
                              , positiveExponent
                              , negativeExponent
                              ]
         noExponent :: QC.Gen (ParserExpectedOutput Int64)
         noExponent = let gen (ExpectedOutput input value) = ExpectedOutput input (fromIntegral value)
                      in gen <$> genInteger Nothing Nothing
         positiveExponent = do
            (minValue,maxExponent) <- QC.suchThat 
                                       ( (,)
                                     <$> QC.choose (0,2^7)
                                     <*> QC.choose (0,maxBound :: Int16)
                                       )
                                       $ \(baseVal,exp) -> (baseVal * 10 ^ exp) <= toInteger (maxBound :: Int64)
            actualExponent <- QC.choose (0,maxExponent)
            exponentChar <- QC.elements "Ee"
            valueStr <- intersperseUnderscores $ show $ minValue * 10 ^ (maxExponent - actualExponent)
            exponentStr <- intersperseUnderscores $ show actualExponent
            let inputStr = valueStr ++ [exponentChar] ++ exponentStr
                expectedOutput = fromIntegral $ minValue * 10 ^ maxExponent
            return $ ExpectedOutput inputStr expectedOutput
         negativeExponent :: QC.Gen (ParserExpectedOutput Int64)
         negativeExponent = do
            expectedValue <- QC.choose (0,maxBound :: Int64)
            shiftVal <- QC.choose (0,2^7) -- Number of zeros on end of value
            let actualValue = intersperseUnderscores $ show expectedValue ++ replicate shiftVal '0'
            input <- (++)
                     <$> actualValue
                     <*> ( (\e val -> (e:'-':val))
                       <$> QC.elements "Ee"
                       <*> intersperseUnderscores (show shiftVal)
                         )
            return $ ExpectedOutput input expectedValue

-- |Tests for decimal literals that parse to reals
validDecimalReals :: TestTree
validDecimalReals = QC.testProperty "Valid real-kind decimal literals" $
   QC.forAll genReal $ \(ExpectedOutput input expectedOutput) -> (parse abstractLiteral "TEST" input) == Right (UniversalReal expectedOutput)
   where genReal :: QC.Gen (ParserExpectedOutput Double)
         genReal = do
            expectedOutput <- QC.suchThat QC.arbitrary (>=0)
            showFunction <- QC.elements [showEFloat,showFFloat,showFFloatAlt]
            let input = (showFunction Nothing expectedOutput) ""
            return $ ExpectedOutput input expectedOutput

-- |Tests for invalid decimal literals
invalidDecimals :: TestTree
invalidDecimals = testGroup "Invalid decimals"
   [ invalidDecimalRealAccuracy
   --, invalidDecimalOutOfBounds
   ]

-- |Tests for inaccurate decimal literals
-- Abstract (decimal) literals that are between two valid double values but have no corresponding double value of their own
-- Expected to round to one of the two double values that bound them
invalidDecimalRealAccuracy :: TestTree
invalidDecimalRealAccuracy = QC.testProperty "Non-accurate decimal (real-kind) literal (double rounding)" $
   QC.forAll genReal $ \(ExpectedOutput input (val1,val2)) -> parse abstractLiteral "TEST" input == Right val1 || parse abstractLiteral "TEST" input == Right val2
   where genReal :: QC.Gen (ParserExpectedOutput (AbstractLiteral,AbstractLiteral))
         genReal = do
            integerDifference <- QC.choose (2.0^52,2.0^53 - 1)
            exponentValue <- QC.choose (1,970) :: QC.Gen Int16
            showFunction <- QC.elements [showEFloat,showFFloatAlt]
            let makeValue = (*) (2.0 ^ exponentValue)
                showValue value = (showFunction Nothing value) ""
                value1 = makeValue integerDifference
                value2 = makeValue $ integerDifference + 1.0
                value1Str = showValue value1
                value2Str = showValue value2
                findDifference :: String -> String -> String -> String -> Int -> (String,String,Int,String,String)
                findDifference ('.':restA) ('.':restB) valueA valueB count = findDifference' restA restB valueA valueB count
                findDifference (fstA:restA) (fstB:restB) valueA valueB count = findDifference restA restB (fstA:valueA) (fstB:valueB) (count+1)
                findDifference' :: String -> String -> String -> String -> Int -> (String,String,Int,String,String)
                findDifference' ('e':restA) ('e':restB) valueA valueB count = (valueA,valueB,count,restA,restB)
                findDifference' ('e':restA) (fstB:restB) valueA valueB count = findDifference' ('e':restA) restB ('0':valueA) (fstB:valueB) count
                findDifference' (fstA:restA) ('e':restB) valueA valueB count = findDifference' restA ('e':restB) (fstA:valueA) ('0':valueB) count
                findDifference' (fstA:restA) (fstB:restB) valueA valueB count = findDifference' restA restB (fstA:valueA) (fstB:valueB) count
                findDifference' [] (fstB:restB) valueA valueB count = findDifference' [] restB ('0':valueA) (fstB:valueB) count
                findDifference' (fstA:restA) [] valueA valueB count = findDifference' restA [] (fstA:valueA) ('0':valueB) count
                findDifference' [] [] valueA valueB count = (valueA,valueB,count,"","")
                (revValue1,revValue2,shiftVal,expValue1,expValue2) = findDifference value1Str value2Str [] [] 0
                convertToInt = read . reverse :: String -> Integer
            preExpValue <- QC.choose (convertToInt revValue1,convertToInt revValue2)
            expValue <- case (expValue1,expValue2) of
               ([],[]) -> return Nothing
               (expValue1,expValue2) -> Just <$> QC.choose (read expValue1,read expValue2) :: QC.Gen (Maybe Integer)
            let makeValueStr (fst:rest) (Just 0) output = makeValueStr rest Nothing (fst:'.':output)
                makeValueStr (fst:rest) (Just val) output = makeValueStr rest (Just $ val-1) (fst:output)
                makeValueStr (fst:rest) Nothing output = makeValueStr rest Nothing (fst:output)
                makeValueStr [] Nothing output = reverse output
                preExpValueStr = makeValueStr (show preExpValue) (Just shiftVal) []
            exponentChar <- QC.elements "Ee"
            let input = case expValue of
                           Just val -> preExpValueStr ++ [exponentChar] ++ show val
                           Nothing -> preExpValueStr
            return $ ExpectedOutput input (UniversalReal value1,UniversalReal value2)

-- |Tests for decimal literals out of bounds
-- Abstract (decimal) literals that are formatted correctly but are out of bounds
invalidDecimalOutOfBounds :: TestTree
invalidDecimalOutOfBounds = QC.testProperty "Decimal literal out of bounds" $
   QC.forAll (QC.oneof [intOutOfBounds,realOutOfBounds]) $ isLeft . (parse abstractLiteral "TEST")
   where intOutOfBounds = QC.oneof [intOutOfBoundsPositiveExp,intOutOfBoundsNegativeExp,intOutOfBoundsNoExp]
         genPositive :: QC.Gen Integer
         genPositive = QC.suchThat QC.arbitrary (>=0)
         showValue (value,exponent,eChr) = show value ++ [eChr] ++ show exponent
         intOutOfBoundsPositiveExp =
            let value = (,,)
                        <$> genPositive
                        <*> genPositive
                        <*> QC.elements "Ee"
                checkValue (value,exponent,_) = (value * 10 ^ exponent) > toInteger (maxBound :: Int64)
            in showValue <$> QC.suchThat value checkValue
         intOutOfBoundsNegativeExp =
            let value = (,,)
                        <$> genPositive
                        <*> QC.suchThat QC.arbitrary (<0)
                        <*> QC.elements "Ee"
                checkValue (value,exponent,_) = let double = fromInteger value * 10.0 ^^ fromInteger exponent
                                                in double > fromIntegral (maxBound :: Int64) || double < 0.0
            in showValue <$> QC.suchThat value checkValue
         intOutOfBoundsNoExp = show <$> QC.suchThat QC.arbitrary (>toInteger (maxBound :: Int64))
         realOutOfBounds =
            let genReal :: QC.Gen (String,Char,String)
                genReal = (,,)
                          <$> ((\s -> s "") . (showFFloat Nothing) <$> (QC.arbitrary :: QC.Gen Double))
                          <*> QC.elements "Ee"
                          <*> (show <$> genPositive)
                showReal (val,exp,expVal) = val ++ [exp] ++ expVal
                checkReal (val,exp,expVal) = isInfinite $ (read (val ++ "e" ++ expVal) :: Double)
            in showReal <$> QC.suchThat genReal checkReal

-- |Tests for abstract literals with invalid formatting
invalidAbstractFormatting :: TestTree
invalidAbstractFormatting = QC.testProperty "Invalid abstract literal formatting" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ isLeft . (parse abstractLiteral "TEST")
   where checkInvalid = isNothing . (matchRegex $ mkRegexWithOpts regexString True True)
         regexString = "^([0-9](_?[0-9])*(\\.[0-9](_?[0-9])*)?([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|2#[01](_?[01])*(\\.[01](_?[01])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|2:[01](_?[01])*(\\.[01](_?[01])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|3#[0-2](_?[0-2])*(\\.[0-2](_?[0-2])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|3:[0-2](_?[0-2])*(\\.[0-2](_?[0-2])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|4#[0-3](_?[0-3])*(\\.[0-3](_?[0-3])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|4:[0-3](_?[0-3])*(\\.[0-3](_?[0-3])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|5#[0-4](_?[0-4])*(\\.[0-4](_?[0-4])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|5:[0-4](_?[0-4])*(\\.[0-4](_?[0-4])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|6#[0-5](_?[0-5])*(\\.[0-5](_?[0-5])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|6:[0-5](_?[0-5])*(\\.[0-5](_?[0-5])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|7#[0-6](_?[0-6])*(\\.[0-6](_?[0-6])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|7:[0-6](_?[0-6])*(\\.[0-6](_?[0-6])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|8#[0-7](_?[0-7])*(\\.[0-7](_?[0-7])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|8:[0-7](_?[0-7])*(\\.[0-7](_?[0-7])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|9#[0-8](_?[0-8])*(\\.[0-8](_?[0-8])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|9:[0-8](_?[0-8])*(\\.[0-8](_?[0-8])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|10#[0-9](_?[0-9])*(\\.[0-9](_?[0-9])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|10:[0-9](_?[0-9])*(\\.[0-9](_?[0-9])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|11#[0-9Aa](_?[0-9Aa])*(\\.[0-9Aa](_?[0-9Aa])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|11:[0-9Aa](_?[0-9Aa])*(\\.[0-9Aa](_?[0-9Aa])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|12#[0-9ABab](_?[0-9ABab])*(\\.[0-9ABab](_?[0-9ABab])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|12:[0-9ABab](_?[0-9ABab])*(\\.[0-9ABab](_?[0-9ABab])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|13#[0-9A-Ca-c](_?[0-9A-Ca-c])*(\\.[0-9A-Ca-c](_?[0-9A-Ca-c])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|13:[0-9A-Ca-c](_?[0-9A-Ca-c])*(\\.[0-9A-Ca-c](_?[0-9A-Ca-c])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|14#[0-9A-Da-d](_?[0-9A-Da-d])*(\\.[0-9A-Da-d](_?[0-9A-Da-d])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|14:[0-9A-Da-d](_?[0-9A-Da-d])*(\\.[0-9A-Da-d](_?[0-9A-Da-d])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|15#[0-9A-Ea-e](_?[0-9A-Ea-e])*(\\.[0-9A-Ea-e](_?[0-9A-Ea-e])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|15:[0-9A-Ea-e](_?[0-9A-Ea-e])*(\\.[0-9A-Ea-e](_?[0-9A-Ea-e])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|16#[0-9A-Fa-f](_?[0-9A-Fa-f])*(\\.[0-9A-Fa-f](_?[0-9A-Fa-f])*)?#([Ee][+-]?[0-9](_?[0-9])*)?\
                       \|16:[0-9A-Fa-f](_?[0-9A-Fa-f])*(\\.[0-9A-Fa-f](_?[0-9A-Fa-f])*)?:([Ee][+-]?[0-9](_?[0-9])*)?\
                       \)"

-- |Character literal tests
characters :: TestTree
characters = testGroup "Character literals"
   [ validCharacters
   , invalidCharacters
   ]

-- |Valid character literal tests
validCharacters :: TestTree
validCharacters = QC.testProperty "Valid character literals" $
   QC.forAll (QC.elements allGraphicCharacters) $ \char -> (parse characterLiteral "TEST" ['\'',char,'\'']) == Right char

-- |Invalid character literal tests
invalidCharacters :: TestTree
invalidCharacters = QC.testProperty "Invalid character literals" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ \input -> isLeft $ parse characterLiteral "TEST" input
   where checkInvalid ('\'':char:'\'':_) = not $ elem char allGraphicCharacters
         checkInvalid _ = True

-- |String literal tests
strings :: TestTree
strings = testGroup "String literals"
   [ validStrings
   , invalidStrings
   ]

-- |Valid string literal tests
validStrings :: TestTree
validStrings = QC.testProperty "Valid string literals" $
   QC.forAll genString $ \(ExpectedOutput input expectedOutput) -> (parse stringLiteral "TEST" input) == Right expectedOutput
   where genString :: QC.Gen (ParserExpectedOutput String)
         genString = do
            container <- QC.elements ['"','%']
            stringLength <- QC.choose (0,200)
            expectedOutput <- replicateM stringLength $ QC.elements allGraphicCharacters
            let sanitiseString chr
                  | chr == container = replicate 2 container
                  | otherwise = [chr]
                input = container : (concat $ map sanitiseString expectedOutput) ++ [container]
            return $ ExpectedOutput input expectedOutput

-- |Invalid string literal tests
invalidStrings :: TestTree
invalidStrings = QC.testProperty "Invalid string literals" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ \input -> isLeft $ parse stringLiteral "TEST" input
   where checkInvalid ('%':rest) = checkString '%' rest
         checkInvalid ('"':rest) = checkString '"' rest
         checkInvalid [] = False
         checkInvalid _ = True
         checkString container (chr:rest)
            | chr == container = False
            | elem chr allGraphicCharacters = checkString container rest
            | otherwise = True
         checkString _ [] = True

-- |Test for bit string literals
bitStrings :: TestTree
bitStrings = testGroup "Bit string literals"
   [ validBitStrings
   , invalidBitStrings
   ]

-- |Tests for valid bit string literals
validBitStrings :: TestTree
validBitStrings = QC.testProperty "Valid bit string literals" $
   QC.forAll genBitStr $ \(ExpectedOutput input expectedOutput) -> (parse bitStringLiteral "TEST" input) == Right expectedOutput

-- |Tests for invalid bit string literals
invalidBitStrings :: TestTree
invalidBitStrings = QC.testProperty "Invalid bit string literals" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ \input -> isLeft $ parse bitStringLiteral "TEST" input
   where checkInvalid = isNothing . (matchRegex $ mkRegexWithOpts regexString True True)
         regexString = "^([Bb][\"][01](_?[01])*[\"]|[Bb]%[01](_?[01])*%|[Oo][\"][0-7](_?[0-7])*[\"]|[Oo]%[0-7](_?[0-7])*%|[Xx][\"][0-9a-fA-F](_?[0-9a-fA-F])*[\"]|[Xx]%[0-9a-fA-F](_?[0-9a-fA-F])*%)"

-- |Tests for comments
comments :: TestTree
comments = testGroup "Comments"
   [ validComments
   , invalidComments
   ]

-- |Tests for valid comments
validComments :: TestTree
validComments = QC.testProperty "Valid comments" $
   QC.forAll genComment $ \(ExpectedOutput input expectedRemainder) -> parseIncludeRemainder input == Right expectedRemainder
   where parseIncludeRemainder = parse (comment *> manyTill anyChar eof) "TEST"

-- |Tests for invalid comments
invalidComments :: TestTree
invalidComments = QC.testProperty "Invalid comments" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ \input -> isLeft $ parse comment "TEST" input
   where checkInvalid ('-':'-':_) = False
         checkInvalid _ = True

-- |Tests for internal functions from "Parser.Combinators.Lex.Internal"
internals :: TestTree
internals = testGroup "Internal module tests"
   [ graphicCharacters
   , basicGraphicCharacters
   , basicCharacters
   , upperCaseLetters
   , specialCharacters
   , spaceCharacters
   , formatEffectors
   , lowerCaseLetters
   , otherSpecialCharacters
   , letterOrDigits
   , letters
   , integers
   , exponents
   ]

-- |Tests for graphical characters
-- See 'allGraphicCharacters'
graphicCharacters :: TestTree
graphicCharacters = charTest allGraphicCharacters "Graphic characters" graphicCharacter

-- |All valid graphic characters
-- Graphics characters include:
-- - 'allBasicGraphicCharacters'
-- - 'allLowerCase'
-- - 'allOtherSpecialCharacters'
allGraphicCharacters :: [Char]
allGraphicCharacters = allBasicGraphicCharacters ++ allLowerCase ++ allOtherSpecialCharacters

-- |Tests for basic graphical characters
-- See 'allBasicGraphicCharacters'
basicGraphicCharacters :: TestTree
basicGraphicCharacters = charTest allBasicGraphicCharacters "Basic graphic characters" basicGraphicCharacter

-- |All valid basic graphic characters
allBasicGraphicCharacters :: [Char]
allBasicGraphicCharacters = allUpperCase ++ allDigits ++ allSpecialCharacters ++ " "

-- |Tests for basic graphical characters
-- Basic characters include:
-- - 'allBasicGraphicCharacters'
-- - 'allFormatEffectors'
basicCharacters :: TestTree
basicCharacters = charTest (allBasicGraphicCharacters ++ allFormatEffectors) "Basic characters" basicCharacter

-- |Tests for upper case letters
-- See 'allUpperCase'
upperCaseLetters :: TestTree
upperCaseLetters = charTest allUpperCase "Upper case letters" upperCaseLetter

-- |All upper case characters
allUpperCase :: [Char]
allUpperCase = ['A'..'Z']

-- |Tests for special characters
-- See 'allSpecialCharacters'
specialCharacters :: TestTree
specialCharacters = charTest allSpecialCharacters "Special characters" specialCharacter

-- |All special characters
allSpecialCharacters :: [Char]
allSpecialCharacters =
   [ '"'
   , '#'
   , '&'
   , '\''
   , '('
   , ')'
   , '*'
   , '+'
   , ','
   , '-'
   , '.'
   , '/'
   , ':'
   , ';'
   , '<'
   , '='
   , '>'
   , '_'
   , '|'
   ]

-- |Tests for the space character
spaceCharacters :: TestTree
spaceCharacters = charTest " " "Space character" spaceCharacter

-- |Tests for format effectors
-- See 'allFormatEffectors'
formatEffectors :: TestTree
formatEffectors = charTest allFormatEffectors "Format effectors" formatEffector

-- |All format effectors
-- - Horizontal tabulation (\\t)
-- - Vertical tabulation (\\v)
-- - Carriage return (\\r)
-- - Line feed (\\n)
-- - Form feed (\\f)
allFormatEffectors :: [Char]
allFormatEffectors = ['\t','\v','\r','\n','\f']

-- |Tests for lower case letters
-- See 'allLowerCase'
lowerCaseLetters :: TestTree
lowerCaseLetters = charTest allLowerCase "Lower case letters" lowerCaseLetter

-- |All lower case characters
allLowerCase :: [Char]
allLowerCase = ['a'..'z']

-- |Tests for special characters
-- See 'allOtherSpecialCharacters'
otherSpecialCharacters :: TestTree
otherSpecialCharacters = charTest allOtherSpecialCharacters "Other special characters" otherSpecialCharacter

-- |All other special characters
allOtherSpecialCharacters :: [Char]
allOtherSpecialCharacters =
   [ '!'
   , '$'
   , '%'
   , '@'
   , '?'
   , '['
   , '\\'
   , ']'
   , '^'
   , '`'
   , '{'
   , '}'
   , '~'
   ]

-- |Tests for letters or digits
-- Either:
-- - 'allDigits'
-- - 'allLowerCase'
-- - 'allUpperCase'
letterOrDigits :: TestTree
letterOrDigits = charTest (allLowerCase++allUpperCase++allDigits) "Letters or digits" letterOrDigit

-- |All digit characters
allDigits :: [Char]
allDigits = ['0'..'9']

-- |Tests for letters
-- Either:
-- - 'allLowerCase'
-- - 'allUpperCase'
letters :: TestTree
letters = charTest (allLowerCase++allUpperCase) "Letters" letter

-- |Tests for integer
-- @
--    integer ::= digit { [ underline ] digit }
-- @
integers :: TestTree
integers = testGroup "Integers"
   [ validIntegers
   , invalidIntegers
   ]

-- |Tests for valid integers
validIntegers :: TestTree
validIntegers = QC.testProperty "Valid integers" $
   QC.forAll (genInteger Nothing Nothing) $
      \(ExpectedOutput input expectedValue) -> parse integer "TEST" input == Right (show expectedValue)

-- |Tests for invalid integers
invalidIntegers :: TestTree
invalidIntegers = QC.testProperty "Invalid integers" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ isLeft . (parse integer "TEST")
   where checkInvalid = isNothing . (matchRegex $ mkRegexWithOpts "^[0-9](_?[0-9])*" True True)

-- |Tests for exponents
-- @
--    exponent ::= E [ + ] integer | E [ - ] integer
-- @
exponents :: TestTree
exponents = testGroup "Exponents"
   [ validExponents
   , invalidExponents
   ]

-- |Tests for valid exponents
validExponents :: TestTree
validExponents = QC.testProperty "Valid exponents" $
   QC.forAll genExponent $ \(ExpectedOutput input expectedOutput) -> parse exponent' "TEST" input == Right (toInteger expectedOutput)

-- |Tests for invalid exponents
invalidExponents :: TestTree
invalidExponents = QC.testProperty "Invalid exponents" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ isLeft . (parse exponent' "TEST")
   where checkInvalid = isNothing . (matchRegex $ mkRegexWithOpts "^[Ee][+-]?[0-9](_?[0-9])*" True True)

-- |Tester for character parsers
charTest :: [Char] -> String -> Parser Char -> TestTree
charTest validChars testDesc testParse = QC.testProperty testDesc $
   QC.forAll QC.arbitrary $ \input ->
      let output = parse testParse "TEST" [input]
      in if elem input validChars
            then output == Right input
            else isLeft output
