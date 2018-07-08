{-|
   Module      : Spec.Parser.Combinators.Lex
   Description : Tests for lexical element combinators
|-}
module Spec.Parser.Combinators.Lex (tests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import Control.Monad (replicateM)
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Data.Int
         ( Int8
         , Int16
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
         , genBitStr
         , genIdentifier
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
   ]

-- |Tests for decimal literals
decimalLiterals :: TestTree
decimalLiterals = testGroup "Decimal literals"
   [ validDecimals
   --, invalidDecimals
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
   QC.forAll genInteger $ \(ExpectedOutput input expectedOutput) -> (parse abstractLiteral "TEST" input) == Right (UniversalInteger expectedOutput)
   where genInteger :: QC.Gen (ParserExpectedOutput Int64)
         genInteger = QC.oneof
                        [ noExponent
                        , positiveExponent
                        , negativeExponent
                        ]
         genInt :: (QC.Arbitrary a,Integral a) => QC.Gen a
         genInt = QC.suchThat QC.arbitrary (>= 0)
         noExponent :: QC.Gen (ParserExpectedOutput Int64)
         noExponent =
            let gen value = ExpectedOutput (show value) value
            in gen <$> genInt
         positiveExponent :: QC.Gen (ParserExpectedOutput Int64)
         positiveExponent = do
            let genMulTen :: QC.Gen (Integer,Int8)
                genMulTen = (\baseVal exponent -> (toInteger baseVal * 10 ^ exponent,exponent))
                            <$> (genInt :: QC.Gen Int16)
                            <*> genInt
            (expectedOutput,exponent) <- (\(a,b) -> (fromInteger a,b))
                                         <$> QC.suchThat genMulTen ((\val -> val >= 0 && val <= toInteger (maxBound :: Int64)) . fst)
            actualExponent <- QC.choose (0,exponent)
            exponentChar <- QC.elements "Ee"
            let input = show (floor $ fromIntegral expectedOutput / 10.0 ^^ actualExponent) ++ [exponentChar] ++ show actualExponent
            return $ ExpectedOutput input expectedOutput
         negativeExponent :: QC.Gen (ParserExpectedOutput Int64)
         negativeExponent = do
            expectedOutput <- genInt
            shiftVal <- genInt
            exponentChar <- QC.elements "Ee"
            let input = show expectedOutput ++ replicate shiftVal '0' ++ [exponentChar] ++ show (-shiftVal)
            return $ ExpectedOutput input expectedOutput

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
            stringLength <- QC.choose (1,200)
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
         genComment :: QC.Gen (ParserExpectedOutput String)
         genComment = do
            randomString <- QC.arbitrary
            let splitStringOnNewline = span (`notElem` "\r\n") randomString
                (input,expectedOutput) =
                  case splitStringOnNewline of
                     (fst,snd@('\r':'\n':rest)) -> (fst++snd,rest)
                     (fst,snd@('\r':rest)) -> (fst++"\r\n"++rest,rest)
                     (fst,snd@('\n':rest)) -> (fst++snd,rest)
                     (fst,[]) -> (fst,[])
            return $ ExpectedOutput ("--" ++ input) expectedOutput

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
   --, exponents
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
   QC.forAll (genInteger 0 200) $ \(ExpectedOutput input expectedOutput) -> parse integer "TEST" input == Right expectedOutput

-- |Tests for invalid integers
invalidIntegers :: TestTree
invalidIntegers = QC.testProperty "Invalid integers" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ isLeft . (parse integer "TEST")
   where checkInvalid = isNothing . (matchRegex $ mkRegexWithOpts "^[0-9](_?[0-9])*" True True)

-- |Tester for character parsers
charTest :: [Char] -> String -> Parser Char -> TestTree
charTest validChars testDesc testParse = QC.testProperty testDesc $
   QC.forAll QC.arbitrary $ \input ->
      let output = parse testParse "TEST" [input]
      in if elem input validChars
            then output == Right input
            else isLeft output
