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
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Regex
         ( matchRegex
         , mkRegexWithOpts
         )

import Parser.Combinators.Lex
import Parser.Combinators.Lex.Internal
import Parser.Types.Token (mkUpperString)

import Types
         ( ExpectedOutput(..)
         , ParserExpectedOutput
         )
import Spec.Generators.LexElements (genIdentifier)

-- |All tests for the module "Parser.Combinators.Lex"
-- Includes tests for "Parser.Combinators.Lex.Internal"
tests :: TestTree
tests = testGroup "Lexical element combinator tests"
   [ identifiers
   --, abstractLiterals
   , characters
   --, strings
   --, bitStrings
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

-- |Character literal tests
characters :: TestTree
characters = testGroup "Character literals" $
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
                  | otherwise = replicate 2 chr
                input = container : (concat $ map sanitiseString expectedOutput) ++ [container]
            return $ ExpectedOutput input expectedOutput

-- |Invalid string literal tests
invalidStrings :: TestTree
invalidStrings = QC.testProperty "Invalid string literals" $
   QC.forAll (QC.suchThat QC.arbitrary checkInvalid) $ \input -> isLeft $ parse stringLiteral "TEST" input
   where checkInvalid ('%':rest) = checkString '%' rest
         checkInvalid ('"':rest) = checkString '"' rest
         checkInvalid [] = False
         checkString container (chr:rest)
            | chr == container = False
            | elem chr allGraphicCharacters = checkString container rest
            | otherwise = True
         checkString _ [] = True

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
allBasicGraphicCharacters = allUpperCase ++ ['0'..'9'] ++ allSpecialCharacters ++ " "

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

-- |Tester for character parsers
charTest :: [Char] -> String -> Parser Char -> TestTree
charTest validChars testDesc testParse = QC.testProperty testDesc $
   QC.forAll QC.arbitrary $ \input ->
      let output = parse testParse "TEST" [input]
      in if elem input validChars
            then output == Right input
            else isLeft output
