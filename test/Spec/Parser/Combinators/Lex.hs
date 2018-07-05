{-|
   Module      : Spec.Parser.Combinators.Lex
   Description : Tests for lexical element combinators
|-}
module Spec.Parser.Combinators.Lex (tests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

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

import Types (ExpectedOutput(..))
import Spec.Generators.LexElements (genIdentifier)

-- |All tests for the module "Parser.Combinators.Lex"
-- Includes tests for "Parser.Combinators.Lex.Internal"
tests :: TestTree
tests = testGroup "Lexical element combinator tests"
   [ identifiers
   --, abstractLiterals
   --, characters
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
-- Graphics characters include:
-- - 'allBasicGraphicCharacters'
-- - 'allLowerCase'
-- - 'allOtherSpecialCharacters'
graphicCharacters :: TestTree
graphicCharacters = charTest (allBasicGraphicCharacters ++ allLowerCase ++ allOtherSpecialCharacters) "Graphic characters" graphicCharacter

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