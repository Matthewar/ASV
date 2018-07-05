{-|
   Module      : Spec.Parser.Combinators.ReservedWords
   Description : Tests for reserved word combinators
|-}
module Spec.Parser.Combinators.ReservedWords (tests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import Control.Monad (replicateM)
import Data.Char
         ( toUpper
         , toLower
         )
import Data.Either (isLeft)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import Parser.Combinators.ReservedWords

import Types (ExpectedOutput(..))

-- |All tests for the module "Parser.Combinators.ReservedWords"
tests :: TestTree
tests = testGroup "Reserved words"
   [ validReservedWords
   , invalidReservedWords
   ]

-- |Tests for all reserved words
reservedWordsTest :: (String -> Parser () -> TestTree) -> String -> TestTree
reservedWordsTest testWord testDesc = testGroup testDesc
   [ testWord "abs" absKeyword
   , testWord "access" accessKeyword
   , testWord "after" afterKeyword
   , testWord "alias" aliasKeyword
   , testWord "all" allKeyword
   , testWord "and" andKeyword
   , testWord "architecture" architectureKeyword
   , testWord "array" arrayKeyword
   , testWord "assert" assertKeyword
   , testWord "attribute" attributeKeyword
   , testWord "begin" beginKeyword
   , testWord "block" blockKeyword
   , testWord "body" bodyKeyword
   , testWord "buffer" bufferKeyword
   , testWord "bus" busKeyword
   , testWord "case" caseKeyword
   , testWord "component" componentKeyword
   , testWord "configuration" configurationKeyword
   , testWord "constant" constantKeyword
   , testWord "disconnect" disconnectKeyword
   , testWord "downto" downtoKeyword
   , testWord "else" elseKeyword
   , testWord "elsif" elsifKeyword
   , testWord "end" endKeyword
   , testWord "entity" entityKeyword
   , testWord "exit" exitKeyword
   , testWord "file" fileKeyword
   , testWord "for" forKeyword
   , testWord "function" functionKeyword
   , testWord "generate" generateKeyword
   , testWord "generic" genericKeyword
   , testWord "guarded" guardedKeyword
   , testWord "if" ifKeyword
   , testWord "in" inKeyword
   , testWord "inout" inoutKeyword
   , testWord "is" isKeyword
   , testWord "label" labelKeyword
   , testWord "library" libraryKeyword
   , testWord "linkage" linkageKeyword
   , testWord "loop" loopKeyword
   , testWord "map" mapKeyword
   , testWord "mod" modKeyword
   , testWord "nand" nandKeyword
   , testWord "new" newKeyword
   , testWord "next" nextKeyword
   , testWord "nor" norKeyword
   , testWord "not" notKeyword
   , testWord "null" nullKeyword
   , testWord "of" ofKeyword
   , testWord "on" onKeyword
   , testWord "open" openKeyword
   , testWord "or" orKeyword
   , testWord "others" othersKeyword
   , testWord "out" outKeyword
   , testWord "package" packageKeyword
   , testWord "port" portKeyword
   , testWord "procedure" procedureKeyword
   , testWord "process" processKeyword
   , testWord "range" rangeKeyword
   , testWord "record" recordKeyword
   , testWord "register" registerKeyword
   , testWord "rem" remKeyword
   , testWord "report" reportKeyword
   , testWord "return" returnKeyword
   , testWord "select" selectKeyword
   , testWord "severity" severityKeyword
   , testWord "signal" signalKeyword
   , testWord "subtype" subtypeKeyword
   , testWord "then" thenKeyword
   , testWord "to" toKeyword
   , testWord "transport" transportKeyword
   , testWord "type" typeKeyword
   , testWord "units" unitsKeyword
   , testWord "until" untilKeyword
   , testWord "use" useKeyword
   , testWord "variable" variableKeyword
   , testWord "wait" waitKeyword
   , testWord "when" whenKeyword
   , testWord "while" whileKeyword
   , testWord "with" withKeyword
   , testWord "xor" xorKeyword
   ]

-- |Tests for all valid reserved words
validReservedWords :: TestTree
validReservedWords = reservedWordsTest validReservedWord "Valid reserved words"

-- |Tests for incorrect (invalid) reserved words
invalidReservedWords :: TestTree
invalidReservedWords = reservedWordsTest invalidReservedWord "Invalid reserved words"

-- |Valid keyword test
-- Generates valid (case insensitive) keyword and checks that the parser succeeded.
validReservedWord :: String -> Parser () -> TestTree
validReservedWord keyword parser = QC.testProperty ("Valid reserved word " ++ keyword) $
   QC.forAll (genKeyword keyword) $ \input -> (parse parser "TEST" input) == Right ()

-- |Invalid keyword test
-- Generates invalid (case insensitive) word (that doesn't match keyword) and checks that the parser failed.
invalidReservedWord :: String -> Parser () -> TestTree
invalidReservedWord keyword parser = QC.testProperty ("Invalid reserved word " ++ keyword) $
   QC.forAll (genNonKeyword keyword) $ \input -> isLeft $ parse parser "TEST" input

-- |Generate valid keyword
-- When passed valid keyword as 'String' input, generates randomised case form of it.
-- E.g. Passed "and", returns 'QC.Gen' "ANd" or 'QC.Gen' "aNd", etc.
genKeyword :: String -> QC.Gen String
genKeyword = genCaseInsensitiveWord

-- |Generates anything but keyword
-- When passed a valid keyword as a lower case 'String' input, generates randomised string that doesn't parse as this keyword.
genNonKeyword :: String -> QC.Gen String
genNonKeyword keyword = QC.suchThat (genCaseInsensitiveWord =<< QC.arbitrary) $ \word -> (map toLower word) /= keyword

-- |Generates a case insensitive word
-- When passed a 'String' word, generates randomised case form of it.
genCaseInsensitiveWord :: String -> QC.Gen String
genCaseInsensitiveWord word = do
   let genBool = QC.elements [True,False]
   wordBools <- replicateM (length word) genBool
   let lexInput = map chooseCase $ zip wordBools word
   return lexInput
   where chooseCase (True,chr) = toUpper chr
         chooseCase (False,chr) = toLower chr
