module Lexer.Sentences
   ( sentences
   ) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Control.Monad (replicateM)

import Lexer.Types.Token (Token(..))

import Lexer.Functions
         ( getLexResult
         , stringToKeyword
         , stringToDelimiter
         )
import Generators.LexElements
         ( genIdentifier
         , genKeyword
         , genDelimiter
         )

-- |Sentence tests
-- Combination of identifiers, keywords and identifiers
sentences :: TestTree
sentences = testGroup "Sentence tests for lexer"
   [ singleItemSentences
   --, multiItemSentences
   ]

-- |Sentences made of a single type of lexical element
-- Consists of a string that will lex to a list of tokens of the same type
singleItemSentences :: TestTree
singleItemSentences = testGroup "Sentences made of a single type of lexical element"
   [ keywordSentences
   --, literalSetences
   , identifierSentences
   ]

newtype Sentence = Sentence [(String,String)]
instance Show Sentence where
   show (Sentence strs) = concat $ map (\(a,b) -> a ++ b) strs

-- |Sentences made of only keywords
keywordSentences :: TestTree
keywordSentences = QC.testProperty "Sentences made of only keywords" $
   QC.forAll genSentence $ \(Sentence sentence) -> QC.ioProperty $ do
      lexRun <- getLexResult $ show $ Sentence sentence
      let (keywordStrs,delimiterStrs) = unzip sentence
          keywords = map (Keyword . stringToKeyword) keywordStrs
          joinOutput (keyword,delimiterStr) =
            (keyword:case stringToDelimiter delimiterStr of
               Nothing -> []
               Just op -> [Operator op]
            )
          expectedOutput = Right $ concat $ map joinOutput $ zip keywords delimiterStrs
      return $ lexRun == expectedOutput
   where genSentence :: QC.Gen Sentence
         genSentence = do
            length <- QC.choose (1,100)
            keywords <- replicateM length genKeyword
            delimiters <- replicateM length genDelimiter
            return $ Sentence $ zip keywords delimiters

--literalSetences :: TestTree
--literalSetences = QC.testProperty "Sentences made of only literals" $
--   QC.forAll genSentence $ \(Sentence sentence) -> QC.ioProperty $ do
--      lexRun <- getLexResult $ show $ Sentence sentence
--      let (literalStrs,

identifierSentences :: TestTree
identifierSentences = QC.testProperty "Sentences made of only identifiers" $
   QC.forAll genSentence $ \(Sentence sentence) -> QC.ioProperty $ do
      lexRun <- getLexResult $ show $ Sentence sentence
      let (identifierStrs,delimiterStrs) = unzip sentence
          identifiers = map Identifier identifierStrs
          joinOutput (identifier,delimiterStr) =
            (identifier:case stringToDelimiter delimiterStr of
               Nothing -> []
               Just op -> [Operator op]
            )
          expectedOutput = Right $ concat $ map joinOutput $ zip identifiers delimiterStrs
      return $ lexRun == expectedOutput
   where genSentence :: QC.Gen Sentence
         genSentence = do
            length <- QC.choose (1,100)
            identifiers <- replicateM length $ genIdentifier 1 10
            delimiters <- replicateM length genDelimiter
            return $ Sentence $ zip identifiers delimiters

-- |Sentences made of different lexical elements types
-- Consists of a string that will lex to a list of tokens of different types
--multiItemSentences :: TestTree
--multiItemSentences = testGroup "Sentences made of different types of lexical elements"
--   [ keywordsAndliteralSentences
--   , keywordsAndIdentifierSentences
--   , literalAndIdentifierSentences
--   , allItemsSentences
--   ]
