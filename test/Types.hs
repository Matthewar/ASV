{-|
   Module      : Types
   Description : Types for testing
|-}
module Types
   ( ExpectedOutput(..)
   , ParserExpectedOutput
   ) where

data (Show a) => ExpectedOutput a b =
   ExpectedOutput
      { input :: a
      , expectedOutput :: b
      }

instance (Show a,Show b) => Show (ExpectedOutput a b) where
   show (ExpectedOutput input expectedOutput) =
      "Input: \""
      ++ show input
      ++ "\"; Output: \""
      ++ show expectedOutput
      ++ "\""

-- |Parser expected output type
-- Input will always be 'String'
type ParserExpectedOutput a = ExpectedOutput String a
