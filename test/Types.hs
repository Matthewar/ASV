{-|
   Module      : Types
   Description : Types for testing
|-}
module Types
   ( ExpectedOutput(..)
   ) where

data (Show a) => ExpectedOutput a b =
   ExpectedOutput
      { input :: a
      , expectedOutput :: b
      }

instance Show a => Show (ExpectedOutput a b) where
   show (ExpectedOutput input _) = show input
