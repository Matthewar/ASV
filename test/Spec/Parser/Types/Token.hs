{-|
   Module      : Spec.Parser.Types.Token
   Description : Tests for base tokenising types

   Tests of both "Parser.Types.Token" using "Parser.Types.Token.Internal"
|-}
module Spec.Parser.Types.Token (tests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import Control.Exception
         ( catch
         , evaluate
         , ErrorCall
         )
import Control.Monad (replicateM)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toUpper)
import Data.List (isPrefixOf)

import Parser.Types.Token
import Parser.Types.Token.Internal

import Types
         ( ExpectedOutput(..)
         , ParserExpectedOutput
         )

-- |Tests for module "Parser.Types.Token"
-- Uses internal components in "Parser.Types.Token.Internal"
tests :: TestTree
tests = testGroup "Parser token types constructors"
   [ upperStringTests
   , bitStringTests
   ]

-- |Tests for constructor function of 'UpperString'
-- Uses constructor function 'mkUpperString'
upperStringTests :: TestTree
upperStringTests = testGroup "Upper case string constructor"
   [ validUpperStrings
   --, invalidUpperStrings
   ]

-- |Valid upper case string tests
validUpperStrings :: TestTree
validUpperStrings = QC.testProperty "Valid upper case strings" $
   QC.forAll genUpperString $ \(ExpectedOutput input expectedOutput) -> mkUpperString input == expectedOutput
   where genUpperString :: QC.Gen (ParserExpectedOutput UpperString)
         genUpperString = do
            stringLength <- QC.elements [1..200]
            input <- replicateM stringLength QC.arbitrary
            let expectedOutput = UpperString $ B.pack $ map toUpper $ input
            return $ ExpectedOutput input expectedOutput

-- |Tests for constructor function of 'BitString'
-- Uses constructor function 'mkBitString'
bitStringTests :: TestTree
bitStringTests = testGroup "Bit string constructor"
   [ validBitStrings
   , invalidBitStrings
   ]

-- |Valid bit string tests
validBitStrings :: TestTree
validBitStrings = QC.testProperty "Valid bit strings" $
   QC.forAll genBitString $ \(ExpectedOutput input expectedOutput) -> (mkBitString input) == expectedOutput
   where genBitString :: QC.Gen (ParserExpectedOutput BitString)
         genBitString = do
            stringLength <- QC.elements [1..200]
            let binaryChar = QC.elements "01"
            input <- replicateM stringLength binaryChar
            let expectedOutput = BitString $ B.pack input
            return $ ExpectedOutput input expectedOutput

-- |Invalid bit string tests
invalidBitStrings :: TestTree
invalidBitStrings = QC.testProperty "Invalid bit strings" $
   QC.forAll genInvalid $ \(ExpectedOutput input expectedOutput) -> QC.ioProperty $
      catch ((evaluate $ mkBitString input) *> return False) (return . checkError expectedOutput)
   where genInvalid = do
            let checkInvalid ('1':rest) = checkInvalid rest
                checkInvalid ('0':rest) = checkInvalid rest
                checkInvalid _ = True
            str <- QC.suchThat QC.arbitrary (\s -> not (null s) && checkInvalid s)
            let findInvalid ('1':rest) = findInvalid rest
                findInvalid ('0':rest) = findInvalid rest
                findInvalid (chr:_) = chr
            return $ ExpectedOutput str $ "Invalid character in bit string '" ++ [findInvalid str] ++ "'"
         checkError :: String -> ErrorCall -> Bool
         checkError expected actual = isPrefixOf expected $ show actual
