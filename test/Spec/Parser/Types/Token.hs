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
import Data.Char
         ( toLower
         , toUpper
         )
import Data.Functor ((<&>))
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
   , abstractLiteralTests
   , bitStringTests
   , internalTypes
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

-- |Tests for the derived classes for 'AbstractLiteral'
-- Both classes 'Eq' and 'Show' are derived for this type.
abstractLiteralTests :: TestTree
abstractLiteralTests = testGroup "Abstract literal class tests"
   [ abstractLiteralEqTests
   , abstractLiteralShowTests
   ]

-- |Tests for the derived 'Eq' type class for 'AbstractLiteral' type
abstractLiteralEqTests :: TestTree
abstractLiteralEqTests = testGroup "Eq type class tests"
   [ QC.testProperty "(==) test" $ baseTest (==) [genSameInteger,genSameReal]
   , QC.testProperty "(/=) test" $ baseTest (/=) [genPairInteger,genPairReal,genPairDifferent]
   ]
   where baseTest :: (AbstractLiteral -> AbstractLiteral -> Bool) -> [QC.Gen (AbstractLiteral,AbstractLiteral)] -> QC.Property
         baseTest compare genList = QC.forAll (QC.oneof genList) $ \(value1,value2) -> value1 `compare` value2
         genSameInteger = QC.arbitrary <&> \a -> (UniversalInteger a,UniversalInteger a)
         genSameReal = QC.arbitrary <&> \a -> (UniversalReal a,UniversalReal a)
         genPairValues :: (QC.Arbitrary a,Eq a,Num a) => QC.Gen (a,a)
         genPairValues = QC.suchThat ((,) <$> QC.arbitrary <*> QC.arbitrary) $ \(a,b) -> a /= b
         genPairInteger = genPairValues <&> \(a,b) -> (UniversalInteger a,UniversalInteger b)
         genPairReal = genPairValues <&> \(a,b) -> (UniversalReal b,UniversalReal a)
         genPairDifferent = do
            int <- QC.arbitrary
            real <- QC.arbitrary
            QC.elements [(UniversalInteger int,UniversalReal real),(UniversalReal real,UniversalInteger int)]

-- |Tests for the derived 'Show' type class for 'AbstractLiteral' type
abstractLiteralShowTests :: TestTree
abstractLiteralShowTests = QC.testProperty "Show type class test" $
   QC.forAll genShowLiteral $ \(ExpectedOutput input expectedOutput) -> show input == expectedOutput
   where genShowLiteral = convertShow <$> genLiteral
         genLiteral = QC.oneof [ UniversalInteger <$> QC.arbitrary
                               , UniversalReal <$> QC.arbitrary
                               ]
         convertShow lit@(UniversalInteger val) = ExpectedOutput lit $ "UniversalInteger " ++ showValue val
         convertShow lit@(UniversalReal val) = ExpectedOutput lit $ "UniversalReal " ++ showValue val
         showValue val
            | val < 0 = "(" ++ show val ++ ")"
            | otherwise = show val

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
                checkInvalid [] = False
                checkInvalid _ = True
            str <- QC.suchThat QC.arbitrary (\s -> not (null s) && checkInvalid s)
            let findInvalid ('1':rest) = findInvalid rest
                findInvalid ('0':rest) = findInvalid rest
                findInvalid (chr:_) = chr
            return $ ExpectedOutput str $ "Invalid character in bit string '" ++ [findInvalid str] ++ "'"
         checkError :: String -> ErrorCall -> Bool
         checkError expected actual = isPrefixOf expected $ show actual

-- |Tests for internal token types
-- "Parser.Types.Token.Internal"
internalTypes :: TestTree
internalTypes = testGroup "Internal types"
   [ upperStringInternalTests
   --, bitStringTests
   ]

-- |Tests for 'UpperString' type classes
-- Derived Eq class and explicit Show class
upperStringInternalTests :: TestTree
upperStringInternalTests = testGroup "UpperString type class tests"
   [ upperStringEqTests
   --, upperStringShowTests
   ]

-- |Tests for the derived 'Eq' type class for 'UpperString' type
upperStringEqTests :: TestTree
upperStringEqTests = testGroup "Eq type class tests"
   [ QC.testProperty "(==) test" $ baseTest (==) genSameStrings
   , QC.testProperty "(/=) test" $ baseTest (/=) genDiffStrings
   ]
   where baseTest compare genStrings = QC.forAll genStrings $ \(a,b) -> mkUpperString a `compare` mkUpperString b
         genSameStrings = do
            rawString <- QC.arbitrary
            let lowerUpper chr = QC.elements [toUpper,toLower] <*> return chr
                caseString = mapM lowerUpper rawString
            (,) <$> caseString <*> caseString
         genDiffStrings = QC.oneof [genCompletelyDiffStrings,genSlightlyDiffStrings]
         genCompletelyDiffStrings = let rawString = QC.arbitrary
                                        dualStrings = (,) <$> rawString <*> rawString
                                    in QC.suchThat dualStrings $ \(a,b) -> B.pack a /= B.pack b
         genSlightlyDiffStrings = do
            rawString <- QC.suchThat QC.arbitrary (not . null)
            index <- QC.choose (0,length rawString - 1)
            let change :: String -> Int -> String -> String -> QC.Gen (String,String)
                change (fst:rest) index newA newB
                  | index == 0 = QC.suchThat QC.arbitrary (\fstB -> B.singleton fstB /= B.singleton fst) >>= \fstB -> change rest (index-1) (fst:newA) (fstB:newB)
                  | otherwise = change rest (index-1) (fst:newA) (fst:newB)
                change [] index newA newB = return (newA,newB)
            change rawString index [] []
