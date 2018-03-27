{-|
   Module      : Generators.ParseElements
   Description : Generators of parser elements of VHDL
-}
module Generators.ParseElements 
   ( genEmptyEntity
   ) where

import qualified Test.Tasty.QuickCheck as QC
import Control.Monad (replicateM)

-- |Generate an empty VHDL entity declaration
-- Pass name, whether optional parts are included
genEmptyEntity :: String -> Bool -> Bool -> QC.Gen String
genEmptyEntity entityName statementPart secondLabel = do
   let space = do
         numSpaces <- QC.elements [1..10]
         replicateM numSpaces $ QC.elements "\n\t "
   space1 <- space
   space2 <- space
   space3 <- space
   space4 <- space
   let part1 = "entity" ++ space1 ++ entityName ++ space2 ++ "is" ++ space3
       part3 = "end" ++ space4
       part5 = ";"
   part2 <- if statementPart then do
               space5 <- space
               return $ "begin" ++ space5
            else return ""
   part4 <- if secondLabel then do
               space6 <- space
               return $ entityName ++ space6
            else return ""
   return $ part1 ++ part2 ++ part3 ++ part4 ++ part5
