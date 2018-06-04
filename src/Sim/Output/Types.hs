module Sim.Output.Types
   ( outputTypes
   ) where

-- ?? Probably add instances for conversions

import qualified Data.Map.Strict as MapS
import Data.List (intersperse)
import Data.Int (Int64)
import Data.Function ((&))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )

import Parser.Netlist.Types.Representation
         ( Type(..)
         , NetlistName(..)
         , Enumerate(..)
         , IntegerRange
         , FloatRange
         , Subtype(..)
         )
import Parser.Netlist.Types.Stores (TypeStore)
import Sim.Output.Names (showEnum)
import Manager.Types.Error (ConverterError)

newline = "\n"
tab = "   "

outputTypes :: FilePath -> NetlistName -> TypeStore -> ExceptT ConverterError IO ()
outputTypes file unitName = (outputTypes' file unitName) . MapS.toList

outputTypes' :: FilePath -> NetlistName -> [(String,Type)] -> ExceptT ConverterError IO ()
outputTypes' fileName unitName ((typeName,typeData):others) = do
   case typeData of
      EnumerationType enums -> printEnumType fileName unitName typeName enums
      IntegerType -> printIntType fileName unitName typeName
      FloatingType -> printFloatType fileName unitName typeName
      PhysicalType baseUnit _ -> printPhysicalType fileName unitName typeName baseUnit -- ?? Use base/secondary units for printing output data
      ArrayType bounds subtypeName _ -> printArrayType fileName unitName typeName bounds subtypeName
   outputTypes' fileName unitName others
outputTypes' _ _ [] = return ()

printEnumType :: FilePath -> NetlistName -> String -> [Enumerate] -> ExceptT ConverterError IO ()
printEnumType file unitName name enums =
   let typeName = "Type'" ++ name
       mkFullTypeName (NetlistName lib unit) = lib ++ "'" ++ unit ++ "'" ++ typeName
       fullTypeName = mkFullTypeName unitName
       convertEnum = showEnum typeName
       typeBaseFormatting = concat $ intersperse (newline ++ tab ++ "| ") $ map convertEnum enums
       typeBaseStr =
         "data " ++ typeName ++ " ="
         ++ newline ++ tab
         ++ typeBaseFormatting
         ++ newline ++ tab
         ++ "deriving (Eq,Ord)"
       outputStr =
         let printEnum (Enum_Identifier iden) = "\"" ++ iden ++ "\""
             printEnum (Enum_Char '"') = "\"'\\\"'\""
             printEnum (Enum_Char '\\') = "\"'\\'\""
             printEnum (Enum_Char chr) = "\"'" ++ [chr] ++ "'\""
         in "instance STD.STANDARD.SignalOutput " ++ typeName ++ " where"
            ++ ( concat
               $ map (\enum -> newline ++ tab ++ "sigOut " ++ convertEnum enum ++ " = " ++ printEnum enum)
               $ enums
               )
       createComparisonFunc operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'_'" ++ fullTypeName ++ "'out'STD'STANDARD'Type'ANON'BOOLEAN"
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName ++ " -> " ++ "STD.STANDARD.Type'ANON'BOOLEAN"
            ++ newline
            ++ funcName ++ " value1 value2 = STD.STANDARD.boolToBoolean $ value1 " ++ operatorStr ++ " value2"
       equalStr = createComparisonFunc "EQUAL" "=="
       nequalStr = createComparisonFunc "NEQUAL" "/="
       lessThanStr = createComparisonFunc "LESSTHAN" "<"
       lessThanOrEqualStr = createComparisonFunc "LESSTHANOREQUAL" "<="
       greaterThanStr = createComparisonFunc "GREATERTHAN" ">"
       greaterThanOrEqualStr = createComparisonFunc "GREATERTHANOREQUAL" ">="
--       typeScalar =
--         "instance (VHDL_Scalar "
--         ++ name
--         ++ ") where"
--         ++ newline ++ tab
--         ++ "scalarLeft = "
--         ++ (convertEnum $ head enums)
--         ++ newline ++ tab
--         ++ "scalarRight = "
--         ++ (convertEnum $ last enums)
--         ++ newline ++ tab
--         ++ "scalarHigh = scalarRight"
--         ++ newline ++ tab
--         ++ "scalarLow = scalarLeft"
--       (discretePosStr,discreteValStr) = makeDiscretePosVal 0 enums [] []
--       (discreteLeftFormatting,discreteRightFormatting) = makeDiscreteLeftRight enums [] []
--       discreteSuccStr = concat $ map (\s -> newline ++ tab ++ "discreteSucc " ++ s) discreteRightFormatting
--       discretePredStr = concat $ map (\s -> newline ++ tab ++ "discretePred " ++ s) discreteLeftFormatting
--       discreteLeftOfStr = concat $ map (\s -> newline ++ tab ++ "discreteLeftOf " ++ s) discreteLeftFormatting
--       discreteRightOfStr = concat $ map (\s -> newline ++ tab ++ "discreteRightOf " ++ s) discreteRightFormatting
--       typeDiscrete =
--         "instance (VHDL_Discrete "
--         ++ name
--         ++ ") where"
--         ++ discretePosStr
--         ++ discreteValStr
--         ++ discreteSuccStr
--         ++ discretePredStr
--         ++ discreteLeftOfStr
--         ++ discreteRightOfStr
   in liftIO $ appendFile file $
         newline ++ typeBaseStr
         ++ newline ++ outputStr
         ++ newline ++ equalStr
         ++ newline ++ nequalStr
         ++ newline ++ lessThanStr
         ++ newline ++ lessThanOrEqualStr
         ++ newline ++ greaterThanStr
         ++ newline ++ greaterThanOrEqualStr
         ++ newline
         -- ++ newline ++ typeScalar ++ newline ++ typeDiscrete
--   where makeDiscretePosVal _ [] posStrLst valStrLst = (concat posStrLst,concat valStrLst)
--         makeDiscretePosVal val (enum:enums) posStrLst valStrLst =
--            let convEnum = convertEnum enum
--                posStr = newline ++ tab ++ "discretePos " ++ convEnum ++ " = " ++ val
--                valStr = newline ++ tab ++ "discreteVal " ++ val ++ " = " ++ convEnum
--            in makeDiscretePosVal (val + 1) enums (posStr:posStrLst) (valStr:valStrLst)
--         makeDiscreteLeftRight (enum1:enum2:enums) leftStr rightStr =
--            let convEnum1 = convertEnum enum1
--                convEnum2 = convertEnum enum2
--                left = convEnum2 ++ " = " ++ convEnum1
--                right = convEnum1 ++ " = " ++ convEnum2
--            in makeDiscreteLeftRight enums (left:leftStr) (right:rightStr)
--         makeDiscreteLeftRight _ leftStr rightStr = (leftStr,rightStr)

printIntType :: FilePath -> NetlistName -> String -> ExceptT ConverterError IO ()
printIntType file unitName name =
   let typeName = "Type'" ++ name
       mkFullTypeName (NetlistName lib unit) = lib ++ "'" ++ unit ++ "'" ++ typeName
       fullTypeName = mkFullTypeName unitName
       typeBaseStr =
         "newtype " ++ typeName ++ " = "
         ++ newline ++ tab
         ++ typeName ++ " Int64"
         ++ newline ++ tab
         ++ "deriving (Eq,Ord)"
       constructorStr =
         let funcName = "mk" ++ typeName
             highVal = show $ (maxBound :: Int64)
             lowVal = show $ (minBound :: Int64)
         in funcName ++ " :: Integer -> " ++ typeName
            ++ newline
            ++ funcName ++ " value = "
            ++ newline ++ tab
            ++ "if value > " ++ highVal ++ " || value < " ++ lowVal
            ++ newline ++ tab ++ tab
            ++ "then error $ \"" ++ typeName ++ " value \" ++ show value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
            ++ newline ++ tab ++ tab
            ++ "else " ++ typeName ++ " $ fromInteger value"
       extractStr =
         let funcName = "extract" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> Integer"
            ++ newline
            ++ funcName ++ " (" ++ typeName ++ " value) = toInteger value"
       outputStr =
         "instance STD.STANDARD.SignalOutput " ++ typeName ++ " where"
         ++ newline ++ tab
         ++ "sigOut = show . extract" ++ typeName
       createComparisonFunc operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'_'" ++ fullTypeName ++ "'out'STD'STANDARD'Type'ANON'BOOLEAN"
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName ++ " -> " ++ "STD.STANDARD.Type'ANON'BOOLEAN"
            ++ newline
            ++ funcName ++ " value1 value2 = STD.STANDARD.boolToBoolean $ value1 " ++ operatorStr ++ " value2"
       equalStr = createComparisonFunc "EQUAL" "=="
       nequalStr = createComparisonFunc "NEQUAL" "/="
       lessThanStr = createComparisonFunc "LESSTHAN" "<"
       lessThanOrEqualStr = createComparisonFunc "LESSTHANOREQUAL" "<="
       greaterThanStr = createComparisonFunc "GREATERTHAN" ">"
       greaterThanOrEqualStr = createComparisonFunc "GREATERTHANOREQUAL" ">="
       createArithFunc operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'_'" ++ fullTypeName ++ "'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName ++ " -> " ++ typeName
            ++ newline
            ++ funcName ++ " value1 value2 = " ++ constrFunc ++ " $ (" ++ extractFunc ++ " value1) " ++ operatorStr ++ " (" ++ extractFunc ++ " value2)"
       addStr = createArithFunc "PLUS" "+"
       minusStr = createArithFunc "MINUS" "-"
       multStr = createArithFunc "MULT" "*"
       divStr = createArithFunc "DIV" "`div`"
       modStr = createArithFunc "MOD" "`mod`"
       remStr = createArithFunc "REM" "`rem`"
       createUnaryOperation operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName
            ++ newline
            ++ funcName ++ " = " ++ constrFunc ++ " . " ++ operatorStr ++ " . " ++ extractFunc
       absStr = createUnaryOperation "ABS" "abs"
       plusStr = createUnaryOperation "PLUS" "(\\s -> s)"
       negStr = createUnaryOperation "MINUS" "negate"
       expStr =
         let funcName = "function'op'EXP'in'" ++ fullTypeName ++ "'_'STD'STANDARD'Type'ANON'INTEGER'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> STD.STANDARD.Type'ANON'INTEGER -> " ++ typeName
            ++ newline
            ++ funcName ++ " value1 value2 = " ++ constrFunc ++ " $ round $ (fromIntegral $ " ++ extractFunc ++ " value1) ^^ (STD.STANDARD.extractType'ANON'INTEGER value2)"
--       typeScalar =
--         "instance (VHDL_Scalar "
--         ++ name
--         ++ ") where"
--         ++ newline ++ tab
--         ++ "scalarLeft = "
--         ++ show left
--         ++ newline ++ tab
--         ++ "scalarRight = "
--         ++ show right
--         ++ newline ++ tab
--         ++ "scalarHigh = "
--         ++ show high
--         ++ newline ++ tab
--         ++ "scalarLow = "
--         ++ show low
--       typeDiscrete =
--         "instance (VHDL_Discrete "
--         ++ typeName
--         ++ ") where"
--         ++ newline ++ tab
--         ++ "discretePos ("
--         ++ typeName
--         ++ "val) = val"
--         ++ newline ++ tab
--         ++ "discreteVal val = "
--         ++ typeName
--         ++ " val"
--         ++ newline ++ tab
--         ++ "discreteSucc val = va 
   in liftIO $ appendFile file $
         newline ++ typeBaseStr
         ++ newline ++ constructorStr
         ++ newline ++ extractStr
         ++ newline ++ outputStr
         ++ newline ++ equalStr
         ++ newline ++ nequalStr
         ++ newline ++ lessThanStr
         ++ newline ++ lessThanOrEqualStr
         ++ newline ++ greaterThanStr
         ++ newline ++ greaterThanOrEqualStr
         ++ newline ++ addStr
         ++ newline ++ minusStr
         ++ newline ++ multStr
         ++ newline ++ divStr
         ++ newline ++ modStr
         ++ newline ++ remStr
         ++ newline ++ absStr
         ++ newline ++ plusStr
         ++ newline ++ negStr
         ++ newline ++ expStr
         ++ newline
         -- ++ typeScalar ++ newline ++ typeDiscrete

printFloatType :: FilePath -> NetlistName -> String -> ExceptT ConverterError IO ()
printFloatType file unitName name =
   let typeName = "Type'" ++ name
       mkFullTypeName (NetlistName lib unit) = lib ++ "'" ++ unit ++ "'" ++ typeName
       fullTypeName = mkFullTypeName unitName
       typeBaseStr =
         "newtype " ++ typeName ++ " = "
         ++ newline ++ tab
         ++ typeName ++ " Double"
         ++ newline ++ tab
         ++ "deriving (Eq,Ord)"
       constructorStr =
         let funcName = "mk" ++ typeName
         in funcName ++ " :: Double -> " ++ typeName
            ++ newline
            ++ funcName ++ " value ="
            ++ newline ++ tab
            ++ "case value of"
            ++ newline ++ tab ++ tab
            ++ "_ | isInfinite value -> error \"" ++ typeName ++ " value is outside of 64 bit IEEE 754 (REAL implementation) range\""
            ++ newline ++ tab ++ tab
            ++ "_ -> " ++ typeName ++ " value"
       extractStr =
         let funcName = "extract" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> Double"
            ++ newline
            ++ funcName ++ " (" ++ typeName ++ " value) = value"
       outputStr =
         "instance STD.STANDARD.SignalOutput " ++ typeName ++ " where"
         ++ newline ++ tab
         ++ "sigOut flt = (showFloat $ extract" ++ typeName ++ " flt) \"\""
       createComparisonFunc operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'_'" ++ fullTypeName ++ "'out'STD'STANDARD'Type'ANON'BOOLEAN"
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName ++ " -> " ++ "STD.STANDARD.Type'ANON'BOOLEAN"
            ++ newline
            ++ funcName ++ " value1 value2 = STD.STANDARD.boolToBoolean $ value1 " ++ operatorStr ++ " value2"
       equalStr = createComparisonFunc "EQUAL" "=="
       nequalStr = createComparisonFunc "NEQUAL" "/="
       lessThanStr = createComparisonFunc "LESSTHAN" "<"
       lessThanOrEqualStr = createComparisonFunc "LESSTHANOREQUAL" "<="
       greaterThanStr = createComparisonFunc "GREATERTHAN" ">"
       greaterThanOrEqualStr = createComparisonFunc "GREATERTHANOREQUAL" ">="
       createArithFunc operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'_'" ++ fullTypeName ++ "'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName ++ " -> " ++ typeName
            ++ newline
            ++ funcName ++ " value1 value2 = " ++ constrFunc ++ " $ (" ++ extractFunc ++ " value1) " ++ operatorStr ++ " (" ++ extractFunc ++ " value2)"
       addStr = createArithFunc "PLUS" "+"
       minusStr = createArithFunc "MINUS" "-"
       multStr = createArithFunc "MULT" "*"
       divStr = createArithFunc "DIV" "/"
       createUnaryOperation operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName
            ++ newline
            ++ funcName ++ " = " ++ constrFunc ++ " . " ++ operatorStr ++ " . " ++ extractFunc
       absStr = createUnaryOperation "ABS" "abs"
       plusStr = createUnaryOperation "PLUS" "(\\s -> s)"
       negStr = createUnaryOperation "MINUS" "negate"
       expStr =
         let funcName = "function'op'EXP'in'" ++ fullTypeName ++ "'_'STD'STANDARD'Type'ANON'INTEGER'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> STD.STANDARD.Type'ANON'INTEGER -> " ++ typeName
            ++ newline
            ++ funcName ++ " value1 value2 = " ++ constrFunc ++ " $ (" ++ extractFunc ++ " value1) ^^ (STD.STANDARD.extractType'ANON'INTEGER value2)"
   in liftIO $ appendFile file $
         newline ++ typeBaseStr
         ++ newline ++ constructorStr
         ++ newline ++ extractStr
         ++ newline ++ outputStr
         ++ newline ++ equalStr
         ++ newline ++ nequalStr
         ++ newline ++ lessThanStr
         ++ newline ++ lessThanOrEqualStr
         ++ newline ++ greaterThanStr
         ++ newline ++ greaterThanOrEqualStr
         ++ newline ++ addStr
         ++ newline ++ minusStr
         ++ newline ++ multStr
         ++ newline ++ divStr
         ++ newline ++ absStr
         ++ newline ++ plusStr
         ++ newline ++ negStr
         ++ newline ++ expStr
         ++ newline

printPhysicalType :: FilePath -> NetlistName -> String -> String -> ExceptT ConverterError IO ()
printPhysicalType fileName unitName name baseUnit =
   let typeName = "Type'" ++ name
       mkFullTypeName (NetlistName lib unit) = lib ++ "'" ++ unit ++ "'" ++ typeName
       fullTypeName = mkFullTypeName unitName
       typeBaseStr =
         "newtype " ++ typeName ++ " = "
         ++ newline ++ tab
         ++ typeName ++ " Int64"
         ++ newline ++ tab
         ++ "deriving (Eq,Ord)"
       constructorStr =
         let funcName = "mk" ++ typeName
             highVal = show $ (maxBound :: Int64)
             lowVal = show $ (minBound :: Int64)
         in funcName ++ " :: Integer -> " ++ typeName
            ++ newline
            ++ funcName ++ " value = "
            ++ newline ++ tab
            ++ "if value > " ++ highVal ++ " || value < " ++ lowVal
            ++ newline ++ tab ++ tab
            ++ "then error $ \"" ++ typeName ++ " value \" ++ show value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
            ++ newline ++ tab ++ tab
            ++ "else " ++ typeName ++ " $ fromInteger value"
       extractStr =
         let funcName = "extract" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> Integer"
            ++ newline
            ++ funcName ++ " (" ++ typeName ++ " value) = toInteger value"
       outputStr =
         "instance STD.STANDARD.SignalOutput " ++ typeName ++ " where"
         ++ newline ++ tab
         ++ "sigOut phys = (show $ extract" ++ typeName ++ " phys) ++ \" " ++ baseUnit ++ "\""
       createComparisonFunc operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'_'" ++ fullTypeName ++ "'out'STD'STANDARD'Type'ANON'BOOLEAN"
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName ++ " -> " ++ "STD.STANDARD.Type'ANON'BOOLEAN"
            ++ newline
            ++ funcName ++ " value1 value2 = STD.STANDARD.boolToBoolean $ value1 " ++ operatorStr ++ " value2"
       equalStr = createComparisonFunc "EQUAL" "=="
       nequalStr = createComparisonFunc "NEQUAL" "/="
       lessThanStr = createComparisonFunc "LESSTHAN" "<"
       lessThanOrEqualStr = createComparisonFunc "LESSTHANOREQUAL" "<="
       greaterThanStr = createComparisonFunc "GREATERTHAN" ">"
       greaterThanOrEqualStr = createComparisonFunc "GREATERTHANOREQUAL" ">="
       createArithFunc operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'_'" ++ fullTypeName ++ "'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName ++ " -> " ++ typeName
            ++ newline
            ++ funcName ++ " value1 value2 = " ++ constrFunc ++ " $ (" ++ extractFunc ++ " value1) " ++ operatorStr ++ " (" ++ extractFunc ++ " value2)"
       addStr = createArithFunc "PLUS" "+"
       minusStr = createArithFunc "MINUS" "-"
       multStr =
         let funcName1 = "function'op'MULT'in'" ++ fullTypeName ++ "'_'STD'STANDARD'Type'ANON'INTEGER'out'" ++ fullTypeName
             funcName2 = "function'op'MULT'in'STD'STANDARD'Type'ANON'INTEGER'_'" ++ fullTypeName ++ "'out'" ++ fullTypeName
             funcName3 = "function'op'MULT'in'" ++ fullTypeName ++ "'_'STD'STANDARD'Type'ANON'REAL'out'" ++ fullTypeName
             funcName4 = "function'op'MULT'in'STD'STANDARD'Type'ANON'REAL'_'" ++ fullTypeName ++ "'out'" ++ fullTypeName
         in funcName1 ++ " :: " ++ typeName ++ " -> STD.STANDARD.Type'ANON'INTEGER -> " ++ typeName
            ++ newline
            ++ funcName1 ++ " value1 value2 = mk" ++ typeName ++ " $ (extract" ++ typeName ++ " value1) * (STD.STANDARD.extractType'ANON'INTEGER value2)"
            ++ newline
            ++ funcName2 ++ " :: STD.STANDARD.Type'ANON'INTEGER -> " ++ typeName ++ " -> " ++ typeName
            ++ newline
            ++ funcName2 ++ " value1 value2 = " ++ funcName1 ++ " value2 value1"
            ++ newline
            ++ funcName3 ++ " :: " ++ typeName ++ " -> STD.STANDARD.Type'ANON'REAL -> " ++ typeName
            ++ newline
            ++ funcName3 ++ " value1 value2 = mk" ++ typeName ++ " $ round $ (fromIntegral $ extract" ++ typeName ++ " value1) * (STD.STANDARD.extractType'ANON'REAL value2)"
            ++ newline
            ++ funcName4 ++ " :: STD.STANDARD.Type'ANON'REAL -> " ++ typeName ++ " -> " ++ typeName
            ++ newline
            ++ funcName4 ++ " value1 value2 = " ++ funcName3 ++ " value2 value1"
       divStr =
         let funcName1 = "function'op'DIV'in'" ++ fullTypeName ++ "'_'STD'STANDARD'Type'ANON'INTEGER'out'" ++ fullTypeName
             funcName2 = "function'op'DIV'in'" ++ fullTypeName ++ "'_'STD'STANDARD'Type'ANON'REAL'out'" ++ fullTypeName
             funcName3 = "function'op'DIV'in'" ++ fullTypeName ++ "'_'" ++ fullTypeName ++ "'out'UniversalInteger"
         in funcName1 ++ " :: " ++ typeName ++ " -> STD.STANDARD.Type'ANON'INTEGER -> " ++ typeName
            ++ newline
            ++ funcName1 ++ " value1 value2 = mk" ++ typeName ++ " $ (extract" ++ typeName ++ " value1) `div` (STD.STANDARD.extractType'ANON'INTEGER value2)"
            ++ newline
            ++ funcName2 ++ " :: " ++ typeName ++ " -> STD.STANDARD.Type'ANON'REAL -> " ++ typeName
            ++ newline
            ++ funcName2 ++ " value1 value2 = mk" ++ typeName ++ " $ round $ (fromIntegral $ extract" ++ typeName ++ " value1) / (STD.STANDARD.extractType'ANON'REAL value2)"
            ++ newline
            ++ funcName3 ++ " :: " ++ typeName ++ " -> " ++ typeName ++ " -> Integer"
            ++ newline
            ++ funcName3 ++ " value1 value2 = (extract" ++ typeName ++ " value1) `div` (extract" ++ typeName ++ " value2)"
       createUnaryOperation operatorName operatorStr =
         let funcName = "function'op'" ++ operatorName ++ "'in'" ++ fullTypeName ++ "'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> " ++ typeName
            ++ newline
            ++ funcName ++ " = " ++ constrFunc ++ " . " ++ operatorStr ++ " . " ++ extractFunc
       absStr = createUnaryOperation "ABS" "abs"
       plusStr = createUnaryOperation "PLUS" "(\\s -> s)"
       negStr = createUnaryOperation "MINUS" "negate"
       expStr =
         let funcName = "function'op'EXP'in'" ++ fullTypeName ++ "'_'STD'STANDARD'Type'ANON'INTEGER'out'" ++ fullTypeName
             extractFunc = "extract" ++ typeName
             constrFunc = "mk" ++ typeName
         in funcName ++ " :: " ++ typeName ++ " -> STD.STANDARD.Type'ANON'INTEGER -> " ++ typeName
            ++ newline
            ++ funcName ++ " value1 value2 = " ++ constrFunc ++ " $ round $ (fromIntegral $ " ++ extractFunc ++ " value1) ^^ (STD.STANDARD.extractType'ANON'INTEGER value2)"
   in liftIO $ appendFile fileName $
         newline ++ typeBaseStr
         ++ newline ++ constructorStr
         ++ newline ++ extractStr
         ++ newline ++ outputStr
         ++ newline ++ equalStr
         ++ newline ++ nequalStr
         ++ newline ++ lessThanStr
         ++ newline ++ lessThanOrEqualStr
         ++ newline ++ greaterThanStr
         ++ newline ++ greaterThanOrEqualStr
         ++ newline ++ addStr
         ++ newline ++ minusStr
         ++ newline ++ multStr
         ++ newline ++ divStr
         ++ newline ++ absStr
         ++ newline ++ plusStr
         ++ newline ++ negStr
         ++ newline ++ expStr
         ++ newline

printArrayType :: FilePath -> NetlistName -> String -> [(NetlistName,String,Subtype)] -> (NetlistName,String) -> ExceptT ConverterError IO ()
printArrayType fileName unitName name bounds (elemTypePackage,elemTypeName) =
   let typeName = "Type'" ++ name
       mkFullTypeName (NetlistName lib unit) = lib ++ "'" ++ unit ++ "'" ++ typeName
       fullTypeName = mkFullTypeName unitName
       typeBaseStr =
         "newtype " ++ typeName ++ " ="
         ++ newline ++ tab
         ++ typeName ++ " (Data.Map.Strict.Map ("
         ++ (concat $ intersperse "," $ map (\(subtypePackage,subtypeName,_) -> show subtypePackage ++ ".Type'" ++ subtypeName) bounds)
         ++ ") " ++ show elemTypePackage ++ ".Type'" ++ elemTypeName ++ ")"
       constructorStr =
         let funcName = "mk" ++ typeName
             extractSubtypeType (EnumerationSubtype _ (enumPackage,enumName) _ _) = show enumPackage ++ ".Type'" ++ enumName
             extractSubtypeType (IntegerSubtype _ _ _) = "Integer"
             extractBaseFunc (EnumerationSubtype _ (enumPackage,enumName) _ _) = ""
             extractBaseFunc (IntegerSubtype _ (intPackage,intName) _) = show intPackage ++ ".mkType'" ++ intName ++ " $ "
         in funcName ++ " :: [(("
            ++ (concat $ intersperse "," $ map (\(_,_,subtypeData) -> extractSubtypeType subtypeData) bounds)
            ++ ")," ++ show elemTypePackage ++ ".Type'" ++ elemTypeName ++ ")] -> " ++ typeName
            ++ newline
            ++ funcName ++ " ="
            ++ newline ++ tab
            ++ "let modKeyFunc (" ++ (concat $ intersperse "," $ map (\s -> "value" ++ show s) [1..(length bounds)]) ++ ") ="
            ++ newline ++ tab ++ tab ++ tab
            ++ "( "
            ++ ( concat
               $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
               $ map (\((subtypePackage,subtypeName,subtypeData),valueName) -> show subtypePackage ++ ".mkType'" ++ subtypeName ++ " $ " ++ extractBaseFunc subtypeData ++ valueName)
               $ zip bounds
               $ map (\value -> "value" ++ show value)
               $ [1..(length bounds)]
               )
            ++ newline ++ tab ++ tab ++ tab
            ++ ")"
            ++ newline ++ tab ++ tab
            ++ " mapFunc (key,value) = (modKeyFunc key,value)"
            ++ newline ++ tab
            ++ "in " ++ typeName ++ " . Data.Map.Strict.fromList . map mapFunc"
   in liftIO $ appendFile fileName $
         newline ++ typeBaseStr
         ++ newline ++ constructorStr
         ++ newline
