module Sim.Subtypes
   ( outputSubtypes
   ) where

import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import qualified Data.Map.Strict as MapS
import Numeric (showFFloat)

import Parser.Netlist.Types.Representation
         ( NetlistName
         , Subtype(..)
         , Enumerate(..)
         , IntegerRange
         , FloatRange
         )
import Parser.Netlist.Functions.Representation
         ( int_scalarHigh
         , int_scalarLow
         , float_scalarHigh
         , float_scalarLow
         )
import Parser.Netlist.Types.Stores
         ( SubtypeStore
         )
import Sim.Types (showEnum)
import Manager.Types.Error (ConverterError)

newline = "\n"
tab = "   "

outputSubtypes :: FilePath -> SubtypeStore -> ExceptT ConverterError IO ()
outputSubtypes fileName = (outputSubtypes' fileName) . MapS.toList

outputSubtypes' :: FilePath -> [(String,Subtype)] -> ExceptT ConverterError IO ()
outputSubtypes' fileName ((name,subtypeData):others) = do
   case subtypeData of
      EnumerationSubtype _ typeName enums constraint -> printEnumSubtype fileName name typeName enums constraint
      IntegerSubtype _ typeName constraint -> printIntegerSubtype fileName name typeName constraint
      FloatingSubtype _ typeName constraint -> printFloatingSubtype fileName name typeName constraint
      PhysicalSubtype _ typeName _ _ constraint -> printPhysicalSubtype fileName name typeName constraint
      ArraySubtype _ typeName bounds elementTypeName elementTypeData -> printArraySubtype fileName name typeName bounds elementTypeName elementTypeData
   outputSubtypes' fileName others
outputSubtypes' _ [] = return ()

printEnumSubtype :: FilePath -> String -> (NetlistName,String) -> [Enumerate] -> (Enumerate,Enumerate) -> ExceptT ConverterError IO ()
printEnumSubtype fileName name (baseTypePackage,baseTypeName) enums (lowBound,highBound) =
   let subtypeName = "Type'" ++ name
       baseTypeFullName = show baseTypePackage ++ ".Type'" ++ baseTypeName
       typeBaseStr = "type " ++ subtypeName ++ " = " ++ baseTypeFullName
       constructorStr =
         let funcName = "mk" ++ subtypeName
             splitEnums False (enum:others) inRange =
               if enum == lowBound
                  then splitEnums True others (enum:inRange)
                  else splitEnums False others inRange
             splitEnums True (enum:others) inRange =
               if enum == highBound
                  then (enum:inRange)
                  else splitEnums True others (enum:inRange)
             validEnums = splitEnums False enums []
             enumError (Enum_Identifier str) = "identifier value \\\"" ++ str ++ "\\\""
             enumError (Enum_Char chr) = "identifier character '" ++ [chr] ++ "'"
         in funcName ++ " :: " ++ subtypeName ++ " -> " ++ subtypeName
            ++ newline
            ++ ( concat
               $ map (\fullEnumName -> funcName ++ " " ++ fullEnumName ++ " = " ++ fullEnumName ++ newline)
               $ map (\enum -> showEnum baseTypeFullName enum)
               $ validEnums
               )
            ++ ( if length validEnums == length enums
                  then ""
                  else funcName ++ " otherEnum = error $ \"Enum value of type \\\"" ++ subtypeName ++ "\\\" is out of range\""
               )
   in liftIO $ appendFile fileName $ newline ++ typeBaseStr ++ newline ++ constructorStr

printIntegerSubtype :: FilePath -> String -> (NetlistName,String) -> IntegerRange -> ExceptT ConverterError IO ()
printIntegerSubtype fileName name (baseTypePackage,baseTypeName) intRange =
   let subtypeName = "Type'" ++ name
       baseTypeFullName = show baseTypePackage ++ ".Type'" ++ baseTypeName
       typeBaseStr = "type " ++ subtypeName ++ " = " ++ baseTypeFullName
       constructorStr =
         let funcName = "mk" ++ subtypeName
             highVal = show $ int_scalarHigh intRange
             lowVal = show $ int_scalarLow intRange
         in funcName ++ " :: " ++ subtypeName ++ " -> " ++ subtypeName
            ++ newline
            ++ funcName ++ " wrappedValue = "
            ++ newline ++ tab
            ++ "let value = " ++ show baseTypePackage ++ ".extractType'" ++ baseTypeName ++ " wrappedValue"
            ++ newline ++ tab
            ++ "in if value > " ++ highVal ++ " || value < " ++ lowVal
            ++ newline ++ tab ++ tab ++ tab
            ++ "then error $ \"" ++ subtypeName ++ " value \" ++ show value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
            ++ newline ++ tab ++ tab ++ tab
            ++ "else wrappedValue"
   in liftIO $ appendFile fileName $ newline ++ typeBaseStr ++ newline ++ constructorStr ++ newline

printFloatingSubtype :: FilePath -> String -> (NetlistName,String) -> FloatRange -> ExceptT ConverterError IO ()
printFloatingSubtype fileName name (baseTypePackage,baseTypeName) floatRange =
   let subtypeName = "Type'" ++ name
       baseTypeFullName = show baseTypePackage ++ ".Type'" ++ baseTypeName
       typeBaseStr = "type " ++ subtypeName ++ " = " ++ baseTypeFullName
       constructorStr =
         let funcName = "mk" ++ subtypeName
             highVal = (showFFloat Nothing $ float_scalarHigh floatRange) ""
             lowVal = (showFFloat Nothing $ float_scalarLow floatRange) ""
         in funcName ++ " :: " ++ subtypeName ++ " -> " ++ subtypeName
            ++ newline
            ++ funcName ++ " wrappedValue ="
            ++ newline ++ tab
            ++ "let value = " ++ show baseTypePackage ++ ".extractType'" ++ baseTypeName ++ " wrappedValue"
            ++ newline ++ tab
            ++ "in if value < " ++ lowVal ++ " || value > " ++ highVal
            ++ newline ++ tab ++ tab ++ tab
            ++ "then error $ \"" ++ subtypeName ++ " value \" ++ show value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
            ++ newline ++ tab ++ tab ++ tab
            ++ "else wrappedValue"
   in liftIO $ appendFile fileName $ newline ++ typeBaseStr ++ newline ++ constructorStr ++ newline

printPhysicalSubtype :: FilePath -> String -> (NetlistName,String) -> IntegerRange -> ExceptT ConverterError IO ()
printPhysicalSubtype fileName name (baseTypePackage,baseTypeName) intRange =
   let subtypeName = "Type'" ++ name
       baseTypeFullName = show baseTypePackage ++ ".Type'" ++ baseTypeName
       typeBaseStr = "type " ++ subtypeName ++ " = " ++ baseTypeFullName
       constructorStr =
         let funcName = "mk" ++ subtypeName
             highVal = show $ int_scalarHigh intRange
             lowVal = show $ int_scalarLow intRange
         in funcName ++ " :: " ++ subtypeName ++ " -> " ++ subtypeName
            ++ newline
            ++ funcName ++ " wrappedValue ="
            ++ newline ++ tab
            ++ "let value = " ++ show baseTypePackage ++ ".extractType'" ++ baseTypeName ++ " wrappedValue"
            ++ newline ++ tab
            ++ "in if value > " ++ highVal ++ " || value < " ++ lowVal
            ++ newline ++ tab ++ tab ++ tab
            ++ "then error $ \"" ++ subtypeName ++ " value \" ++ show value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
            ++ newline ++ tab ++ tab ++ tab
            ++ "else wrappedValue"
   in liftIO $ appendFile fileName $ newline ++ typeBaseStr ++ newline ++ constructorStr ++ newline

printArraySubtype :: FilePath -> String -> (NetlistName,String) -> [(NetlistName,String,Subtype)] -> (NetlistName,String) -> Subtype -> ExceptT ConverterError IO ()
printArraySubtype fileName name (baseTypePackage,baseTypeName) bounds (elemTypePackage,elemTypeName) elemTypeData =
   let subtypeName = "Type'" ++ name
       baseTypeFullName = show baseTypePackage ++ ".Type'" ++ baseTypeName
       typeBaseStr = "type " ++ subtypeName ++ " = " ++ baseTypeFullName
--       constructorStr =
--         let funcName = "mk" ++ subtypeName
--       insertFuncStr =
--         let funcName = "arrayInsert'" ++ subtypeName
--         in funcName ++ " :: ("
--       constructorStrs =
--         let insertFuncName = "arrayInsert'" ++ subtypeName
--             constrFuncName = "mk" ++ subtypeName
--             sliceFuncName = "arraySlice'" ++ subtypeName
--             elemTypeFullName = show elemTypePackage ++ ".Type'" ++ elemTypeName
--       constructorStrs =
--         let func1Name = "mk'ArraySingle'" ++ subtypeName -- haskellIndex1 -> haskellIndex2 -> ... -> value -> ((vhdlIndex1,vhdlIndex2,...),value)
--             func2Name = "mk'ArrayInsert'" ++ subtypeName -- haskellIndex1 -> haskellIndex2 -> ... -> value -> oldArray -> newArray
--             func3Name = "mk" ++ subtypeName -- (haskellIndex1,haskellIndex1) -> (haskellIndex2,haskellIndex2) -> ... -> [[...value]]... -> oldArray -> newArray
--             getHaskellType (EnumerationSubtype _ (enumPackage,enumName) _ _) = show enumPackage ++ ".Type'" ++ enumName
--             getHaskellType (IntegerSubtype _ _ _) = "Integer"
--             indexVarNums = map show [1..(length bounds)]
--         in funcName ++ " :: " ++ (concat $ intersperse " -> " $ map (\(_,_,subtype) -> getHaskellType subtype) bounds) ++ " -> " ++ elemTypeFullName ++ " -> " ++ subtypeName
--            ++ newline
--            ++ funcName ++ (concat $ map (\index -> " index" ++ show index) indexVars) ++ " value ="
--            ++ newline ++ tab
--            ++ "let"
--            ++ ( concat
--               $ zipWith
--                  (\(boundPackage,boundName,_) indexVal ->
--                        newline ++ tab ++ tab
--                        ++ "convIndex" ++ show indexVal ++ " = "
--                        ++ show boundPackage ++ ".mkType'" ++ boundName ++ " index" ++ show indexVal
--                  )
--                  bounds
--                  indexVars
--               )
--            ++ newline ++ tab
--            ++ "in (" ++ (concat $ intersperse "," $ map (\i -> "convIndex" ++ show i) indexVars) ++ ")"
--
--         in funcName ++ " :: Integer -> " ++ subtypeName
--            ++ newline
--            ++ funcName ++ " value ="
--            ++ newline ++ tab
--            ++ "if value > " ++ highVal
--            ++ " || value < " ++ lowVal
--            ++ newline ++ tab ++ tab
--            ++ "then " ++ baseTypeFullName ++ " value"
--            ++ newline ++ tab ++ tab
--            ++ "else error $ \"" ++ subtypeName ++ " value \" ++ value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
   in liftIO $ appendFile fileName $ newline ++ typeBaseStr ++ newline -- ++ constructorStrs ++ newline
