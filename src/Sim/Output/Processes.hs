module Sim.Output.Processes
   ( outputProcesses
   ) where

import qualified Data.Map.Strict as MapS
import Data.List (intersperse)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import Numeric (showFFloat)

import Parser.Types.Expressions (AllTypes(..))
import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Calculation(..)
         , Value(..)
         , SequentialStatement(..)
         , Type(..)
         , SignalType(..)
         )
import Parser.Netlist.Types.Stores
         ( Process(..)
         , ProcessStore
         )
import Sim.Output.Types
         ( outputTypes
         , showEnum
         )
import Sim.Output.Subtypes (outputSubtypes)
import Sim.Output.Constants (outputConstants)
import Manager.Types.Error (ConverterError)

newline = "\n"
tab = "   "

outputProcesses :: FilePath -> NetlistName -> ProcessStore -> ExceptT ConverterError IO ()
outputProcesses file unitName = (outputProcesses' file unitName) . MapS.toList

outputProcesses' :: FilePath -> NetlistName -> [(String,Process)] -> ExceptT ConverterError IO ()
outputProcesses' fileName unitName ((processName,process):others) = do
   let nestDeclNames = MapS.mapKeys $ \name -> processName ++ "'" ++ name
       nestedNames = zip (repeat unitName) $
                        (MapS.keys $ processTypes process)
                        ++ (MapS.keys $ processSubtypes process)
                        ++ (MapS.keys $ processConstants process)
   --outputFunctions fileName unitName $ nestDeclNames $ processFunctions process
   outputTypes fileName unitName $ nestDeclNames $ processTypes process
   outputSubtypes fileName $ nestDeclNames $ processSubtypes process
   outputConstants fileName $ nestDeclNames $ processConstants process
   -- ?? Etc.
   outputStatements fileName unitName nestedNames processName $ processStatements process
   outputProcesses' fileName unitName others
outputProcesses' _ _ [] = return ()

outputStatements :: FilePath -> NetlistName -> [(NetlistName,String)] -> String -> [SequentialStatement] -> ExceptT ConverterError IO ()
outputStatements fileName unitName@(NetlistName _ entityName) nestedNames processName statements =
   let makeProcessContents :: [SequentialStatement] -> [(Maybe [String],[[String]])] -> [Maybe Calculation] -> ([(Maybe [String],[[String]])],[Maybe Calculation])
       makeProcessContents (wait@(WaitStatement _ _ timeout):otherStatements) waitList otherTimeouts =
         let newStatement = convertStatement wait unitName nestedNames processName
         in makeProcessContents otherStatements ((Just newStatement,[]):waitList) (timeout:otherTimeouts)
       makeProcessContents (nonWaitStatement:otherStatements) [] [] =
         let newStatement = convertStatement nonWaitStatement unitName nestedNames processName
         in makeProcessContents otherStatements [(Nothing,[newStatement])] [Nothing]
       makeProcessContents (nonWaitStatement:otherStatements) ((wait,nonWaits):rest) otherTimeouts =
         let newStatement = convertStatement nonWaitStatement unitName nestedNames processName
         in makeProcessContents otherStatements ((wait,newStatement:nonWaits):rest) otherTimeouts
       makeProcessContents [] results timeouts = (results,timeouts)
       (processContents,timeouts) = makeProcessContents statements [] []
       numStages = length processContents - 1
       initialStateStr = case last timeouts of
                           Just timeExp -> "(0,(" ++ convertCalculation nestedNames processName timeExp ++ "))"
                           Nothing -> "(0,STD.STANDARD.mkType'ANON'TIME 0)"
       convertContents :: Int -> [(Maybe [String],[[String]])] -> [[String]] -> [String] --[(String,[String])]
       convertContents val ((waitData,otherStrs):rest) contentsStr =
         let valStr = "stage" ++ show val
             checkStr =
               [valStr ++ "check ="]
               ++ case waitData of
                     Just waitStr -> map (\s -> tab ++ s) $
                        waitStr
                        ++ [ tab ++ "then " ++ valStr
                           , tab ++ "else return (" ++ show val ++ ",waitTime)"
                           ]
                     Nothing -> [tab ++ valStr]
             runStr =
               [valStr ++ " = do"]
               ++ (map (\s -> tab ++ s) $ concat $ reverse otherStrs)
               ++ [tab ++ case (last processContents) of
                           (Just _,_) | numStages == val -> "return " ++ initialStateStr
                           (Nothing,_) | numStages == val -> "stage0check"
                           _ ->
                              "return ("
                              ++ (show $ val + 1)
                              ++ ",("
                              ++ (convertCalculation nestedNames processName $ fromJust $ timeouts !! (numStages - (val + 1)))
                              ++ "))"
                  ]
         in convertContents (val - 1) rest ((checkStr++runStr):contentsStr)
       convertContents _ [] contentsStr = concat contentsStr
       processContentsStr =
         concat $
         map (\s -> newline ++ tab ++ tab ++ s) $
         convertContents numStages processContents []
       entityStackName = show unitName ++ ".Entity'" ++ entityName ++ "'Stack Int"
       processFuncStr =
         "process'" ++ processName ++ " :: (Int,STD.STANDARD.Type'ANON'TIME) -> " ++ entityStackName
         ++ newline
         ++ "process'" ++ processName ++ " (stageNum,waitTime) = do"
         ++ newline ++ tab
         ++ "(Control'Time realTime deltaTime) <- lift get"
         ++ newline ++ tab
         ++ "(Entity'State portsIn state _) <- get"
         ++ newline ++ tab
         ++ "let"
         ++ processContentsStr
         ++ newline ++ tab
         ++ " stageList = ["
         ++ ( concat
            $ intersperse ","
            $ map (\s -> "stage" ++ show s ++ "check") [0..numStages]
            )
         ++ "]"
         ++ newline ++ tab
         ++ " stageRun = stageList !! stageNum"
         ++ newline ++ tab
         ++ "stageRun"
       processInitialStr =
         let constName = "process'initial'" ++ processName
         in constName ++ " :: (Int,STD.STANDARD.Type'ANON'TIME)"
            ++ newline
            ++ constName ++ " = " ++ initialStateStr
   in liftIO $ appendFile fileName $
         newline
         ++ processFuncStr ++ newline
         ++ processInitialStr ++ newline

convertStatement :: SequentialStatement -> NetlistName -> [(NetlistName,String)] -> String -> [String]
convertStatement (WaitStatement sensitivity condition timeout) unitName nestedNames processName =
   let convertSensitivity :: (SignalType,String) -> String
       convertSensitivity (signalType,signalName) =
         "("
         ++ "control'signal'active $ "
         ++ case signalType of
               InternalSignal -> "signal'" ++ signalName ++ " $ state"
               PortIn -> "ports'in'" ++ signalName ++ " $ portsIn"
         ++ ")"
       sensitivityStr = if null sensitivity
                           then "True"
                           else
                              "("
                              ++ (concat $ intersperse " || " $ map convertSensitivity sensitivity)
                              ++ ")"
       conditionStr = "((" ++ convertCalculation nestedNames processName condition ++ ") == STD.STANDARD.Type'ANON'BOOLEAN'Iden'TRUE)"
       timeoutStr = case timeout of
                     Just _ -> "(waitTime == realTime)"
                     Nothing -> "(STD.STANDARD.mkType'ANON'TIME " ++ show (maxBound :: Int64) ++ " == realTime)"
   in ["if (" ++ sensitivityStr ++ " && " ++ conditionStr ++ ") || " ++ timeoutStr]
convertStatement (AssertStatement condition report severity) unitName nestedNames processName =
   let unitNameStr = show unitName
       conditionStr = convertCalculation nestedNames processName condition
       messageStr = "[]" -- ?? Temp until arrays parsed
       severityStr = convertCalculation nestedNames processName severity
   in [ "when (" ++ conditionStr ++ ") $"
      , tab ++ "case " ++ severityStr ++ " of"
      , tab ++ tab ++ "Type'ANON'BOOLEAN'Iden'NOTE -> control'assertNote " ++ unitNameStr ++ " " ++ messageStr
      , tab ++ tab ++ "Type'ANON'BOOLEAN'Iden'WARNING -> control'assertWarning " ++ unitNameStr ++ " " ++ messageStr
      , tab ++ tab ++ "Type'ANON'BOOLEAN'Iden'ERROR -> control'assertError " ++ unitNameStr ++ " " ++ messageStr
      , tab ++ tab ++ "Type'ANON'BOOLEAN'Iden'FAILURE -> control'assertFailure " ++ unitNameStr ++ " " ++ messageStr
      ]
convertStatement NullStatement _ _ _ = ["return ()"]

convertCalculation :: [(NetlistName,String)] -> String -> Calculation -> String
convertCalculation nestedNames processName (Calc_Value value typeData) =
   case (value,typeData) of
      (Value_Enum _ enum,Type_Type (enumPackage,enumName) _) | elem (enumPackage,enumName) nestedNames ->
         showEnum (show enumPackage ++ ".Type'" ++ processName ++ "'" ++ enumName) enum
      (Value_Enum _ enum,Type_Type (enumPackage,enumName) _) ->
         showEnum (show enumPackage ++ "." ++ enumName) enum
      (Value_Int int,Type_UniversalInt) -> show int
      (Value_Int int,Type_Type (typePackage,typeName) _) ->
         show typePackage ++ ".mkType'" ++ typeName ++ " " ++ show int
      (Value_Float flt,Type_UniversalReal) ->
         (showFFloat Nothing flt) ""
      (Value_Float flt,Type_Type (typePackage,typeName) _) -> 
         show typePackage ++ ".mkType'" ++ typeName ++ " " ++ (showFFloat Nothing flt) ""
      (Value_Physical int,Type_Type (physPackage,physName) _) | elem (physPackage,physName) nestedNames ->
         show physPackage ++ ".mkType'" ++ processName ++ "'" ++ physName ++ " " ++ show int
      (Value_Physical int,Type_Type (physPackage,physName) _) ->
         show physPackage ++ ".mkType'" ++ physName ++ " " ++ show int
      --Value_Array
--convertCalculation (Calc_FunctionCall ) =
convertCalculation nestedNames processName (Calc_Const (unitName,constName))
   | elem (unitName,constName) nestedNames = show unitName ++ ".constant'" ++ processName ++ "'" ++ constName
   | otherwise = show unitName ++ "." ++ constName
convertCalculation a1 a2 (Calc_BuiltinNegate calc (Type_Type typeName _)) =
   (printUnaryFunctionName "MINUS" typeName) ++ " (" ++ convertCalculation a1 a2 calc ++ ")"
convertCalculation a1 a2 (Calc_BuiltinNegate calc Type_UniversalInt) = "negate (" ++ convertCalculation a1 a2 calc ++ ")"
convertCalculation a1 a2 (Calc_BuiltinNegate calc Type_UniversalReal) = "negate (" ++ convertCalculation a1 a2 calc ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSum (calc1,Type_Type typeName1 _) (calc2,Type_Type _ _)) =
   (printFunctionName "PLUS" typeName1 typeName1 typeName1)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSum (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 calc1 ++ ") + (" ++ convertCalculation a1 a2 calc2 ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSum (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 calc1 ++ ") + (" ++ convertCalculation a1 a2 calc2 ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSum (calc1,Type_Type typeFullName@(typePackage,typeName) _) (calc2,Type_UniversalInt)) =
   (printFunctionName "PLUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSum (calc1,Type_Type typeFullName@(typePackage,typeName) _) (calc2,Type_UniversalReal)) =
   (printFunctionName "PLUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSum (calc1,Type_UniversalInt) (calc2,Type_Type typeFullName@(typePackage,typeName) _)) =
   (printFunctionName "PLUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSum (calc1,Type_UniversalReal) (calc2,Type_Type typeFullName@(typePackage,typeName) _)) =
   (printFunctionName "PLUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSubtract (calc1,Type_Type typeName1 _) (calc2,Type_Type _ _)) =
   (printFunctionName "MINUS" typeName1 typeName1 typeName1)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSubtract (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 calc1 ++ ") - (" ++ convertCalculation a1 a2 calc2 ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSubtract (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 calc1 ++ ") - (" ++ convertCalculation a1 a2 calc2 ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSubtract (calc1,Type_Type typeFullName@(typePackage,typeName) _) (calc2,Type_UniversalInt)) =
   (printFunctionName "MINUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSubtract (calc1,Type_Type typeFullName@(typePackage,typeName) _) (calc2,Type_UniversalReal)) =
   (printFunctionName "MINUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSubtract (calc1,Type_UniversalInt) (calc2,Type_Type typeFullName@(typePackage,typeName) _)) =
   (printFunctionName "MINUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinSubtract (calc1,Type_UniversalReal) (calc2,Type_Type typeFullName@(typePackage,typeName) _)) =
   (printFunctionName "MINUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
--convertCalculation a1 a2 (Calc_BuiltinConcat
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_Type typeName1 _) (calc2,Type_Type typeName2 _))
   | typeName1 == typeName2 = (printFunctionName "MULT" typeName1 typeName1 typeName1)
                              ++ " (" ++ convertCalculation a1 a2 calc1
                              ++ ") (" ++ convertCalculation a1 a2 calc2
                              ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 calc1 ++ ") * (" ++ convertCalculation a1 a2 calc2 ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 calc1 ++ ") * (" ++ convertCalculation a1 a2 calc2 ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_Type typeName1 (PhysicalType _ _)) (calc2,Type_Type typeName2 _)) =
   (printFunctionName "MULT" typeName1 typeName2 typeName1)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_Type typeName (PhysicalType _ _)) (calc2,Type_UniversalInt)) =
   (printFunctionName "MULT" typeName (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (STD.STANDARD.mk'Type'ANON'INTEGER $ " ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_Type typeName (PhysicalType _ _)) (calc2,Type_UniversalReal)) =
   (printFunctionName "MULT" typeName (NetlistName "STD" "STANDARD","ANON'REAL") typeName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (STD.STANDARD.mk'Type'ANON'REAL $ " ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_Type typeName1 _) (calc2,Type_Type typeName2 (PhysicalType _ _))) =
   (printFunctionName "MULT" typeName1 typeName2 typeName2)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_UniversalInt) (calc2,Type_Type typeName (PhysicalType _ _))) =
   (printFunctionName "MULT" (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName typeName)
   ++ " (STD.STANDARD.mk'Type'ANON'INTEGER $ " ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMult (calc1,Type_UniversalReal) (calc2,Type_Type typeName (PhysicalType _ _))) =
   (printFunctionName "MULT" (NetlistName "STD" "STANDARD","ANON'REAL") typeName typeName)
   ++ " (STD.STANDARD.mk'Type'ANON'REAL $ " ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
-- ?? Bultin div
convertCalculation a1 a2 (Calc_BuiltinMod calc1 calc2 (Type_Type typeName _)) =
   (printFunctionName "MOD" typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinMod calc1 calc2 Type_UniversalInt) = "(" ++ convertCalculation a1 a2 calc1 ++ ") `mod` (" ++ convertCalculation a1 a2 calc2 ++ ")"
convertCalculation a1 a2 (Calc_BuiltinRem calc1 calc2 (Type_Type typeName _)) =
   (printFunctionName "REM" typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
convertCalculation a1 a2 (Calc_BuiltinRem calc1 calc2 Type_UniversalInt) = "(" ++ convertCalculation a1 a2 calc1 ++ ") `rem` (" ++ convertCalculation a1 a2 calc2 ++ ")"
convertCalculation a1 a2 (Calc_BuiltinExp (calc1,Type_Type typeName _) (calc2,Type_Type _ _)) =
   (printFunctionName "EXP" typeName (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName)
   ++ " (" ++ convertCalculation a1 a2 calc1
   ++ ") (" ++ convertCalculation a1 a2 calc2
   ++ ")"
-- ?? Need universal int/real expressions for exponentials
convertCalculation a1 a2 (Calc_BuiltinAbs calc (Type_Type typeName _)) =
   (printUnaryFunctionName "ABS" typeName) ++ " (" ++ convertCalculation a1 a2 calc ++ ")"
convertCalculation a1 a2 (Calc_BuiltinAbs calc Type_UniversalInt) = "abs (" ++ convertCalculation a1 a2 calc ++ ")"
convertCalculation a1 a2 (Calc_BuiltinAbs calc Type_UniversalReal) = "abs (" ++ convertCalculation a1 a2 calc ++ ")"
-- ?? Other expressions

printUnaryFunctionName :: String -> (NetlistName,String) -> String
printUnaryFunctionName opName typeName =
   let typeFullName = printFunctionType typeName
   in "function'op'" ++ opName ++ "'in'" ++ typeFullName ++ "'out'" ++ typeFullName

printFunctionName :: String -> (NetlistName,String) -> (NetlistName,String) -> (NetlistName,String) -> String
printFunctionName opName typeName1 typeName2 typeName3 =
   "function'op'" ++ opName
   ++ "'in'" ++ printFunctionType typeName1
   ++ "'_'" ++ printFunctionType typeName2
   ++ "'out'" ++ printFunctionType typeName3

printFunctionType :: (NetlistName,String) -> String
printFunctionType (NetlistName typePackage typeUnit,typeName) = typePackage ++ "'" ++ typeUnit ++ "'" ++ typeName
