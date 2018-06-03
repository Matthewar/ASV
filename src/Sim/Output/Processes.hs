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
         , Waveform(..)
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
         let newStatement = convertStatement wait unitName nestedNames processName Nothing
         in makeProcessContents otherStatements ((Just newStatement,[]):waitList) (timeout:otherTimeouts)
       makeProcessContents (nonWaitStatement:otherStatements) [] [] =
         let newStatement = convertStatement nonWaitStatement unitName nestedNames processName (Just 0)
         in makeProcessContents otherStatements [(Nothing,[newStatement])] [Nothing]
       makeProcessContents (nonWaitStatement:otherStatements) ((wait,nonWaits):rest) otherTimeouts =
         let newStatement = convertStatement nonWaitStatement unitName nestedNames processName (Just $ length nonWaits)
         in makeProcessContents otherStatements ((wait,newStatement:nonWaits):rest) otherTimeouts
       makeProcessContents [] results timeouts = (results,timeouts)
       (processContents,timeouts) = makeProcessContents statements [] []
       numStages = length processContents - 1
       initialStateStr = case last timeouts of -- ?? Check that wait time is positive, this is globally static (uses initial value of any signals), should be analysed before printing
                           Just timeExp -> "(0,control'delayCheck (" ++ convertCalculation nestedNames processName True timeExp ++ "))"
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
               ++ [tab ++ case (last processContents,last timeouts) of
                           ((Just _,_),Just timeExp) | numStages == val -> "return (0,control'delayCheck (" ++ convertCalculation nestedNames processName False timeExp ++ "))"
                           ((Just _,_),Nothing) | numStages == val -> "return (0,STD.STANDARD.mkType'ANON'TIME 0)"
                           ((Nothing,_),_) | numStages == val -> "stage0check"
                           _ ->
                              "return ("
                              ++ (show $ val + 1)
                              ++ ",STD.STANDARD.function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME realTime (control'delayCheck ("
                              ++ (convertCalculation nestedNames processName False $ fromJust $ timeouts !! (numStages - (val + 1)))
                              ++ ")))"
                  ]
         in convertContents (val - 1) rest ((checkStr++runStr):contentsStr)
       convertContents _ [] contentsStr = concat contentsStr
       processContentsStr =
         concat $
         map (\s -> newline ++ tab ++ tab ++ s) $
         convertContents numStages processContents []
       entityStackName = show unitName ++ ".Entity'" ++ entityName ++ "'Stack (Int,STD.STANDARD.Type'ANON'TIME)"
       processFuncStr =
         "process'" ++ processName ++ " :: (Int,STD.STANDARD.Type'ANON'TIME) -> " ++ entityStackName
         ++ newline
         ++ "process'" ++ processName ++ " (stageNum,waitTime) = do"
         ++ newline ++ tab
         ++ "curTime@(Control'Time realTime deltaTime) <- lift get"
         ++ newline ++ tab
         ++ "(Entity'State portsIn state _ _) <- get"
         ++ newline ++ tab
         ++ "let"
         ++ processContentsStr
         ++ newline ++ tab ++ tab
         ++ "stageList = ["
         ++ ( concat
            $ intersperse ","
            $ map (\s -> "stage" ++ show s ++ "check") [0..numStages]
            )
         ++ "]"
         ++ newline ++ tab ++ tab
         ++ "stageRun = stageList !! stageNum"
         ++ newline ++ tab
         ++ "lift stageRun"
       processInitialStr =
         let constName = "process'initial'" ++ processName
         in constName ++ " :: (Int,STD.STANDARD.Type'ANON'TIME)"
            ++ newline
            ++ constName ++ " = " ++ initialStateStr
   in liftIO $ appendFile fileName $
         newline
         ++ processFuncStr ++ newline
         ++ processInitialStr ++ newline

convertStatement :: SequentialStatement -> NetlistName -> [(NetlistName,String)] -> String -> Maybe Int -> [String]
convertStatement (WaitStatement sensitivity condition timeout) unitName nestedNames processName _ =
   let convertSensitivity :: (SignalType,String) -> String
       convertSensitivity (signalType,signalName) =
         "("
         ++ "control'signal'event $ " -- ?? event or active
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
       conditionStr = "((" ++ convertCalculation nestedNames processName False condition ++ ") == STD.STANDARD.Type'ANON'BOOLEAN'Iden'TRUE)"
       timeoutStr = case timeout of
                     Just _ -> "(waitTime == realTime)"
                     Nothing -> "(STD.STANDARD.mkType'ANON'TIME " ++ show (maxBound :: Int64) ++ " == realTime)"
   in ["if (" ++ sensitivityStr ++ " && " ++ conditionStr ++ ") || " ++ timeoutStr]
convertStatement (AssertStatement condition report severity) unitName nestedNames processName _ =
   let unitNameStr = show unitName
       conditionStr = convertCalculation nestedNames processName False condition
       messageStr = "$ STD.STANDARD.mkType'ANON'STRING []" -- ?? Temp until arrays parsed
       severityStr = convertCalculation nestedNames processName False severity
   in [ "when ((" ++ conditionStr ++ ") == STD.STANDARD.Type'ANON'BOOLEAN'Iden'TRUE) $"
      , tab ++ "case " ++ severityStr ++ " of"
      , tab ++ tab ++ "STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'NOTE -> control'assertNote \"" ++ unitNameStr ++ "\" " ++ messageStr
      , tab ++ tab ++ "STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'WARNING -> control'assertWarning \"" ++ unitNameStr ++ "\" " ++ messageStr
      , tab ++ tab ++ "STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'ERROR -> control'assertError \"" ++ unitNameStr ++ "\" " ++ messageStr
      , tab ++ tab ++ "STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'FAILURE -> control'assertFailure \"" ++ unitNameStr ++ "\" " ++ messageStr
      ]
convertStatement (SignalAssignStatement signalName signalType waveforms) unitName nestedNames processName (Just uniqID) =
   let signalNameStr =
         case signalType of
            PortOut -> "ports'out'"
            InternalSignal -> "signal'"
         ++ signalName
       writeSignalStr =
         "entity'"
         ++ case signalType of
               PortOut -> "portsOut = portsOut { "
               InternalSignal -> "state = signals { "
         ++ signalNameStr
         ++ " ="
       readSignalStr =
         signalNameStr
         ++ case signalType of
               PortOut -> " portsOut"
               InternalSignal -> " state"
       modifyFuncStr =
         ["let modify'" ++ show uniqID ++ " entState@(Entity'State portsIn signals portsOut _) ="
         , tab ++ "entState"
         , tab ++ tab ++ "{" ++ writeSignalStr
         , tab ++ tab ++ tab ++ "control'updateSignal'inertial"
         , tab ++ tab ++ tab ++ tab ++ "curTime"
         , tab ++ tab ++ tab ++ tab ++ "[ "
         ]
         ++ ( map ((++) (newline ++ tab ++ tab ++ tab ++ tab ++ ", "))
            $ map printWaveform
            $ waveforms
            )
         ++ [ tab ++ tab ++ tab ++ tab ++ "]"
            , tab ++ tab ++ tab ++ tab ++ "(" ++ readSignalStr ++ ")"
            ]
   in modifyFuncStr
      ++ ["modify modify'" ++ show uniqID]
   where printWaveform (ValueWaveform valueExp timeExp) =
            "(" ++ convertCalculation nestedNames processName False timeExp
            ++ "," ++ convertCalculation nestedNames processName False valueExp
            ++ ")"
         --printWaveform (NullWaveform timeExp) = 
convertStatement NullStatement _ _ _ _ = ["return ()"]

convertCalculation :: [(NetlistName,String)] -> String -> Bool -> Calculation -> String
convertCalculation a1 a2 _ (Calc_Value value typeData) =
   case (value,typeData) of
      (Value_Enum _ enum,Type_Type enumTypeName _) ->
         showEnum (printTypeName a1 a2 enumTypeName) enum
      (Value_Int int,Type_UniversalInt) -> show int
      (Value_Int int,Type_Type typeName _) ->
         printTypeConstructor a1 a2 typeName ++ " " ++ show int
      (Value_Float flt,Type_UniversalReal) ->
         (showFFloat Nothing flt) ""
      (Value_Float flt,Type_Type typeName _) -> 
         printTypeConstructor a1 a2 typeName ++ " " ++ (showFFloat Nothing flt) ""
      (Value_Physical int,Type_Type typeName _) ->
         printTypeConstructor a1 a2 typeName ++ " " ++ show int
      --Value_Array
--convertCalculation (Calc_FunctionCall ) =
convertCalculation nestedNames processName _ (Calc_Const (unitName,constName))
   | elem (unitName,constName) nestedNames = show unitName ++ ".constant'" ++ processName ++ "'" ++ constName
   | otherwise = show unitName ++ "." ++ constName
convertCalculation a1 a2 a3 (Calc_BuiltinNegate calc (Type_Type typeName _)) =
   (printUnaryFunctionName a1 a2 "MINUS" typeName) ++ " (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNegate calc Type_UniversalInt) = "negate (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNegate calc Type_UniversalReal) = "negate (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_Type typeName1 _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "PLUS" typeName1 typeName1 typeName1)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") + (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") + (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_Type typeFullName@(typePackage,typeName) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "PLUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_Type typeFullName@(typePackage,typeName) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "PLUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_UniversalInt) (calc2,Type_Type typeFullName@(typePackage,typeName) _)) =
   (printFunctionName a1 a2 "PLUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_UniversalReal) (calc2,Type_Type typeFullName@(typePackage,typeName) _)) =
   (printFunctionName a1 a2 "PLUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_Type typeName1 _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "MINUS" typeName1 typeName1 typeName1)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") - (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") - (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_Type typeFullName@(typePackage,typeName) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "MINUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_Type typeFullName@(typePackage,typeName) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "MINUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_UniversalInt) (calc2,Type_Type typeFullName@(typePackage,typeName) _)) =
   (printFunctionName a1 a2 "MINUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_UniversalReal) (calc2,Type_Type typeFullName@(typePackage,typeName) _)) =
   (printFunctionName a1 a2 "MINUS" typeFullName typeFullName typeFullName)
   ++ " (" ++ show typePackage ++ ".mk'Type'" ++ typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
--convertCalculation a1 a2 a3 (Calc_BuiltinConcat
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName1 _) (calc2,Type_Type typeName2 _))
   | typeName1 == typeName2 = (printFunctionName a1 a2 "MULT" typeName1 typeName1 typeName1)
                              ++ " (" ++ convertCalculation a1 a2 a3 calc1
                              ++ ") (" ++ convertCalculation a1 a2 a3 calc2
                              ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") * (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") * (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName1 (PhysicalType _ _)) (calc2,Type_Type typeName2 _)) =
   (printFunctionName a1 a2 "MULT" typeName1 typeName2 typeName1)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName (PhysicalType _ _)) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "MULT" typeName (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (STD.STANDARD.mk'Type'ANON'INTEGER $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName (PhysicalType _ _)) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "MULT" typeName (NetlistName "STD" "STANDARD","ANON'REAL") typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (STD.STANDARD.mk'Type'ANON'REAL $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName1 _) (calc2,Type_Type typeName2 (PhysicalType _ _))) =
   (printFunctionName a1 a2 "MULT" typeName1 typeName2 typeName2)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_UniversalInt) (calc2,Type_Type typeName (PhysicalType _ _))) =
   (printFunctionName a1 a2 "MULT" (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName typeName)
   ++ " (STD.STANDARD.mk'Type'ANON'INTEGER $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_UniversalReal) (calc2,Type_Type typeName (PhysicalType _ _))) =
   (printFunctionName a1 a2 "MULT" (NetlistName "STD" "STANDARD","ANON'REAL") typeName typeName)
   ++ " (STD.STANDARD.mk'Type'ANON'REAL $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
-- ?? Bultin div
convertCalculation a1 a2 a3 (Calc_BuiltinMod calc1 calc2 (Type_Type typeName _)) =
   (printFunctionName a1 a2 "MOD" typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMod calc1 calc2 Type_UniversalInt) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") `mod` (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinRem calc1 calc2 (Type_Type typeName _)) =
   (printFunctionName a1 a2 "REM" typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinRem calc1 calc2 Type_UniversalInt) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") `rem` (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinExp (calc1,Type_Type typeName _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "EXP" typeName (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinExp (calc1,Type_Type typeName _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "EXP" typeName (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (STD.STANDARD.mkType'ANON'INTEGER $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
-- ?? Need universal int/real lhs expressions for exponentials
convertCalculation a1 a2 a3 (Calc_BuiltinAbs calc (Type_Type typeName _)) =
   (printUnaryFunctionName a1 a2 "ABS" typeName) ++ " (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinAbs calc Type_UniversalInt) = "abs (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinAbs calc Type_UniversalReal) = "abs (" ++ convertCalculation a1 a2 a3 calc ++ ")"
-- ?? Other expressions
convertCalculation a1 a2 a3 (Calc_ImplicitTypeConversion typeName calc) = printTypeConstructor a1 a2 typeName ++ "(" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_SubtypeResult subtypeName calc) = printTypeConstructor a1 a2 subtypeName ++ "(" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_ExplicitTypeConversion (typeName1,IntegerType) (calc,Type_Type typeName2 IntegerType)) =
   printTypeConstructor a1 a2 typeName1 ++ "(" ++ printTypeExtractor a1 a2 typeName2 ++ " $ " ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_ExplicitTypeConversion (typeName1,FloatingType) (calc,Type_Type typeName2 FloatingType)) =
   printTypeConstructor a1 a2 typeName1 ++ "(" ++ printTypeExtractor a1 a2 typeName2 ++ " $ " ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_ExplicitTypeConversion (typeName1,IntegerType) (calc,Type_Type typeName2 FloatingType)) =
   printTypeConstructor a1 a2 typeName1 ++ " $ round (" ++ printTypeExtractor a1 a2 typeName2 ++ " $ " ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_ExplicitTypeConversion (typeName1,FloatingType) (calc,Type_Type typeName2 IntegerType)) =
   printTypeConstructor a1 a2 typeName1 ++ " $ fromInteger (" ++ printTypeExtractor a1 a2 typeName2 ++ " $ " ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_ExplicitTypeConversion (typeName1,IntegerType) (calc,Type_UniversalReal)) =
   printTypeConstructor a1 a2 typeName1 ++ " $ round (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_ExplicitTypeConversion (typeName1,FloatingType) (calc,Type_UniversalInt)) =
   printTypeConstructor a1 a2 typeName1 ++ " $ fromInteger (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation _ _ isInitial (Calc_Signal signalName signalType) = "control'readSignal (" ++ printSignalName signalName signalType isInitial ++ ")"
convertCalculation _ _ isInitial (Calc_SignalEvent signalName signalType)
   | isInitial = "STD.STANDARD.Type'ANON'BOOLEAN'Iden'FALSE"
   | otherwise = "control'signal'event (" ++ printSignalName signalName signalType False ++ ")"

printUnaryFunctionName :: [(NetlistName,String)] -> String -> String -> (NetlistName,String) -> String
printUnaryFunctionName nestedNames processName opName typeName =
   let typeFullName = printFunctionType nestedNames processName typeName
   in "function'op'" ++ opName ++ "'in'" ++ typeFullName ++ "'out'" ++ typeFullName

printFunctionName :: [(NetlistName,String)] -> String -> String -> (NetlistName,String) -> (NetlistName,String) -> (NetlistName,String) -> String
printFunctionName nestedNames processName opName typeName1 typeName2 typeName3 =
   let printFuncSeg = printFunctionType nestedNames processName
   in "function'op'" ++ opName
      ++ "'in'" ++ printFuncSeg typeName1
      ++ "'_'" ++ printFuncSeg typeName2
      ++ "'out'" ++ printFuncSeg typeName3

printFunctionType :: [(NetlistName,String)] -> String -> (NetlistName,String) -> String
printFunctionType nestedNames processName name@(NetlistName typePackage typeUnit,typeName)
   | elem name nestedNames = typePackage ++ "'" ++ typeUnit ++ "'Type'" ++ processName ++ "'" ++ typeName
   | otherwise = typePackage ++ "'" ++ typeUnit ++ "'Type" ++ typeName

printTypeName :: [(NetlistName,String)] -> String -> (NetlistName,String) -> String
printTypeName nestedNames processName name@(NetlistName typePackage typeUnit,typeName)
   | elem name nestedNames = typePackage ++ "." ++ typeUnit ++ ".Type'" ++ processName ++ "'" ++ typeName
   | otherwise = typePackage ++ "." ++ typeUnit ++ ".Type'" ++ typeName

printTypeConstructor :: [(NetlistName,String)] -> String -> (NetlistName,String) -> String
printTypeConstructor nestedNames processName name@(NetlistName typePackage typeUnit,typeName)
   | elem name nestedNames = typePackage ++ "." ++ typeUnit ++ ".mkType'" ++ processName ++ "'" ++ typeName
   | otherwise = typePackage ++ "." ++ typeUnit ++ ".mkType'" ++ typeName

printTypeExtractor :: [(NetlistName,String)] -> String -> (NetlistName,String) -> String
printTypeExtractor nestedNames processName name@(NetlistName typePackage typeUnit,typeName)
   | elem name nestedNames = typePackage ++ "." ++ typeUnit ++ ".extractType'" ++ processName ++ "'" ++ typeName
   | otherwise = typePackage ++ "." ++ typeUnit ++ ".extractType'" ++ typeName

printSignalName :: (Maybe NetlistName,String) -> SignalType -> Bool -> String
printSignalName (Just signalPackage,signalName) InternalSignal _ = show signalPackage ++ ".signal'" ++ signalName
printSignalName (Nothing,signalName) InternalSignal False = "signal'" ++ signalName ++ " signals"
printSignalName (Nothing,signalName) InternalSignal True = "signal'" ++ signalName ++ " initial'STATE"
printSignalName (Nothing,signalName) PortIn False = "ports'in'" ++ signalName ++ " portsIn"
printSignalName (Nothing,signalName) PortIn True = "ports'in'" ++ signalName ++ " initial'PORTS'IN"
