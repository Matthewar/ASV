module Sim.Output.Processes
   ( outputProcesses
   ) where

import qualified Data.Map.Strict as MapS
import Data.List (intersperse)
import Data.Int (Int64)
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
import Sim.Output.Types (outputTypes)
import Sim.Output.Subtypes (outputSubtypes)
import Sim.Output.Constants (outputConstants)
import Sim.Output.Names (showEnum)
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
   let processStr =
         "process'" ++ processName ++ " :: [ProcessStatement PORTS'IN STATE PORTS'OUT]"
         ++ newline
         ++ "process'" ++ processName ++ " = cycle $"
         ++ newline ++ tab
         ++ "[ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ ", ")
            $ map
               (\s ->
                  concat
                  $ intersperse (newline ++ tab ++ tab)
                  $ convertStatement s unitName nestedNames processName
               )
            $ statements
            )
         -- ++ newline
         -- ++ "   , Control'End\n\ -- ?? Need to deal with infinite loops within a process, if there are no wait statements
         --    \   ]"
         ++ newline ++ tab
         ++ "]"
   in liftIO $ appendFile fileName $ newline ++ processStr ++ newline

convertStatement :: SequentialStatement -> NetlistName -> [(NetlistName,String)] -> String -> [String]
convertStatement (WaitStatement sensitivity condition timeout) unitName nestedNames processName =
   let convertSensitivity :: (SignalType,String) -> String
       convertSensitivity (signalType,signalName) =
         "("
         ++ "control'signal'event $ " -- ?? event or active
         ++ case signalType of
               InternalSignal -> "signal'" ++ signalName ++ " $ signals"
               PortIn -> "ports'in'" ++ signalName ++ " $ portsIn"
         ++ ")"
       sensitivityStr = if null sensitivity
                           then "True"
                           else
                              "("
                              ++ (concat $ intersperse " || " $ map convertSensitivity sensitivity)
                              ++ ")"
       conditionStr = "(" ++ convertCalculation nestedNames processName False condition ++ ")" -- ?? Remove initial bool argument
   in [ "Control'Wait"
      , tab ++ "(\\(Entity'State portsIn signals _) -> " ++ sensitivityStr ++ ")"
      , tab ++ "(\\(Entity'State portsIn signals _) (Control'Time realTime _) -> " ++ conditionStr ++ ")"
      , tab ++ case timeout of
                  Just timeoutExp -> "(Right (\\(Entity'State portsIn signals _) (Control'Time realTime _) -> Just (" ++ convertCalculation nestedNames processName False timeoutExp ++ ")))"
                  Nothing -> "(Right (\\_ _ -> Nothing))"
      ]
convertStatement (AssertStatement condition report severity) unitName nestedNames processName =
   let unitNameStr = show unitName
       conditionStr = convertCalculation nestedNames processName False condition
       --messageStr = convertCalculation nestedNames processName False report
       messageStr = "STD.STANDARD.mkType'ANON'STRING []" -- ?? Temp until arrays parsed
       severityStr = convertCalculation nestedNames processName False severity
   in [ "Control'Assert"
      , tab ++ "\"" ++ unitNameStr ++ "\""
      , tab ++ "(\\(Entity'State portsIn signals _) (Control'Time realTime _) -> " ++ conditionStr ++ ")"
      , tab ++ "(\\(Entity'State portsIn signals _) (Control'Time realTime _) -> " ++ messageStr ++ ")"
      , tab ++ "(\\(Entity'State portsIn signals _) (Control'Time realTime _) -> " ++ severityStr ++ ")"
      ]
convertStatement (SignalAssignStatement signalName signalType waveforms) unitName nestedNames processName =
   let signalNameStr =
         case signalType of
            PortOut -> "ports'out'"
            InternalSignal -> "signal'"
         ++ signalName
       readSignalStr =
         signalNameStr
         ++ case signalType of
               PortOut -> " portsOut"
               InternalSignal -> " signals"
   in [ "Control'Statement (\\state@(Entity'State portsIn signals portsOut) currentTime@(Control'Time realTime _) ->"
      , tab ++ "state { entity'" ++ case signalType of
                                       PortOut -> "portsOut = portsOut"
                                       InternalSignal -> "state = signals"
      , tab ++ tab ++ "{ " ++ signalNameStr ++ " ="
      , tab ++ tab ++ tab ++ "control'updateSignal'inertial"
      , tab ++ tab ++ tab ++ tab ++ "currentTime"
      , tab ++ tab ++ tab ++ tab ++ "[ " ++ (printWaveform $ head waveforms)
      ]
      ++ ( map ((++) (tab ++ tab ++ tab ++ tab ++ ", "))
         $ map printWaveform
         $ tail
         $ waveforms
         )
      ++ [ tab ++ tab ++ tab ++ tab ++ "]"
         , tab ++ tab ++ tab ++ tab ++ "(" ++ readSignalStr ++ ")"
         , tab ++ tab ++ "}"
         , tab ++ "}"
         , ")"
         ]
   where printWaveform (ValueWaveform valueExp timeExp) =
            "(" ++ convertCalculation nestedNames processName False timeExp
            ++ "," ++ convertCalculation nestedNames processName False valueExp
            ++ ")"
         --printWaveform (NullWaveform timeExp) = 
convertStatement (IfStatement elsifs elses) unitName nestedNames processName =
   convertIfs elsifs
   where convertIfs ((condition,statements):otherIfs) =
            [ "Control'If"
            , tab ++ "(\\(Entity'State portsIn signals _) (Control'Time realTime _) -> " ++ convertCalculation nestedNames processName False condition ++ ")"
            ]
            ++ convertIfStatements statements
            ++ [tab ++ "]"]
            ++ case otherIfs of
                  [] -> convertIfStatements elses
                        ++ [tab ++ "]"]
                  _ -> map ((++) tab) (convertIfs otherIfs)
         convertIfStatements = (map ((++) tab)) . concat . printListStatement . (map (\s -> convertStatement s unitName nestedNames processName))
         printListStatement (statement:statements) = (printStatements '[' statement:map (printStatements ',') statements)
         printListStatement [] = [["["]]
         printStatements char (statement:statements) = (([char,' '] ++ statement):map ((++) "  ") statements)
convertStatement NullStatement _ _ _ = ["Control'Statement (\\currentState _ -> currentState)"]

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
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_Type typeName1@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "PLUS" funcPackage typeName1 typeName1 typeName1)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") + (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") + (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "PLUS" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "PLUS" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "PLUS" funcPackage typeName typeName typeName)
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSum (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "PLUS" funcPackage typeName typeName typeName)
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_Type typeName1@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "MINUS" funcPackage typeName1 typeName1 typeName1)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") - (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") - (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "MINUS" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "MINUS" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "MINUS" funcPackage typeName typeName typeName)
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinSubtract (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "MINUS" funcPackage typeName typeName typeName)
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
--convertCalculation a1 a2 a3 (Calc_BuiltinConcat
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName1@(funcPackage,_) _) (calc2,Type_Type typeName2 _))
   | typeName1 == typeName2 = (printFunctionName a1 a2 "MULT" funcPackage typeName1 typeName1 typeName1)
                              ++ " (" ++ convertCalculation a1 a2 a3 calc1
                              ++ ") (" ++ convertCalculation a1 a2 a3 calc2
                              ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") * (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") * (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName1@(funcPackage,_) (PhysicalType _ _)) (calc2,Type_Type typeName2 _)) =
   (printFunctionName a1 a2 "MULT" funcPackage typeName1 typeName2 typeName1)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName@(funcPackage,_) (PhysicalType _ _)) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "MULT" funcPackage typeName (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (STD.STANDARD.mk'Type'ANON'INTEGER $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName@(funcPackage,_) (PhysicalType _ _)) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "MULT" funcPackage typeName (NetlistName "STD" "STANDARD","ANON'REAL") typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (STD.STANDARD.mk'Type'ANON'REAL $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_Type typeName1 _) (calc2,Type_Type typeName2@(funcPackage,_) (PhysicalType _ _))) =
   (printFunctionName a1 a2 "MULT" funcPackage typeName1 typeName2 typeName2)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) (PhysicalType _ _))) =
   (printFunctionName a1 a2 "MULT" funcPackage (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName typeName)
   ++ " (STD.STANDARD.mk'Type'ANON'INTEGER $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMult (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) (PhysicalType _ _))) =
   (printFunctionName a1 a2 "MULT" funcPackage (NetlistName "STD" "STANDARD","ANON'REAL") typeName typeName)
   ++ " (STD.STANDARD.mk'Type'ANON'REAL $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
-- ?? Bultin div
convertCalculation a1 a2 a3 (Calc_BuiltinMod calc1 calc2 (Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "MOD" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinMod calc1 calc2 Type_UniversalInt) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") `mod` (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinRem calc1 calc2 (Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "REM" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinRem calc1 calc2 Type_UniversalInt) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") `rem` (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinExp (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "EXP" funcPackage typeName (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinExp (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "EXP" funcPackage typeName (NetlistName "STD" "STANDARD","ANON'INTEGER") typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (STD.STANDARD.mkType'ANON'INTEGER $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
-- ?? Need universal int/real lhs expressions for exponentials
convertCalculation a1 a2 a3 (Calc_BuiltinAbs calc (Type_Type typeName _)) =
   (printUnaryFunctionName a1 a2 "ABS" typeName) ++ " (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinAbs calc Type_UniversalInt) = "abs (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinAbs calc Type_UniversalReal) = "abs (" ++ convertCalculation a1 a2 a3 calc ++ ")"
-- ?? Only has two enumerated types supported, change from Type_Type
convertCalculation a1 a2 a3 (Calc_BuiltinNot calc (Type_Type typeName _)) =
   (printUnaryFunctionName a1 a2 "NOT" typeName) ++ " (" ++ convertCalculation a1 a2 a3 calc ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "EQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "EQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinEqual (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "EQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "EQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinEqual (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "EQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinEqual (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") == (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinEqual (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") == (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNotEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "NEQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNotEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "NEQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNotEqual (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "NEQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNotEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "NEQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNotEqual (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "NEQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNotEqual (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") /= (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNotEqual (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") /= (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThan (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "LESSTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThan (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "LESSTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThan (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "LESSTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThan (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "LESSTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThan (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "LESSTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThan (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") < (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThan (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") < (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThanOrEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "LESSTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThanOrEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "LESSTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThanOrEqual (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "LESSTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThanOrEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "LESSTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThanOrEqual (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "LESSTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThanOrEqual (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") <= (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinLessThanOrEqual (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") <= (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThan (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "GREATERTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThan (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "GREATERTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThan (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "GREATERTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThan (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "GREATERTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThan (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "GREATERTHAN" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThan (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") > (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThan (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") > (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThanOrEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_Type _ _)) =
   (printFunctionName a1 a2 "GREATERTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThanOrEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalInt)) =
   (printFunctionName a1 a2 "GREATERTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThanOrEqual (calc1,Type_UniversalInt) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "GREATERTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThanOrEqual (calc1,Type_Type typeName@(funcPackage,_) _) (calc2,Type_UniversalReal)) =
   (printFunctionName a1 a2 "GREATERTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThanOrEqual (calc1,Type_UniversalReal) (calc2,Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "GREATERTHANOREQUAL" funcPackage typeName typeName (NetlistName "STD" "STANDARD","ANON'BOOLEAN"))
   ++ " (" ++ printTypeConstructor a1 a2 typeName ++ " $ " ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThanOrEqual (calc1,Type_UniversalInt) (calc2,Type_UniversalInt)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") >= (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinGreaterThanOrEqual (calc1,Type_UniversalReal) (calc2,Type_UniversalReal)) = "(" ++ convertCalculation a1 a2 a3 calc1 ++ ") >= (" ++ convertCalculation a1 a2 a3 calc2 ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinAnd calc1 calc2 (Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "AND" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinOr calc1 calc2 (Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "OR" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinXor calc1 calc2 (Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "XOR" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNand calc1 calc2 (Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "NAND" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
convertCalculation a1 a2 a3 (Calc_BuiltinNor calc1 calc2 (Type_Type typeName@(funcPackage,_) _)) =
   (printFunctionName a1 a2 "NOR" funcPackage typeName typeName typeName)
   ++ " (" ++ convertCalculation a1 a2 a3 calc1
   ++ ") (" ++ convertCalculation a1 a2 a3 calc2
   ++ ")"
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
   | otherwise = "STD.STANDARD.boolToBoolean (control'signal'event (" ++ printSignalName signalName signalType False ++ "))"

printUnaryFunctionName :: [(NetlistName,String)] -> String -> String -> (NetlistName,String) -> String
printUnaryFunctionName nestedNames processName opName typeName@(funcPackage,_) =
   let typeFullName = printFunctionType nestedNames processName typeName
   in show funcPackage ++ ".function'op'" ++ opName ++ "'in'" ++ typeFullName ++ "'out'" ++ typeFullName

printFunctionName :: [(NetlistName,String)] -> String -> String -> NetlistName -> (NetlistName,String) -> (NetlistName,String) -> (NetlistName,String) -> String
printFunctionName nestedNames processName opName funcPackage typeName1 typeName2 typeName3 =
   let printFuncSeg = printFunctionType nestedNames processName
   in show funcPackage ++ ".function'op'" ++ opName
      ++ "'in'" ++ printFuncSeg typeName1
      ++ "'_'" ++ printFuncSeg typeName2
      ++ "'out'" ++ printFuncSeg typeName3

printFunctionType :: [(NetlistName,String)] -> String -> (NetlistName,String) -> String
printFunctionType nestedNames processName name@(NetlistName typePackage typeUnit,typeName)
   | elem name nestedNames = typePackage ++ "'" ++ typeUnit ++ "'Type'" ++ processName ++ "'" ++ typeName
   | otherwise = typePackage ++ "'" ++ typeUnit ++ "'Type'" ++ typeName

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
