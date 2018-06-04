module Sim.Output.Components
   ( outputComponentControl
   , outputGenerics
   ) where

import System.FilePath ((</>))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import Numeric (showFFloat)
import qualified Data.Map.Strict as MapS
import Data.List (intersperse)

import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Generic(..)
         , Port(..)
         , Signal(..)
         , Mode(..)
         , Value(..)
         , Subtype(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , SignalStore
         )
import Sim.Output.Imports (outputImports)
import Sim.Output.Cabal (outputCabalModule)
import Sim.Output.Names
         ( printComponentActualName
         , printComponentSafeName
         , showEnum
         )
import Manager.Types.Error (ConverterError)

newline = "\n"
tab = "   "

outputComponentControl :: FilePath -> String -> [String] -> [Generic] -> [Port] -> SignalStore -> [String] -> ExceptT ConverterError IO ()
outputComponentControl fileName baseName componentName generics ports signals processNames =
   let genericTypeStr =
         let printGeneric (Generic genericName (subtypePackage,subtypeName) _ _) =
               "--GENERICS.generic'" ++ genericName ++ " :: " ++ show subtypePackage ++ "." ++ subtypeName
         in "--GENERICS:"
            ++ (concat $ map (\s -> newline ++ printGeneric s) generics)
       portsInTypeStr =
         let printPort (Port portName _ (subtypePackage,subtypeName) _ _) =
               "ports'in'" ++ portName ++ " :: Control'Signal " ++ show subtypePackage ++ ".Type'" ++ subtypeName
         in "data PORTS'IN ="
            ++ newline ++ tab
            ++ "PORTS'IN"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printPort $ filter (\(Port _ mode _ _ _) -> mode == Mode_In) ports)
            ++ newline ++ tab ++ tab
            ++ "}"
       portsInInitialStr =
         let printPort (Port portName _ (subtypePackage,subtypeName) subtype value) =
               let typeBuild = case subtype of
                                 EnumerationSubtype _ (typePackage,typeName) _ _ -> show typePackage ++ ".Type'" ++ typeName
                                 IntegerSubtype _ (typePackage,typeName) _ -> show typePackage ++ ".mkType'" ++ typeName
                                 FloatingSubtype _ (typePackage,typeName) _ -> show typePackage ++ ".mkType'" ++ typeName
                                 PhysicalSubtype _ (typePackage,typeName) _ _ _ -> show typePackage ++ ".mkType'" ++ typeName
                                 --ArraySubtype _ (typePackage,typeName) _ _ -> show typePackage ++ "." ++ typeName
                   subtypeBuild = show subtypePackage ++ ".mkType'" ++ subtypeName
                   valueStr = case value of
                                 Value_Enum _ enum -> showEnum typeBuild enum
                                 Value_Int int -> typeBuild ++ " " ++ show int
                                 Value_Float flt -> typeBuild ++ " " ++ (showFFloat Nothing flt) ""
                                 Value_Physical phys -> typeBuild ++ " " ++ show phys
                                 --Value_Array
               in "ports'in'" ++ portName ++ " = Control'Signal [(initialTime," ++ subtypeBuild ++ " $ " ++ valueStr ++ ")] False False"
         in "initial'PORTS'IN ="
            ++ newline ++ tab
            ++ "PORTS'IN"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printPort $ filter (\(Port _ mode _ _ _) -> mode == Mode_In) ports)
            ++ newline ++ tab ++ tab
            ++ "}"
       portsOutTypeStr =
         let printPort (Port portName _ (subtypePackage,subtypeName) _ _) =
               "ports'out'" ++ portName ++ " :: Control'Signal " ++ show subtypePackage ++ "." ++ subtypeName
         in "data PORTS'OUT ="
            ++ newline ++ tab
            ++ "PORTS'OUT"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printPort $ filter (\(Port _ mode _ _ _) -> mode == Mode_Out) ports)
            ++ newline ++ tab ++ tab
            ++ "}"
       portsOutInitialStr =
         let printPort (Port portName _ (subtypePackage,subtypeName) subtype value) =
               let typeBuild = case subtype of
                                 EnumerationSubtype _ (typePackage,typeName) _ _ -> show typePackage ++ ".Type'" ++ typeName
                                 IntegerSubtype _ (typePackage,typeName) _ -> show typePackage ++ ".mkType'" ++ typeName
                                 FloatingSubtype _ (typePackage,typeName) _ -> show typePackage ++ ".mkType'" ++ typeName
                                 PhysicalSubtype _ (typePackage,typeName) _ _ _ -> show typePackage ++ ".mkType'" ++ typeName
                                 --ArraySubtype _ (typePackage,typeName) _ _ -> show typePackage ++ "." ++ typeName
                   subtypeBuild = show subtypePackage ++ ".mkType'" ++ subtypeName
                   valueStr = case value of
                                 Value_Enum _ enum -> showEnum typeBuild enum
                                 Value_Int int -> typeBuild ++ " " ++ show int
                                 Value_Float flt -> typeBuild ++ " " ++ (showFFloat Nothing flt) ""
                                 Value_Physical phys -> typeBuild ++ " " ++ show phys
                                 --Value_Array
               in "ports'out'" ++ portName ++ " = Control'Signal [(initialTime," ++ subtypeBuild ++ " $ " ++ valueStr ++ ")] False False"
         in "initial'PORTS'OUT ="
            ++ newline ++ tab
            ++ "PORTS'OUT"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printPort $ filter (\(Port _ mode _ _ _) -> mode == Mode_Out) ports)
            ++ newline ++ tab ++ tab
            ++ "}"
       signalTypeStr =
         let printSignal (signalName,Signal (typePackage,typeName) _ _) =
               "signal'" ++ signalName ++ " :: Control'Signal " ++ show typePackage ++ ".Type'" ++ typeName
         in "data STATE ="
            ++ newline ++ tab
            ++ "STATE"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printSignal $ MapS.toList signals)
            ++ newline ++ tab ++ tab
            ++ "}"
       signalInitialStr =
         let printSignal (signalName,Signal (subtypePackage,subtypeName) subtype value) =
               let typeBuild = case subtype of
                                 EnumerationSubtype _ (typePackage,typeName) _ _ -> show typePackage ++ ".Type'" ++ typeName
                                 IntegerSubtype _ (typePackage,typeName) _ -> show typePackage ++ ".mkType'" ++ typeName
                                 FloatingSubtype _ (typePackage,typeName) _ -> show typePackage ++ ".mkType'" ++ typeName
                                 PhysicalSubtype _ (typePackage,typeName) _ _ _ -> show typePackage ++ ".mkType'" ++ typeName
                                 --ArraySubtype _ (typePackage,typeName) _ _ -> show typePackage ++ "." ++ typeName
                   subtypeBuild = show subtypePackage ++ ".mkType'" ++ subtypeName
                   valueStr = case value of
                                 Value_Enum _ enum -> showEnum typeBuild enum
                                 Value_Int int -> typeBuild ++ " " ++ show int
                                 Value_Float flt -> typeBuild ++ " " ++ (showFFloat Nothing flt) ""
                                 Value_Physical phys -> typeBuild ++ " " ++ show phys
                                 --Value_Array
               in "signal'" ++ signalName ++ " = Control'Signal [(initialTime," ++ subtypeBuild ++ " $ " ++ valueStr ++ ")] False False"
         in "initial'STATE ="
            ++ newline ++ tab
            ++ "STATE"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printSignal $ MapS.toList signals)
            ++ newline ++ tab ++ tab
            ++ "}"
       processTypeStr =
         "data PROCESSES ="
         ++ newline ++ tab
         ++ "PROCESSES"
         ++ newline ++ tab ++ tab
         ++ "{ "
         ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map (\name -> "processes'" ++ name ++ " :: (Int,STD.STANDARD.Type'ANON'TIME)") processNames)
         ++ newline ++ tab ++ tab
         ++ "}"
       processInitialStr =
         "initial'PROCESSES = "
         ++ newline ++ tab
         ++ "PROCESSES"
         ++ newline ++ tab ++ tab
         ++ "{ "
         ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map (\name -> "processes'" ++ name ++ " = process'initial'" ++ name) processNames)
         ++ newline ++ tab ++ tab
         ++ "}"
       entityStateInitialStr =
         "initialStack ="
         ++ newline ++ tab
         ++ "Entity'State"
         ++ newline ++ tab ++ tab
         ++ "initial'PORTS'IN"
         ++ newline ++ tab ++ tab
         ++ "initial'STATE"
         ++ newline ++ tab ++ tab
         ++ "initial'PORTS'OUT"
         ++ newline ++ tab ++ tab
         ++ "initial'PROCESSES"
       collectTimingStr =
         "allTimes :: Entity'" ++ baseName ++ "'Stack (Maybe STD.STANDARD.Type'ANON'TIME)"
         ++ newline
         ++ "allTimes = do"
         ++ newline ++ tab
         ++ "signals <- gets entity'state"
         ++ newline ++ tab
         ++ "processes <- gets entity'processes"
         ++ newline ++ tab
         ++ "let getSignalTime (_:(time,_):_) = Just $ time'Real time"
         ++ newline ++ tab ++ tab
         ++ " getSignalTime _ = Nothing"
         ++ newline ++ tab ++ tab
         ++ " signalData ="
         ++ newline ++ tab ++ tab ++ tab
         ++ "[ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map (\signalName -> "signal'" ++ signalName ++ " signals")
            $ MapS.keys signals
            )
         ++ newline ++ tab ++ tab ++ tab
         ++ "]"
         ++ newline ++ tab ++ tab
         ++ " signalTimes = map fromJust $ filter isJust $ map getSignalTime $ map control'signal'transactions signalData"
         ++ newline ++ tab ++ tab
         ++ " getProcessTime (_,time) = time"
         ++ newline ++ tab ++ tab
         ++ " processData ="
         ++ newline ++ tab ++ tab ++ tab
         ++ "[ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map (\processName -> "processes'" ++ processName ++ " processes")
            $ processNames
            )
         ++ newline ++ tab ++ tab ++ tab
         ++ "]"
         ++ newline ++ tab ++ tab
         ++ " processTimes = map getProcessTime processData"
         ++ newline ++ tab
         ++ "case signalTimes ++ processTimes of"
         ++ newline ++ tab ++ tab
         ++ "[] -> return Nothing"
         ++ newline ++ tab ++ tab
         ++ "other -> return $ Just $ minimum other"
       checkActiveStr =
         "anyReady :: Entity'" ++ baseName ++ "'Stack Bool"
         ++ newline
         ++ "anyReady = do"
         ++ newline ++ tab
         ++ "(Control'Time realTime deltaTime) <- lift get"
         ++ newline ++ tab
         ++ "signals <- gets entity'state"
         ++ newline ++ tab
         ++ "portsIn <- gets entity'portsIn"
         ++ newline ++ tab
         ++ "let getSignalTime (_:nextTime:_) = nextTime == Control'Time realTime (deltaTime + 1)"
         ++ newline ++ tab ++ tab
         ++ " getSignalTime _ = False"
         ++ newline ++ tab ++ tab
         ++ " signalFuncs ="
         ++ newline ++ tab ++ tab ++ tab
         ++ "[ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map (\signalName -> "signal'" ++ signalName)
            $ MapS.keys signals
            )
         ++ newline ++ tab ++ tab ++ tab
         ++ "]"
         ++ newline ++ tab ++ tab
         ++ " portFuncs ="
         ++ newline ++ tab ++ tab ++ tab
         ++ "[ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map (\portName -> "ports'in'" ++ portName)
            $ map port_name
            $ filter (\port -> port_mode port == Mode_In)
            $ ports
            )
         ++ newline ++ tab ++ tab ++ tab
         ++ "]"
         ++ newline ++ tab ++ tab
         ++ " signalVals = map getSignalTime $ map (\\s -> map fst $ control'signal'transactions $ s signals) signalFuncs ++ map (\\p -> map fst $ control'signal'transactions $ p portsIn) portFuncs"
         ++ newline ++ tab
         ++ "return $ or signalVals"
       initialUpdateStr =
         "initialUpdate :: Entity'" ++ baseName ++ "'Stack ()"
         ++ newline
         ++ "initialUpdate = do"
         ++ newline ++ tab
         ++ "(Entity'State portsIn state portsOut _) <- get"
         ++ ( concat
            $ map (\signalName -> newline ++ tab ++ "lift $ control'printInitial \"" ++ printComponentActualName componentName ++ "." ++ signalName ++ "\" $ signal'" ++ signalName ++ " state")
            $ MapS.keys signals
            )
         ++ ( concat
            $ map (\portName -> newline ++ tab ++ "lift $ control'printInitial \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" $ ports'in'" ++ portName ++ " portsIn")
            $ map port_name
            $ filter (\port -> port_mode port == Mode_In)
            $ ports
            )
         ++ ( concat
            $ map (\portName -> newline ++ tab ++ "lift $ control'printInitial \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" $ ports'out'" ++ portName ++ " portsOut")
            $ map port_name
            $ filter (\port -> port_mode port == Mode_Out)
            $ ports
            )
       normalUpdateStr =
         "signalUpdate :: Entity'" ++ baseName ++ "'Stack ()"
         ++ newline
         ++ "signalUpdate = do"
         ++ newline ++ tab
         ++ "currentTime <- lift get"
         ++ newline ++ tab
         ++ "(Entity'State portsIn state portsOut processes) <- get"
         ++ ( concat
            $ map ((++) (newline ++ tab))
            $ map (\portName -> "new'ports'in'" ++ portName ++ " <- lift $ control'updateSignal \"" ++ portName ++ "\" currentTime $ ports'in'" ++ portName ++ " portsIn")
            $ map port_name
            $ filter (\port -> port_mode port == Mode_In)
            $ ports
            )
         ++ ( concat
            $ map ((++) (newline ++ tab))
            $ map (\signalName -> "new'signal'" ++ signalName ++ " <- lift $ control'updateSignal \"" ++ signalName ++ "\" currentTime $ signal'" ++ signalName ++ " state")
            $ MapS.keys signals
            )
         ++ ( concat
            $ map ((++) (newline ++ tab))
            $ map (\portName -> "new'ports'out'" ++ portName ++ " <- lift $ control'updateSignal \"" ++ portName ++ "\" currentTime $ ports'out'" ++ portName ++ " portsOut")
            $ map port_name
            $ filter (\port -> port_mode port == Mode_Out)
            $ ports
            )
         ++ newline ++ tab
         ++ "let newPortsIn ="
         ++ newline ++ tab ++ tab ++ tab
         ++ "PORTS'IN"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ tab)
            $ map (\portName -> "new'ports'in'" ++ portName)
            $ map port_name
            $ filter (\port -> port_mode port == Mode_In)
            $ ports
            )
         ++ newline ++ tab ++ tab
         ++ " newSignals ="
         ++ newline ++ tab ++ tab ++ tab
         ++ "STATE"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ tab)
            $ map (\signalName -> "new'signal'" ++ signalName)
            $ MapS.keys signals
            )
         ++ newline ++ tab ++ tab
         ++ " newPortsOut ="
         ++ newline ++ tab ++ tab ++ tab
         ++ "PORTS'OUT"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ tab)
            $ map (\portName -> "new'ports'out'" ++ portName)
            $ map port_name
            $ filter (\port -> port_mode port == Mode_Out)
            $ ports
            )
         ++ newline ++ tab
         ++ "put $ Entity'State newPortsIn newSignals newPortsOut processes"
       entityStateTypeStr = "type Entity'" ++ baseName ++ "'Stack a = Entity'Stack (Entity'State PORTS'IN STATE PORTS'OUT PROCESSES) a"
       entityControlStr =
         "entityControl :: Entity'" ++ baseName ++ "'Stack ()"
         ++ newline
         ++ "entityControl = do"
         ++ ( concat
            $ map (\s ->
                     newline ++ tab
                     ++ "processState'" ++ s ++ " <- gets entity'processes"
                     ++ newline ++ tab
                     ++ "newProcessState'" ++ s ++ " <- " ++ "process'" ++ s ++ " $ processes'" ++ s ++ " processState'" ++ s
                     ++ newline ++ tab
                     ++ "let newProcessState'total'" ++ s ++ " = processState'" ++ s ++ " { processes'" ++ s ++ " = newProcessState'" ++ s ++ " }"
                     ++ newline ++ tab ++ tab
                     ++ " update'" ++ s ++ " state = state { entity'processes = newProcessState'total'" ++ s ++ " }"
                     ++ newline ++ tab
                     ++ "modify update'" ++ s
                  )
            $ processNames
            )
   in liftIO $ appendFile fileName $
         newline
         ++ genericTypeStr ++ newline
         ++ portsInTypeStr ++ newline
         ++ portsInInitialStr ++ newline
         ++ portsOutTypeStr ++ newline
         ++ portsOutInitialStr ++ newline
         ++ signalTypeStr ++ newline
         ++ signalInitialStr ++ newline
         ++ processTypeStr ++ newline
         ++ processInitialStr ++ newline
         ++ entityStateInitialStr ++ newline
         ++ newline
         ++ collectTimingStr ++ newline
         ++ newline
         ++ checkActiveStr ++ newline
         ++ newline
         ++ initialUpdateStr ++ newline
         ++ newline
         ++ normalUpdateStr ++ newline
         ++ newline
         ++ entityStateTypeStr ++ newline
         ++ newline
         ++ entityControlStr ++ newline

outputGenerics :: FilePath -> NetlistName -> [String] -> [Generic] -> ScopeStore -> ExceptT ConverterError IO ()
outputGenerics buildDir netlistName@(NetlistName lib name) componentName generics scope = do
   let srcDir = buildDir </> "src"
       genericFileName = srcDir </> lib </> name ++ "'COMPONENT'" ++ printComponentSafeName componentName ++ "'GENERICS.hs"
   outputCabalModule buildDir $ NetlistName lib $ name ++ "'COMPONENT'" ++ printComponentSafeName componentName ++ "'GENERICS"
   liftIO $ writeFile genericFileName $
      "module "
      ++ show netlistName ++ "'COMPONENT'" ++ printComponentSafeName componentName ++ "'GENERICS"
      ++ " where"
      ++ newline
   outputImports genericFileName scope
   outputGenerics' genericFileName netlistName componentName generics

outputGenerics' :: FilePath -> NetlistName -> [String] -> [Generic] -> ExceptT ConverterError IO ()
outputGenerics' fileName netlistName componentName (generic:generics) = do
   liftIO $ appendFile fileName $
      concat $ map (\s -> newline ++ s) $ printGeneric netlistName componentName generic
   outputGenerics' fileName netlistName componentName generics
   where printGeneric :: NetlistName -> [String] -> Generic -> [String]
         printGeneric unitName componentName (Generic name (subtypePackage,subtypeName) subtype (Just value)) =
            let package = if unitName == subtypePackage
                           then show unitName ++ "'COMPONENT'" ++ printComponentSafeName componentName ++ "'GENERICS"
                           else show subtypePackage
                (baseTypePackage,baseTypeName) = case subtype of
                                                   EnumerationSubtype _ typeName _ _ -> typeName
                                                   IntegerSubtype _ typeName _ -> typeName
                                                   FloatingSubtype _ typeName _ -> typeName
                                                   PhysicalSubtype _ typeName _ _ _ -> typeName
                makeBaseTypeStr = show baseTypePackage ++ ".mkType'" ++ baseTypeName
                makeSubtypeStr = show subtypePackage ++ ".mkType'" ++ subtypeName
                valueStr =
                  case value of
                     Value_Enum _ enum ->
                        let baseTypeStr = show baseTypePackage ++ ".Type'" ++ baseTypeName
                        in makeSubtypeStr ++ " " ++ showEnum baseTypeStr enum
                     Value_Int intVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ show intVal
                     Value_Float floatVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ (showFFloat Nothing floatVal "")
                     Value_Physical physVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ show physVal
            in [ "generic'" ++ name ++ " :: " ++ package ++ ".Type'" ++ subtypeName
               , "generic'" ++ name ++ " = " ++ valueStr
               ]
outputGenerics' _ _ _ [] = return ()
