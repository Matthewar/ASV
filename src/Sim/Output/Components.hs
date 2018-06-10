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
         , showTypeName
         , showNewSubtypeValue
         , showPortInName
         , showInternalSignalName
         , showPortInDef
         , showPortOutDef
         , showInternalSignalDef
         , showPortInInitialValue
         , showPortOutInitialValue
         , showInternalSignalInitialValue
         , showProcessDef
         , showProcessSetInitial
         , showProcessRead
         , showInternalSignalRead
         )
import Manager.Types.Error (ConverterError)

newline = "\n"
tab = "   "

outputComponentControl :: FilePath -> String -> [String] -> [Generic] -> [Port] -> SignalStore -> [String] -> ExceptT ConverterError IO ()
outputComponentControl fileName baseName componentName generics ports signals processNames =
   let portsIn = filterPortsByMode Mode_In
       portsOut = filterPortsByMode Mode_Out
       genericTypeStr =
         let printGeneric (Generic genericName subtypeName _ _) =
               "--GENERICS.generic'" ++ genericName ++ " :: " ++ showTypeName subtypeName
         in "--GENERICS:"
            ++ ( concat
               $ map (\s -> newline ++ printGeneric s)
               $ generics
               )
       portsInTypeStr =
         let printPort (Port portName _ subtypeName _ _) = showPortInDef portName subtypeName
         in "data PORTS'IN =\n\
            \   PORTS'IN\n\
            \      { "
            ++ ( concat
               $ intersperse (newline ++ tab ++ tab ++ ", ")
               $ map printPort
               $ portsIn
               )
            ++ newline ++ tab ++ tab
            ++ "}"
       portsInInitialStr =
         let printPort (Port portName _ subtypeName subtype value) = showPortInInitialValue portName $ showNewSubtypeValue subtypeName subtype value
         in "initial'PORTS'IN =\n\
            \   PORTS'IN\n\
            \      { "
            ++ ( concat
               $ intersperse (newline ++ tab ++ tab ++ ", ")
               $ map printPort
               $ portsIn
               )
            ++ newline ++ tab ++ tab
            ++ "}"
       portsOutTypeStr =
         let printPort (Port portName _ subtypeName _ _) = showPortOutDef portName subtypeName
         in "data PORTS'OUT =\n\
            \   PORTS'OUT\n\
            \      { "
            ++ ( concat
               $ intersperse (newline ++ tab ++ tab ++ ", ")
               $ map printPort
               $ portsOut
               )
            ++ newline ++ tab ++ tab
            ++ "}"
       portsOutInitialStr =
         let printPort (Port portName _ subtypeName subtype value) = showPortOutInitialValue portName $ showNewSubtypeValue subtypeName subtype value
         in "initial'PORTS'OUT =\n\
            \   PORTS'OUT\n\
            \      { "
            ++ ( concat
               $ intersperse (newline ++ tab ++ tab ++ ", ")
               $ map printPort
               $ portsOut
               )
            ++ newline ++ tab ++ tab
            ++ "}"
       signalTypeStr =
         let printSignal (signalName,Signal typeName _ _) = showInternalSignalDef signalName typeName
         in "data STATE =\n\
            \   STATE\n\
            \      { "
            ++ ( concat
               $ intersperse (newline ++ tab ++ tab ++ ", ")
               $ map printSignal
               $ MapS.toList signals
               )
            ++ newline ++ tab ++ tab
            ++ "}"
       signalInitialStr =
         let printSignal (signalName,Signal subtypeName subtype value) = showInternalSignalInitialValue signalName $ showNewSubtypeValue subtypeName subtype value
         in "initial'STATE =\n\
            \   STATE\n\
            \      { "
            ++ ( concat
               $ intersperse (newline ++ tab ++ tab ++ ", ")
               $ map printSignal
               $ MapS.toList signals
               )
            ++ newline ++ tab ++ tab
            ++ "}"
       processTypeStr =
         "data PROCESSES =\n\
         \   PROCESSES\n\
         \      { "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ ", ")
            $ map showProcessDef
            $ processNames
            )
         ++ newline ++ tab ++ tab
         ++ "}"
       processInitialStr =
         "initial'PROCESSES = \n\
         \   PROCESSES\n\
         \      { "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ ", ")
            $ map showProcessSetInitial
            $ processNames
            )
         ++ newline ++ tab ++ tab
         ++ "}"
       entityStateInitialStr =
         "initialStack =\n\
         \   Entity'State\n\
         \      initial'PORTS'IN\n\
         \      initial'STATE\n\
         \      initial'PORTS'OUT\n\
         \      initial'PROCESSES"
       collectTimingStr =
         "allTimes :: Entity'" ++ baseName ++ "'Stack (Maybe STD.STANDARD.Type'ANON'TIME)\n\
         \allTimes = do\n\
         \   signals <- gets entity'state\n\
         \   processes <- gets entity'processes\n\
         \   let getSignalTime (_:(time,_):_) = Just $ time'Real time\n\
         \       getSignalTime _ = Nothing\n\
         \       signalData =\n\
         \         ["
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map showInternalSignalRead
            $ MapS.keys signals
            )
         ++ newline
         ++ "         ]\n\
            \       signalTimes = map fromJust $ filter isJust $ map getSignalTime $ map control'signal'transactions signalData\n\
            \       getProcessTime (_,time) = time\n\
            \       processData =\n\
            \         ["
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map showProcessRead
            $ processNames
            )
         ++ newline
         ++ "         ]\n\
            \       processTimes = map getProcessTime processData\n\
            \   case signalTimes ++ processTimes of\n\
            \      [] -> return Nothing\n\
            \      other -> return $ Just $ minimum other"
       checkActiveStr =
         "anyReady :: Entity'" ++ baseName ++ "'Stack Bool"
         ++ newline
         ++ "anyReady = do\n\
            \   (Control'Time realTime deltaTime) <- lift get\n\
            \   signals <- gets entity'state\n\
            \   portsIn <- gets entity'portsIn\n\
            \   let getSignalTime (_:nextTime:_) = nextTime == Control'Time realTime (deltaTime + 1)\n\
            \       getSignalTime _ = False\n\
            \       signalFuncs =\n\
            \         [ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map showInternalSignalName
            $ MapS.keys signals
            )
         ++ newline
         ++ "         ]\n\
            \       portFuncs =\n\
            \         [ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map showPortInName
            $ map port_name
            $ filter (\port -> port_mode port == Mode_In)
            $ ports
            )
         ++ newline
         ++ "         ]\n\
            \       signalVals = map getSignalTime $ map (\\s -> map fst $ control'signal'transactions $ s signals) signalFuncs ++ map (\\p -> map fst $ control'signal'transactions $ p portsIn) portFuncs\n\
            \   return $ or signalVals"
       initialUpdateStr =
         "initialUpdate :: Entity'" ++ baseName ++ "'Stack ()"
         ++ newline
         ++ "initialUpdate = do\n\
            \   (Entity'State portsIn state portsOut _) <- get"
         ++ ( concat
            $ map (\signalName -> newline ++ tab ++ "lift $ control'printInitial \"" ++ printComponentActualName componentName ++ "." ++ signalName ++ "\" $ signal'" ++ signalName ++ " state")
            $ MapS.keys signals
            )
         ++ ( concat
            $ map (\portName -> newline ++ tab ++ "lift $ control'printInitial \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" $ ports'in'" ++ portName ++ " portsIn")
            $ map port_name
            $ portsIn
            )
         ++ ( concat
            $ map (\portName -> newline ++ tab ++ "lift $ control'printInitial \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" $ ports'out'" ++ portName ++ " portsOut")
            $ map port_name
            $ portsOut
            )
       normalUpdateStr =
         "signalUpdate :: Entity'" ++ baseName ++ "'Stack ()\n\
         \signalUpdate = do\n\
         \   currentTime <- lift get\n\
         \   (Entity'State portsIn state portsOut processes) <- get"
         ++ ( concat
            $ map ((++) (newline ++ tab))
            $ map (\portName -> "new'ports'in'" ++ portName ++ " <- lift $ control'updateSignal \"" ++ portName ++ "\" currentTime $ ports'in'" ++ portName ++ " portsIn")
            $ map port_name
            $ portsIn
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
            $ portsOut
            )
         ++ newline
         ++ "   let newPortsIn =\n\
            \         PORTS'IN"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ tab)
            $ map (\portName -> "new'ports'in'" ++ portName)
            $ map port_name
            $ filter (\port -> port_mode port == Mode_In)
            $ ports
            )
         ++ newline
         ++ "       newSignals =\n\
            \         STATE"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ tab)
            $ map (\signalName -> "new'signal'" ++ signalName)
            $ MapS.keys signals
            )
         ++ newline
         ++ "       newPortsOut =\n\
            \         PORTS'OUT"
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
            $ map ((++) (newline ++ tab))
            $ concat
            $ map (\s ->
                     [ "processState'" ++ s ++ " <- gets entity'processes"
                     , "newProcessState'" ++ s ++ " <- " ++ "process'" ++ s ++ " $ processes'" ++ s ++ " processState'" ++ s
                     , "let newProcessState'total'" ++ s ++ " = processState'" ++ s ++ " { processes'" ++ s ++ " = newProcessState'" ++ s ++ " }"
                     , tab ++ " update'" ++ s ++ " state = state { entity'processes = newProcessState'total'" ++ s ++ " }"
                     , "modify update'" ++ s
                     ]
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
   where filterPortsByMode :: Mode -> [Port]
         filterPortsByMode expectedMode = filter (\(Port _ mode _ _ _) -> mode == expectedMode) ports

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
