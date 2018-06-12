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
       entityStateInitialStr =
         "initialStack =\n\
         \   Entity'State\n\
         \      initial'PORTS'IN\n\
         \      initial'STATE\n\
         \      initial'PORTS'OUT"
       collectTimingStr =
         "allTimes :: Entity'State PORTS'IN STATE PORTS'OUT -> Control'Time -> [[ProcessStatement PORTS'IN STATE PORTS'OUT]] -> Maybe (Either () STD.STANDARD.Type'ANON'TIME)\n\
         \allTimes (Entity'State _ signals _) (Control'Time realTime deltaTime) processes =\n\
         \   let minTime = Control'Time realTime $ deltaTime + 1\n\
         \       getSignalTime (_:(time,_):_)\n\
         \         | time == minTime = Just $ Left ()\n\
         \         | otherwise = Just $ Right $ time'Real time\n\
         \       getSignalTime _ = Nothing\n\
         \       signalData =\n\
         \         [ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ tab ++ tab ++ ", ")
            $ map ((++) "getSignalTime $ control'signal'transactions $ ")
            $ map showInternalSignalRead
            $ MapS.keys signals
            )
         ++ newline
         ++ "         ]\n\
            \       signalTimes = catMaybes signalData\n\
            \   in case (processTime processes,signalTimes) of\n\
            \         (Nothing,[]) -> Nothing\n\
            \         (Just (Left ()),_) -> Just $ Left ()\n\
            \         _ | any isLeft signalTimes -> Just $ Left ()\n\
            \         (Just (Right time),_) -> Just $ minimum (Right time:signalTimes)"
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
         "initialUpdate :: Entity'State PORTS'IN STATE PORTS'OUT -> Control'Time -> IO ()\n\
         \initialUpdate (Entity'State portsIn state portsOut) currentTime = do"
         ++ ( concat
            $ map (\signalName -> newline ++ tab ++ "control'printVars \"" ++ printComponentActualName componentName ++ "." ++ signalName ++ "\" $ signal'" ++ signalName ++ " state")
            $ MapS.keys signals
            )
         ++ ( concat
            $ map (\portName -> newline ++ tab ++ "control'printVars \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" $ ports'in'" ++ portName ++ " portsIn")
            $ map port_name
            $ portsIn
            )
         ++ ( concat
            $ map (\portName -> newline ++ tab ++ "control'printVars \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" $ ports'out'" ++ portName ++ " portsOut")
            $ map port_name
            $ portsOut
            )
         ++ newline ++ tab
         ++ "appendFile \"waves.vcd\" \"$enddefinitions $end\n#0\n$dumpvars \""
         ++ ( concat
            $ map (\signalName -> newline ++ tab ++ "control'printVal \"" ++ printComponentActualName componentName ++ "." ++ signalName ++ "\" $ signal'" ++ signalName ++ " state")
            $ MapS.keys signals
            )
         ++ ( concat
            $ map (\portName -> newline ++ tab ++ "control'printVal \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" $ ports'in'" ++ portName ++ " portsIn")
            $ map port_name
            $ portsIn
            )
         ++ ( concat
            $ map (\portName -> newline ++ tab ++ "control'printVal \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" $ ports'out'" ++ portName ++ " portsOut")
            $ map port_name
            $ portsOut
            )
         ++ newline ++ tab
         ++ "appendFile \"waves.vcd\" \"$end\""
       normalUpdateStr =
         "signalUpdate :: Entity'State PORTS'IN STATE PORTS'OUT -> Control'Time -> IO (Entity'State PORTS'IN STATE PORTS'OUT)\n\
         \signalUpdate (Entity'State portsIn signals portsOut) currentTime@(Control'Time realTime _) = do"
         ++ newline ++ tab
         ++ "appendFile \"waves.vcd\" $ \"#\" ++ show (STD.STANDARD.extractType'ANON'TIME realTime) ++ \"\\n$dumpall \""
         ++ ( concat
            $ map ((++) (newline ++ tab))
            $ map (\portName -> "new'ports'in'" ++ portName ++ " <- control'updateSignal \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" currentTime $ ports'in'" ++ portName ++ " portsIn")
            $ map port_name
            $ portsIn
            )
         ++ ( concat
            $ map ((++) (newline ++ tab))
            $ map (\signalName -> "new'signal'" ++ signalName ++ " <- control'updateSignal \"" ++ printComponentActualName componentName ++ "." ++ signalName ++ "\" currentTime $ signal'" ++ signalName ++ " signals")
            $ MapS.keys signals
            )
         ++ ( concat
            $ map ((++) (newline ++ tab))
            $ map (\portName -> "new'ports'out'" ++ portName ++ " <- control'updateSignal \"" ++ printComponentActualName componentName ++ "." ++ portName ++ "\" currentTime $ ports'out'" ++ portName ++ " portsOut")
            $ map port_name
            $ portsOut
            )
         ++ newline ++ tab
         ++ "appendFile \"waves.vcd\" \"$end\""
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
         ++ "return $ Entity'State newPortsIn newSignals newPortsOut"
       componentStr =
         "initialComponent :: [[ProcessStatement PORTS'IN STATE PORTS'OUT]]"
         ++ newline
         ++ "initialComponent ="
         ++ newline ++ tab
         ++ "[ "
         ++ ( concat
            $ intersperse (newline ++ tab ++ ", ")
            $ map (\s -> "process'" ++ s)
            $ processNames
            )
         ++ newline ++ tab
         ++ "]"
   in liftIO $ appendFile fileName $
         newline
         ++ genericTypeStr ++ newline
         ++ portsInTypeStr ++ newline
         ++ portsInInitialStr ++ newline
         ++ portsOutTypeStr ++ newline
         ++ portsOutInitialStr ++ newline
         ++ signalTypeStr ++ newline
         ++ signalInitialStr ++ newline
         ++ entityStateInitialStr ++ newline
         ++ newline
         ++ collectTimingStr ++ newline
         -- ++ newline
         -- ++ checkActiveStr ++ newline
         ++ newline
         ++ initialUpdateStr ++ newline
         ++ newline
         ++ normalUpdateStr ++ newline
         ++ newline
         ++ componentStr ++ newline
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
