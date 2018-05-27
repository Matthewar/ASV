module Sim.Output.Entities
   ( outputEntities
   , outputEntity
   ) where

import System.FilePath ((</>))
import Numeric (showFFloat)
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
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
         ( Entity(..)
         , EntityStore
         , SignalStore
         )
import Manager.Types.Error (ConverterError)
import Sim.Functions.ConvertNames (convertEntityNames)
import Sim.Output.Imports (outputImports)
import Sim.Output.Types
         ( outputTypes
         , showEnum
         )
import Sim.Output.Subtypes (outputSubtypes)
import Sim.Output.Constants (outputConstants)
import Sim.Output.Processes (outputProcesses)
import Sim.Output.Cabal (outputCabalModule)

newline = "\n"
tab = "   "

outputEntity :: FilePath -> Entity -> NetlistName -> String -> ExceptT ConverterError IO ()
outputEntity buildDir baseEntity netlistName@(NetlistName lib entityName) componentName = do
   let newEntityName = entityName ++ "'COMPONENT'" ++ componentName
       newNetlistName = NetlistName lib newEntityName
       srcDir = buildDir </> "src"
       entityFileName = srcDir </> lib </> newEntityName ++ ".hs"
   outputCabalModule buildDir newNetlistName
   liftIO $ writeFile entityFileName $
       "module "
       ++ show newNetlistName
       ++ " where"
       ++ newline ++ newline
       ++ "import qualified " ++ show newNetlistName ++ "'GENERICS as GENERICS"
       ++ newline
   let newEntity = convertEntityNames baseEntity netlistName newNetlistName
   outputImports entityFileName $ entityScope newEntity
   --outputFunctions entityFileName $ entityFunctions newEntity
   outputTypes entityFileName newNetlistName $ entityTypes newEntity
   outputSubtypes entityFileName $ entitySubtypes newEntity
   outputConstants entityFileName $ entityConstants newEntity
   outputProcesses entityFileName newNetlistName $ entityProcesses newEntity
   -- ?? Etc.
   outputEntityControl entityFileName newEntityName (entityGenerics newEntity) (entityPorts newEntity) (entitySignals newEntity) (MapS.keys $ entityProcesses newEntity)

outputEntities :: FilePath -> EntityStore -> ExceptT ConverterError IO ()
outputEntities buildDir entities = outputEntities' buildDir $ MapS.toList entities

outputEntities' :: FilePath -> [(NetlistName,Entity)] -> ExceptT ConverterError IO ()
outputEntities' buildDir ((netlistName@(NetlistName lib entityName),entity):others) = do
   let srcDir = buildDir </> "src"
       entityFileName = srcDir </> lib </> entityName ++ ".hs"
   outputCabalModule buildDir netlistName
   liftIO $ writeFile entityFileName newline
      -- "module "
      -- ++ show netlistName
      -- ++ " where"
      -- ++ newline
   -- ?? Entity control function generation
   outputImports entityFileName $ entityScope entity
   --outputFunctions entityFileName $ entityFunctions entity
   outputTypes entityFileName netlistName $ entityTypes entity
   outputSubtypes entityFileName $ entitySubtypes entity
   outputConstants entityFileName $ entityConstants entity
   outputProcesses entityFileName netlistName $ entityProcesses entity
   -- ?? Etc.
   outputEntityControl entityFileName entityName (entityGenerics entity) (entityPorts entity) (entitySignals entity) (MapS.keys $ entityProcesses entity)
   outputEntities' buildDir others
outputEntities' _ [] = return ()

outputEntityControl :: FilePath -> String -> [Generic] -> [Port] -> SignalStore -> [String] -> ExceptT ConverterError IO ()
outputEntityControl fileName entityName generics ports signals processNames =
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
               in "ports'in'" ++ portName ++ " = Control'Signal [(initialTime," ++ subtypeBuild ++ " $ " ++ valueStr ++ ")] False"
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
               in "ports'out'" ++ portName ++ " = Control'Signal [(initialTime," ++ subtypeBuild ++ " $ " ++ valueStr ++ ")] False"
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
               "signal'" ++ signalName ++ " :: Control'Signal " ++ show typePackage ++ "." ++ typeName
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
               in "signal'" ++ signalName ++ " = Control'Signal [(STD.STANDARD.mkType'ANON'TIME 0," ++ subtypeBuild ++ " $ " ++ valueStr ++ ")] False"
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
       collectTimingStr = -- ?? Similar function to check active signals
         "allTimes :: Entity'" ++ entityName ++ "'Stack (Maybe STD.STANDARD.Type'ANON'TIME)"
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
       entityStateTypeStr = "type Entity'" ++ entityName ++ "'Stack a = Entity'Stack (Entity'State PORTS'IN STATE PORTS'OUT PROCESSES) a"
       entityControlStr =
         "entityControl :: Entity'" ++ entityName ++ "'Stack ()"
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
         ++ entityStateTypeStr ++ newline
         ++ newline
         ++ entityControlStr ++ newline
