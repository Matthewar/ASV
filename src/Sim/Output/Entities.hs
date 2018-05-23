module Sim.Output.Entities
   ( outputEntities
   ) where

import System.FilePath ((</>))
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
         )
import Parser.Netlist.Types.Stores
         ( Entity(..)
         , EntityStore
         , SignalStore
         )
import Manager.Types.Error (ConverterError)
import Sim.Output.Imports (outputImports)
import Sim.Output.Types (outputTypes)
import Sim.Output.Subtypes (outputSubtypes)
import Sim.Output.Constants (outputConstants)
--import Sim.Output.Processes (outputProcesses)

newline = "\n"
tab = "   "

outputEntities :: FilePath -> EntityStore -> ExceptT ConverterError IO ()
outputEntities buildDir entities = outputEntities' buildDir $ MapS.toList entities

outputEntities' :: FilePath -> [(NetlistName,Entity)] -> ExceptT ConverterError IO ()
outputEntities' buildDir ((netlistName@(NetlistName lib entityName),entity):others) = do
   let entityFileName = buildDir </> lib </> entityName ++ ".hs"
   liftIO $ writeFile entityFileName $
      "module "
      ++ show netlistName
      ++ " where"
      ++ newline
   -- ?? Entity control function generation
   outputImports entityFileName $ entityScope entity
   --outputFunctions entityFileName $ entityFunctions entity
   outputTypes entityFileName netlistName $ entityTypes entity
   outputSubtypes entityFileName $ entitySubtypes entity
   outputConstants entityFileName $ entityConstants entity
   --outputProcesses entityFileName $ entityProcesses entity
   -- ?? Etc.
   outputEntityControl entityFileName (entityGenerics entity) (entityPorts entity) $ entitySignals entity
   outputEntities' buildDir others
outputEntities' _ [] = return ()

outputEntityControl :: FilePath -> [Generic] -> [Port] -> SignalStore -> ExceptT ConverterError IO ()
outputEntityControl fileName generics ports signals =
   let genericTypeStr =
         let printGeneric (Generic genericName (subtypePackage,subtypeName) _ _) =
               "generics'" ++ genericName ++ " :: " ++ show subtypePackage ++ "." ++ subtypeName
         in "data GENERICS "
            ++ newline ++ tab
            ++ "GENERICS"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printGeneric generics)
            ++ newline ++ tab ++ tab
            ++ "}"
       portsInTypeStr =
         let printPort (Port portName _ (subtypePackage,subtypeName) _ _) =
               "ports'in'" ++ portName ++ " :: " ++ show subtypePackage ++ "." ++ subtypeName
         in "data PORTS'IN ="
            ++ newline ++ tab
            ++ "PORTS'IN"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printPort $ filter (\(Port _ mode _ _ _) -> mode == Mode_In) ports)
            ++ newline ++ tab ++ tab
            ++ "}"
       portsOutTypeStr =
         let printPort (Port portName _ (subtypePackage,subtypeName) _ _) =
               "ports'out'" ++ portName ++ " :: " ++ show subtypePackage ++ "." ++ subtypeName
         in "data PORTS'OUT ="
            ++ newline ++ tab
            ++ "PORTS'OUT"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printPort $ filter (\(Port _ mode _ _ _) -> mode == Mode_Out) ports)
            ++ newline ++ tab ++ tab
            ++ "}"
       signalTypeStr =
         let printSignal (signalName,Signal (typePackage,typeName) _ _) =
               "signal'" ++ signalName ++ " :: " ++ show typePackage ++ "." ++ typeName
         in "data STATE ="
            ++ newline ++ tab
            ++ "STATE"
            ++ newline ++ tab ++ tab
            ++ "{ "
            ++ (concat $ intersperse (newline ++ tab ++ tab ++ ", ") $ map printSignal $ MapS.toList signals)
            ++ newline ++ tab ++ tab
            ++ "}"
       entityControlStr = "entityControl :: GENERICS -> Entity'Stack (Entity'State PORTS'IN STATE PORTS'OUT) ()"
   in liftIO $ appendFile fileName $
         newline
         ++ genericTypeStr ++ newline
         ++ portsInTypeStr ++ newline
         ++ portsOutTypeStr ++ newline
         ++ signalTypeStr ++ newline
         ++ entityControlStr ++ newline
