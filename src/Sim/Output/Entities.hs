module Sim.Output.Entities
--   ( outputEntities
   ( outputEntity
   ) where

import System.FilePath ((</>))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import qualified Data.Map.Strict as MapS

import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores (Entity(..))
import Manager.Types.Error (ConverterError)
import Sim.Functions.ConvertNames (convertEntityNames)
import Sim.Output.Imports (outputImports)
import Sim.Output.Types (outputTypes)
import Sim.Output.Subtypes (outputSubtypes)
import Sim.Output.Constants (outputConstants)
import Sim.Output.Processes (outputProcesses)
import Sim.Output.Components (outputComponentControl)
import Sim.Output.Cabal (outputCabalModule)
import Sim.Output.Names (printComponentSafeName)

newline = "\n"
tab = "   "

outputEntity :: FilePath -> Entity -> NetlistName -> [String] -> ExceptT ConverterError IO ()
outputEntity buildDir baseEntity netlistName@(NetlistName lib entityName) componentName = do
   let newEntityName = entityName ++ "'COMPONENT'" ++ printComponentSafeName componentName
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
   outputComponentControl entityFileName newEntityName componentName (entityGenerics newEntity) (entityPorts newEntity) (entitySignals newEntity) (MapS.keys $ entityProcesses newEntity)

--outputEntities :: FilePath -> EntityStore -> ExceptT ConverterError IO ()
--outputEntities buildDir entities = outputEntities' buildDir $ MapS.toList entities
--
--outputEntities' :: FilePath -> [(NetlistName,Entity)] -> ExceptT ConverterError IO ()
--outputEntities' buildDir ((netlistName@(NetlistName lib entityName),entity):others) = do
--   let srcDir = buildDir </> "src"
--       entityFileName = srcDir </> lib </> entityName ++ ".hs"
--   outputCabalModule buildDir netlistName
--   liftIO $ writeFile entityFileName newline
--      -- "module "
--      -- ++ show netlistName
--      -- ++ " where"
--      -- ++ newline
--   -- ?? Entity control function generation
--   outputImports entityFileName $ entityScope entity
--   --outputFunctions entityFileName $ entityFunctions entity
--   outputTypes entityFileName netlistName $ entityTypes entity
--   outputSubtypes entityFileName $ entitySubtypes entity
--   outputConstants entityFileName $ entityConstants entity
--   outputProcesses entityFileName netlistName $ entityProcesses entity
--   -- ?? Etc.
--   outputEntityControl entityFileName entityName (entityGenerics entity) (entityPorts entity) (entitySignals entity) (MapS.keys $ entityProcesses entity)
--   outputEntities' buildDir others
--outputEntities' _ [] = return ()
