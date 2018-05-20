module Sim.Entities
   ( outputEntities
   ) where

import System.FilePath ((</>))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import qualified Data.Map.Strict as MapS

import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores
         ( Entity(..)
         , EntityStore
         )
import Manager.Types.Error (ConverterError)
import Sim.Imports (outputImports)
import Sim.Types (outputTypes)
import Sim.Subtypes (outputSubtypes)
import Sim.Constants (outputConstants)

newline = "\n"

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
   --outputSignals entityFileName $ entitySignals entity
   -- ?? Etc.
   outputEntities' buildDir others
outputEntities' _ [] = return ()
