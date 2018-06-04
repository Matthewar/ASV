module Sim.Output.Architectures
   ( outputArchitecture
   ) where

import System.FilePath ((</>))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import qualified Data.Map.Strict as MapS
import Data.List (intersperse)

import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores (Architecture(..))
import Manager.Types.Error (ConverterError)
import Sim.Functions.ConvertNames (convertArchitectureNames)
import Sim.Output.Imports (outputImports)
import Sim.Output.Types (outputTypes)
import Sim.Output.Subtypes (outputSubtypes)
import Sim.Output.Constants (outputConstants)
import Sim.Output.Processes (outputProcesses)
import Sim.Output.Components (outputComponentControl)
import Sim.Output.Cabal (outputCabalModule)
import Sim.Output.Names
         ( printComponentActualName
         , printComponentSafeName
         )

newline = "\n"
tab = "   "

outputArchitecture :: FilePath -> Architecture -> NetlistName -> [String] -> ExceptT ConverterError IO ()
outputArchitecture buildDir baseArchitecture netlistName@(NetlistName lib architectureName) componentName = do
   let newArchitectureName = architectureName ++ "'COMPONENT'" ++ printComponentSafeName componentName
       newNetlistName = NetlistName lib newArchitectureName
       srcDir = buildDir </> "src"
       architectureFileName = srcDir </> lib </> newArchitectureName ++ ".hs"
   outputCabalModule buildDir newNetlistName
   liftIO $ writeFile architectureFileName $
       "module "
       ++ show newNetlistName
       ++ " where"
       ++ newline ++ newline
       ++ "import qualified " ++ show newNetlistName ++ "'GENERICS as GENERICS"
       ++ newline
   let newArchitecture = convertArchitectureNames baseArchitecture netlistName newNetlistName
   outputImports architectureFileName $ archScope newArchitecture
   --outputFunctions architectureFileName $ archFunctions newArchitecture
   outputTypes architectureFileName newNetlistName $ archTypes newArchitecture
   outputSubtypes architectureFileName $ archSubtypes newArchitecture
   outputConstants architectureFileName $ archConstants newArchitecture
   outputProcesses architectureFileName newNetlistName $ archProcesses newArchitecture
   -- ?? Etc.
   outputComponentControl architectureFileName newArchitectureName componentName (archGenerics newArchitecture) (archPorts newArchitecture) (archSignals newArchitecture) (MapS.keys $ archProcesses newArchitecture)
