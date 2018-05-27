module Sim.Output.Packages
   ( outputPackages
   ) where

import System.FilePath ((</>))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import qualified Data.Map.Strict as MapS

import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores
         ( Package(..)
         , PackageStore
         )
import Manager.Types.Error (ConverterError)
import Sim.Output.Imports (outputImports)
import Sim.Output.Types (outputTypes)
import Sim.Output.Subtypes (outputSubtypes)
import Sim.Output.Constants (outputConstants)
import Sim.Output.Cabal (outputCabalModule)

newline = "\n"

outputPackages :: FilePath -> PackageStore -> ExceptT ConverterError IO ()
outputPackages buildDir packages = outputPackages' buildDir $ MapS.toList packages

outputPackages' :: FilePath -> [(NetlistName,Package)] -> ExceptT ConverterError IO ()
outputPackages' buildDir ((NetlistName "STD" "STANDARD",_):others) = outputPackages' buildDir others -- Don't process STD.STANDARD package
outputPackages' buildDir ((netlistName@(NetlistName lib packageName),package):others) = do
   let srcDir = buildDir </> "src"
       packageFileName = srcDir </> lib </> packageName ++ ".hs"
   outputCabalModule buildDir netlistName
   liftIO $ writeFile packageFileName $
      "module "
      ++ show netlistName
      ++ " where"
      ++ newline
   outputImports packageFileName $ packageScope package
   --outputFunctions packageFileName $ packageFunctions package
   outputTypes packageFileName netlistName $ packageTypes package
   outputSubtypes packageFileName $ packageSubtypes package
   outputConstants packageFileName $ packageConstants package
   --outputSignals packageFileName $ packageSignals package
   -- ?? Etc.
   outputPackages' buildDir others
outputPackages' _ [] = return ()
