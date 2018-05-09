module Sim.Packages
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
import Sim.Imports (outputImports)
import Sim.Types (outputTypes)

newline = "\n"

outputPackages :: FilePath -> PackageStore -> ExceptT ConverterError IO ()
outputPackages buildDir packages = outputPackages' buildDir $ MapS.toList packages

outputPackages' :: FilePath -> [(NetlistName,Package)] -> ExceptT ConverterError IO ()
outputPackages' buildDir ((netlistName@(NetlistName lib packageName),package):others) = do
   let packageFileName = buildDir </> lib </> packageName ++ ".hs"
   liftIO $ writeFile packageFileName $
      "module "
      ++ show netlistName
      ++ " where"
      ++ newline
   outputImports packageFileName $ packageScope package
   --outputFunctions packageFileName $ packageFunctions package
   outputTypes packageFileName $ packageTypes package
   --outputSubtypes packageFileName $ packageSubtypes package
   --outputConstants packageFileName $ packageConstants package
   --outputSignals packageFileName $ packageSignals package
   -- ?? Etc.
   outputPackages' buildDir others
outputPackages' _ [] = return ()
