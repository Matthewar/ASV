module Sim.Output.Top
   ( outputTop
   ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import qualified Data.Map.Strict as MapS
import Data.Function ((&))
import Data.List (nub)
import qualified Data.Text.IO as TextIO

import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores (NetlistStore(..))
import Manager.Types.Error (ConverterError)
import Sim.Output.Entities (outputEntities)
import Sim.Output.Packages (outputPackages)
import Sim.Output.Control (outputControl)
import Sim.Output.Cabal
         ( outputCabalPrefix
         , outputCabalFinal
         )
import Sim.Builtin.STD.STANDARD (standardPackage)
import Sim.Builtin.ControlModule (controlModule)
import Sim.Builtin.MainModule (mainModule)
import Sim.Builtin.Stack (stackFile)

outputTop :: FilePath -> NetlistStore -> NetlistName -> ExceptT ConverterError IO ()
outputTop buildDir netlist topModule = do
   liftIO $ makeDirectories buildDir netlist
   outputCabalPrefix buildDir
   --outputEntities buildDir $ entities netlist
   outputPackages buildDir $ packages netlist
   outputControl buildDir topModule $ entities netlist
   outputBuiltins buildDir
   outputCabalFinal buildDir

makeDirectories :: FilePath -> NetlistStore -> IO ()
makeDirectories buildDir netlist = do
   let netlistNames = MapS.keys $ packages netlist
       libNames =
         netlistNames
         & map (\(NetlistName libName _) -> libName)
         & nub
       srcDir = buildDir </> "src"
       genLibs (lib:others) = do
         let dir = srcDir </> lib
         createDirectoryIfMissing False dir
         genLibs others
       genLibs [] = return ()
   createDirectoryIfMissing False srcDir
   createDirectoryIfMissing False $ buildDir </> "app"
   genLibs libNames

outputBuiltins :: FilePath -> ExceptT ConverterError IO ()
outputBuiltins buildDir = do
   let srcDir = buildDir </> "src"
       standardFileName = srcDir </> "STD/STANDARD.hs"
   liftIO $ TextIO.writeFile standardFileName standardPackage
   let controlFuncsFileName = srcDir </> "Control.hs"
   liftIO $ TextIO.writeFile controlFuncsFileName controlModule
   let executableFileName = buildDir </> "app/Main.hs"
   liftIO $ TextIO.writeFile executableFileName mainModule
   let stackYamlFileName = buildDir </> "stack.yaml"
   liftIO $ TextIO.writeFile stackYamlFileName stackFile
