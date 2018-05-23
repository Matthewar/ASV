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

import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores (NetlistStore(..))
import Manager.Types.Error (ConverterError)
import Sim.Output.Entities (outputEntities)
import Sim.Output.Packages (outputPackages)

outputTop :: FilePath -> NetlistStore -> ExceptT ConverterError IO ()
outputTop buildDir netlist = do
   liftIO $ makeDirectories buildDir netlist
   outputEntities buildDir $ entities netlist
   outputPackages buildDir $ packages netlist

makeDirectories :: FilePath -> NetlistStore -> IO ()
makeDirectories buildDir netlist =
   let netlistNames = MapS.keys $ packages netlist
       libNames =
         netlistNames
         & map (\(NetlistName libName _) -> libName)
         & nub
   in genLibs libNames
   where genLibs (lib:others) = do
            let dir = buildDir </> lib
            createDirectoryIfMissing False dir
            genLibs others
         genLibs [] = return ()
