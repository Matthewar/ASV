module Sim.Output.Cabal
   ( outputCabalPrefix
   , outputCabalModule
   , outputCabalFinal
   ) where

import System.FilePath ((</>))
import qualified Data.Text.IO as TextIO
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )

import Parser.Netlist.Types.Representation (NetlistName)
import Sim.Builtin.Cabal
         ( cabalPrefix
         , cabalSuffix
         )
import Manager.Types.Error (ConverterError)

cabalFileName :: String
cabalFileName = "VHDL-Sim.cabal"

outputCabalPrefix :: FilePath -> ExceptT ConverterError IO ()
outputCabalPrefix buildDir =
   let cabalFilePath = buildDir </> cabalFileName
   in liftIO $ TextIO.writeFile cabalFilePath cabalPrefix

outputCabalModule :: FilePath -> NetlistName -> ExceptT ConverterError IO ()
outputCabalModule buildDir netlistName =
   let cabalFilePath = buildDir </> cabalFileName
       newCabalLine = "\n                     , " ++ show netlistName
   in liftIO $ appendFile cabalFilePath newCabalLine

outputCabalFinal :: FilePath -> ExceptT ConverterError IO ()
outputCabalFinal buildDir =
   let cabalFilePath = buildDir </> cabalFileName
   in liftIO $ TextIO.appendFile cabalFilePath cabalSuffix
