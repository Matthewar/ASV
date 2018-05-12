module Sim.Imports
   ( outputImports
   ) where

import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import qualified Data.Map.Strict as MapS
import Data.List (nub)

import Parser.Netlist.Types.Stores (ScopeStore(..))
import Manager.Types.Error (ConverterError)

newline = "\n"

outputImports :: FilePath -> ScopeStore -> ExceptT ConverterError IO ()
outputImports fileName scope =
   let packageNames =
         (MapS.elems $ scopeFunctionPackage scope)
         ++ (MapS.elems $ scopeTypePackage scope)
         ++ (MapS.elems $ scopeSubtypePackage scope)
         ++ (MapS.elems $ scopeConstantPackage scope)
         ++ (MapS.elems $ scopeSignalPackage scope)
       uniquePackageNames = map show $ nub packageNames
       importStr = concat $ map (\packageName -> "import qualified " ++ packageName ++ newline) uniquePackageNames
       baseImportsStr =
         "import qualified Data.Map.Strict"
         ++ newline
         ++ "import Data.Int (Int64)"
   in liftIO $ appendFile fileName $ newline ++ baseImportsStr ++ newline ++ importStr
