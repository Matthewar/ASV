module Sim.Output.Imports
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
         "import qualified Data.Map.Strict\n\
         \import Data.Int (Int64)\n\
         \import Data.Maybe\n\
         \   ( isJust\n\
         \   , fromJust\n\
         \   , catMaybes\n\
         \   )\n\
         \import Data.Either (isLeft)\n\
         \import Data.Char\n\
         \   ( isAlpha\n\
         \   , isDigit\n\
         \   , toUpper\n\
         \   , isAlphaNum\n\
         \   )\n\
         \import Control.Monad.Trans.State\n\
         \   ( modify\n\
         \   , gets\n\
         \   , get\n\
         \   , put\n\
         \   , execStateT\n\
         \   , evalStateT\n\
         \   )\n\
         \import Control.Monad.Except\n\
         \   ( lift\n\
         \   , when\n\
         \   , runExceptT\n\
         \   )\n\
         \import Control\n\
         \import Numeric (showFloat)\n\
         \import System.Exit\n\
         \   ( exitSuccess\n\
         \   , exitFailure\n\
         \   )"
   in liftIO $ appendFile fileName $ newline ++ baseImportsStr ++ newline ++ importStr
