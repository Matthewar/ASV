module Sim.Output.Control
   ( outputControl
   ) where

import System.FilePath ((</>))
import qualified Data.Map.Strict as MapS
import Data.Maybe (isNothing)
import Control.Monad.Except
         ( ExceptT
         , liftIO
         , when
         )

import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Generic(..)
         )
import Parser.Netlist.Types.Stores
         ( Entity(..)
         , EntityStore
         , emptyScopeStore
         )
import Sim.Output.Entities (outputEntity)
import Sim.Output.Components (outputGenerics)
import Sim.Output.Imports (outputImports)
import Manager.Types.Error (ConverterError)

newline = "\n"
tab = "   "

outputControl :: FilePath -> NetlistName -> EntityStore -> ExceptT ConverterError IO ()
outputControl buildDir topUnitName@(NetlistName topLib topUnit) entities = do
   let componentName = "TOP'MODULE"
       topUnitFullName = NetlistName topLib $ topUnit ++ "'COMPONENT'" ++ componentName -- ?? Need to deal with separate entities and architectures
   entity <- case MapS.lookup topUnitName entities of
      Just entity -> return entity
      Nothing -> error "No entity with top module name"
   -- ?? Check architectures
   let generics = entityGenerics entity
   when (any (\generic -> isNothing $ generic_default generic) generics) $ error "Top module generics must have default values"
   outputGenerics buildDir topUnitName componentName generics (entityScope entity)
   outputEntity buildDir entity topUnitName componentName
   outputTopModule buildDir topUnitFullName

outputTopModule :: FilePath -> NetlistName -> ExceptT ConverterError IO ()
outputTopModule buildDir topUnitFullName@(NetlistName topUnitLib topUnitName) = do
   let controlModuleName = buildDir </> "Top.hs"
   liftIO $ writeFile controlModuleName "module Top where"
   outputImports controlModuleName emptyScopeStore
   let componentTypeStr =
         "data Components ="
         ++ newline ++ tab
         ++ "Components"
         ++ newline ++ tab ++ tab
         ++ "{ component'" ++ topUnitLib ++ "'" ++ topUnitName ++ " :: Entity'State TopModule.PORTS'IN TopModule.STATE TopModule.PORTS'OUT TopModule.PROCESSES"
         ++ newline ++ tab ++ tab
         ++ "}"
       componentInitialStr =
         "initialStack ="
         ++ newline ++ tab
         ++ "Components"
         ++ newline ++ tab ++ tab
         ++ "TopModule.initialStack"
       stackTypeStr =
         "type TopStack a = Entity'Stack Components a"
       repeatFuncStr =
         "topRepeat :: TopStack ()"
         ++ newline
         ++ "topRepeat = do"
         ++ newline ++ tab
         ++ "components <- get"
         ++ newline ++ tab
         ++ "componentState'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ " <- lift $ execStateT TopModule.entityControl $ component'" ++ topUnitLib ++ "'" ++ topUnitName ++ " components"
         ++ newline ++ tab
         ++ "let updateState state = state { component'" ++ topUnitLib ++ "'" ++ topUnitName ++ " = componentState'" ++ topUnitLib ++ "'" ++ topUnitName ++ " }"
         ++ newline ++ tab
         ++ "modify updateState"
         ++ newline ++ tab
         ++ "minTime'" ++ topUnitLib ++ "'" ++ topUnitName ++ " <- lift $ evalStateT TopModule.allTimes componentState'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ newline ++ tab
         ++ "if False -- ?? If signals active, then increase one delta, else below. (Also need to check if delta has reached max)"
         ++ newline ++ tab ++ tab
         ++ "then do"
         ++ newline ++ tab ++ tab ++ tab
         ++ "delta <- lift $ gets (\\time -> time'Delta time)"
         ++ newline ++ tab ++ tab ++ tab
         ++ "when (delta == 1000) $ error \"Possible infinite loop, delta at infinity\""
         ++ newline ++ tab ++ tab ++ tab
         ++ "lift $ modify (\\(Control'Time real delta) -> Control'Time real (delta + 1))"
         ++ newline ++ tab ++ tab
         ++ "else do"
         ++ newline ++ tab ++ tab ++ tab
         ++ "time <- lift $ gets (\\time -> time'Real time)"
         ++ newline ++ tab ++ tab ++ tab
         ++ "when (time == (STD.STANDARD.mkType'ANON'TIME $ toInteger (maxBound :: Int64))) $ error \"Time limit reached\"" -- ?? Come up with better exit
         ++ newline ++ tab ++ tab ++ tab
         ++ "case minTime'" ++ topUnitLib ++ "'" ++ topUnitName ++ " of"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ "Just minTime -> lift $ modify (\\(Control'Time _ delta) -> Control'Time minTime delta)"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ "Nothing -> error \"Nothing to do\""
         ++ newline ++ tab ++ tab ++ tab
         ++ "topRepeat"
       topFuncStr =
         "top :: IO ()"
         ++ newline
         ++ "top = do"
         ++ newline ++ tab
         ++ "result <- runExceptT $"
         ++ newline ++ tab ++ tab
         ++ "execStateT"
         ++ newline ++ tab ++ tab ++ tab
         ++ "( evalStateT"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ "(evalStateT topRepeat initialStack)"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ "(Control'Time (STD.STANDARD.mkType'ANON'TIME 0) 0)"
         ++ newline ++ tab ++ tab ++ tab
         ++ ")"
         ++ newline ++ tab ++ tab ++ tab
         ++ "(Control'SEVERITY_TRACKER 0 0)"
         ++ newline ++ tab
         ++ "case result of"
         ++ newline ++ tab ++ tab
         ++ "Right res -> putStrLn $ show res"
         ++ newline ++ tab ++ tab
         ++ "Left (Control'SEVERITY_FAILURE str) -> putStrLn $ \"Severity Failure: \" ++ str"
   liftIO $ appendFile controlModuleName $
         newline
         ++ "import qualified STD.STANDARD" -- ?? Better way of including this automatically
         ++ newline
         ++ "import qualified " ++ show topUnitFullName ++ " as TopModule"
         ++ newline ++ newline
         ++ componentTypeStr
         ++ newline
         ++ componentInitialStr
         ++ newline ++ newline
         ++ stackTypeStr
         ++ newline ++ newline
         ++ repeatFuncStr
         ++ newline ++ newline
         ++ topFuncStr
