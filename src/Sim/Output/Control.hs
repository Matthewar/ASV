module Sim.Output.Control
   ( outputControl
   ) where

import System.FilePath ((</>))
import qualified Data.Map.Strict as MapS
import Data.Maybe (isNothing)
import Control.Monad.Except
         ( ExceptT
         , throwError
         , liftIO
         , when
         )

import Lexer.Alex.Types (AlexPosn(..))
import Lexer.Types.PositionWrapper
import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Generic(..)
         )
import Parser.Netlist.Types.Stores
         ( Entity(..)
         , EntityStore
         , emptyScopeStore
         , Architecture(..)
         , ArchitectureStore
         )
import Sim.Types.Error (SimOutputError(..))
import Sim.Output.Entities (outputEntity)
import Sim.Output.Architectures (outputArchitecture)
import Sim.Output.Components (outputGenerics)
import Sim.Output.Imports (outputImports)
import Sim.Output.Names (printComponentSafeName)
import Manager.Types.Error (ConverterError(..))

newline = "\n"
tab = "   "

outputControl :: FilePath -> NetlistName -> ArchitectureStore -> EntityStore -> ExceptT ConverterError IO ()
outputControl buildDir topUnitName@(NetlistName topLib topUnit) architectures entities = do
   let componentName = ["TOP"]
       topUnitFullName = NetlistName topLib $ topUnit ++ "'COMPONENT'" ++ printComponentSafeName componentName -- ?? Need to deal with separate entities and architectures
   case MapS.toList $ MapS.filterWithKey (\(libName,entityName,_) _ -> libName == topLib && entityName == topUnit) architectures of
      [] -> do
         entity <- case MapS.lookup topUnitName entities of
            Just entity -> return entity
            Nothing -> throwError $ ConverterError_Sim $ SimErr_NoEntityOrArchWithTopModuleName topUnitName
         let generics = entityGenerics entity
         when (any (\generic -> isNothing $ generic_default generic) generics) $ throwError $ ConverterError_Sim $ SimErr_TopGenericsWithoutDefaultValues
         outputGenerics buildDir topUnitName componentName generics (entityScope entity)
         outputEntity buildDir entity topUnitName componentName
      [(_,architecture)] -> do
         let generics = archGenerics architecture
         when (any (\generic -> isNothing $ generic_default generic) generics) $ throwError $ ConverterError_Sim $ SimErr_TopGenericsWithoutDefaultValues
         outputGenerics buildDir topUnitName componentName generics (archScope architecture)
         outputArchitecture buildDir architecture topUnitName componentName
      _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper (AlexPn 0 0 0) "Multiple architectures for a single entity" -- ?? Need to wrap positions into table
   outputTopModule buildDir topUnitFullName

outputTopModule :: FilePath -> NetlistName -> ExceptT ConverterError IO ()
outputTopModule buildDir topUnitFullName@(NetlistName topUnitLib topUnitName) = do
   let srcDir = buildDir </> "src"
       controlModuleName = srcDir </> "Top.hs"
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
       initialFuncStr =
         "topInitial :: STD.STANDARD.Type'ANON'TIME -> TopStack ()"
         ++ newline
         ++ "topInitial maxTime = do"
         ++ newline ++ tab
         ++ "componentState'" ++ topUnitLib ++ "'" ++ topUnitName ++ " <- gets component'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ newline ++ tab
         ++ "lift $ evalStateT TopModule.initialUpdate componentState'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ newline ++ tab
         ++ "topRepeat maxTime"
       repeatFuncStr =
         "topRepeat :: STD.STANDARD.Type'ANON'TIME -> TopStack ()"
         ++ newline
         ++ "topRepeat maxTime = do"
         ++ newline ++ tab
         ++ "components <- get"
         ++ newline ++ tab
         ++ "componentState'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ " <- lift $ execStateT TopModule.entityControl $ component'" ++ topUnitLib ++ "'" ++ topUnitName ++ " components"
         ++ newline ++ tab
         ++ "minTime'" ++ topUnitLib ++ "'" ++ topUnitName ++ " <- lift $ evalStateT TopModule.allTimes componentState'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ newline ++ tab
         ++ "areAnyReady <- lift $ evalStateT TopModule.anyReady componentState'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ newline ++ tab
         ++ "if areAnyReady"
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
         ++ "when (time == maxTime) $ error \"Time limit reached\"" -- ?? Come up with better exit
         ++ newline ++ tab ++ tab ++ tab
         ++ "case minTime'" ++ topUnitLib ++ "'" ++ topUnitName ++ " of"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ "Just minTime -> lift $ put $ Control'Time minTime 0"
         ++ newline ++ tab ++ tab ++ tab ++ tab
         ++ "Nothing -> error \"Nothing to do\""
         ++ newline ++ tab
         ++ "newComponentState'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ " <- lift $ execStateT TopModule.signalUpdate componentState'" ++ topUnitLib ++ "'" ++ topUnitName
         ++ newline ++ tab
         ++ "let updateState state = state { component'" ++ topUnitLib ++ "'" ++ topUnitName ++ " = newComponentState'" ++ topUnitLib ++ "'" ++ topUnitName ++ " }"
         ++ newline ++ tab
         ++ "modify updateState"
         ++ newline ++ tab
         ++ "topRepeat maxTime"
       topFixedFuncsStr =
         "top :: Options -> IO ()\n\
         \top options = do\n\
         \   let maxTime = extractMaxTime options\n\
         \   result <- runExceptT $\n\
         \      execStateT\n\
         \         ( evalStateT\n\
         \            (evalStateT (topInitial maxTime) initialStack)\n\
         \            (Control'Time (STD.STANDARD.mkType'ANON'TIME 0) 0)\n\
         \         )\n\
         \         (Control'SEVERITY_TRACKER 0 0)\n\
         \   case result of\n\
         \      Right res -> putStrLn $ show res\n\
         \      Left (Control'SEVERITY_FAILURE str) -> putStrLn $ \"Severity Failure: \" ++ str\n\
         \\n\
         \data Options = Options\n\
         \   { maxTime :: String\n\
         \   }\n\
         \\n\
         \extractMaxTime :: Options -> STD.STANDARD.Type'ANON'TIME\n\
         \extractMaxTime (Options timeStr) =\n\
         \   let partitionTime :: String -> (String,String) -> (String,String)\n\
         \       partitionTime (chr:others) ([],[])\n\
         \         | isAlpha chr = error \"Invalid formatting of max-time argument: Must have a numerical component\"\n\
         \       partitionTime (chr:others) (nums,[])\n\
         \         | isDigit chr = partitionTime others (chr:nums,[])\n\
         \         | isAlpha chr = partitionTime others (nums,[toUpper chr])\n\
         \       partitionTime (chr:others) (nums,units)\n\
         \         | isAlpha chr = partitionTime others (nums,(toUpper chr):units)\n\
         \         | isDigit chr = error \"Invalid formatting of max-time argument: Invalid unit name formatting\"\n\
         \       partitionTime [] ([],_) = error \"Invalid formatting of max-time argument: Empty argument\"\n\
         \       partitionTime [] (_,[]) = error \"Invalid formatting of max-time argument: Missing unit\"\n\
         \       partitionTime [] (nums,units) = (reverse nums,reverse units)\n\
         \   in if all isAlphaNum timeStr\n\
         \         then let (valueStr,unitStr) = partitionTime timeStr ([],[])\n\
         \              in case Data.Map.Strict.lookup unitStr unitMap of\n\
         \                  Just multiplier -> STD.STANDARD.mkType'ANON'TIME $ (read valueStr) * multiplier\n\
         \                  Nothing -> error \"Invalid formatting of max-time argument: Invalid unit name\"\n\
         \         else error \"Invalid formatting of max-time argument: Must be alphanumeric\"\n\
         \   where unitMap :: Data.Map.Strict.Map String Integer\n\
         \         unitMap = Data.Map.Strict.fromList $\n\
         \            [ (\"FS\",1)\n\
         \            , (\"PS\",1000)\n\
         \            , (\"NS\",1000000)\n\
         \            , (\"US\",1000000000)\n\
         \            , (\"MS\",1000000000000)\n\
         \            , (\"SEC\",1000000000000000)\n\
         \            , (\"MIN\",60000000000000000)\n\
         \            , (\"HR\",3600000000000000000)\n\
         \            ]"
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
         ++ initialFuncStr
         ++ newline ++ newline
         ++ repeatFuncStr
         ++ newline ++ newline
         ++ topFixedFuncsStr
