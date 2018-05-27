module Sim.Output.Components
   ( outputGenerics
   ) where

import System.FilePath ((</>))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )
import Numeric (showFFloat)

import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Subtype(..)
         , Value(..)
         , Generic(..)
         )
import Parser.Netlist.Types.Stores (ScopeStore)
import Sim.Output.Types (showEnum)
import Sim.Output.Imports (outputImports)
import Sim.Output.Cabal (outputCabalModule)
import Manager.Types.Error (ConverterError)

newline = "\n"
tab = "   "

outputGenerics :: FilePath -> NetlistName -> String -> [Generic] -> ScopeStore -> ExceptT ConverterError IO ()
outputGenerics buildDir netlistName@(NetlistName lib name) componentName generics scope = do
   let srcDir = buildDir </> "src"
       genericFileName = srcDir </> lib </> name ++ "'COMPONENT'" ++ componentName ++ "'GENERICS.hs"
   outputCabalModule buildDir $ NetlistName lib $ name ++ "'COMPONENT'" ++ componentName ++ "'GENERICS"
   liftIO $ writeFile genericFileName $
      "module "
      ++ show netlistName ++ "'COMPONENT'" ++ componentName ++ "'GENERICS"
      ++ " where"
      ++ newline
   outputImports genericFileName scope
   outputGenerics' genericFileName netlistName componentName generics

outputGenerics' :: FilePath -> NetlistName -> String -> [Generic] -> ExceptT ConverterError IO ()
outputGenerics' fileName netlistName componentName (generic:generics) = do
   liftIO $ appendFile fileName $
      concat $ map (\s -> newline ++ s) $ printGeneric netlistName componentName generic
   outputGenerics' fileName netlistName componentName generics
   where printGeneric :: NetlistName -> String -> Generic -> [String]
         printGeneric unitName componentName (Generic name (subtypePackage,subtypeName) subtype (Just value)) =
            let package = if unitName == subtypePackage
                           then show unitName ++ "'COMPONENT'" ++ componentName ++ "'GENERICS"
                           else show subtypePackage
                (baseTypePackage,baseTypeName) = case subtype of
                                                   EnumerationSubtype _ typeName _ _ -> typeName
                                                   IntegerSubtype _ typeName _ -> typeName
                                                   FloatingSubtype _ typeName _ -> typeName
                                                   PhysicalSubtype _ typeName _ _ _ -> typeName
                makeBaseTypeStr = show baseTypePackage ++ ".mkType'" ++ baseTypeName
                makeSubtypeStr = show subtypePackage ++ ".mkType'" ++ subtypeName
                valueStr =
                  case value of
                     Value_Enum _ enum ->
                        let baseTypeStr = show baseTypePackage ++ ".Type'" ++ baseTypeName
                        in makeSubtypeStr ++ " " ++ showEnum baseTypeStr enum
                     Value_Int intVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ show intVal
                     Value_Float floatVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ (showFFloat Nothing floatVal "")
                     Value_Physical physVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ show physVal
            in [ "generic'" ++ name ++ " :: " ++ package ++ ".Type'" ++ subtypeName
               , "generic'" ++ name ++ " = " ++ valueStr
               ]
outputGenerics' _ _ _ [] = return ()
