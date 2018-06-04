module Sim.Output.Constants
   ( outputConstants
   ) where

import Control.Monad.Except
         ( ExceptT
         , throwError
         , liftIO
         )
import qualified Data.Map.Strict as MapS
import Numeric (showFFloat)

import Parser.Netlist.Types.Representation
         ( Constant(..)
         , Value(..)
         , Subtype(..)
         )
import Parser.Netlist.Types.Stores (ConstantStore)
import Manager.Types.Error (ConverterError(..))
import Sim.Output.Names (showEnum)
import Sim.Types.Error (SimOutputError(..))

newline = "\n"
tab = "   "

outputConstants :: FilePath -> ConstantStore -> ExceptT ConverterError IO ()
outputConstants fileName = (outputConstants' fileName) . MapS.toList

outputConstants' :: FilePath -> [(String,Constant)] -> ExceptT ConverterError IO ()
outputConstants' fileName ((name,Constant (packageName,typeName) subtype value):others) = do
   let constName = "constant'" ++ name
       constTypeStr = show packageName ++ ".Type'" ++ typeName
       constTypeDeclStr = constName ++ " :: " ++ constTypeStr
   extractedValue <- case value of
                        Just val -> return val
                        Nothing -> throwError $ ConverterError_Sim $ SimErr_ConstantNoValue name
   let (baseTypePackage,baseTypeName) = case subtype of
                                          EnumerationSubtype _ typeName _ _ -> typeName
                                          IntegerSubtype _ typeName _ -> typeName
                                          FloatingSubtype _ typeName _ -> typeName
                                          PhysicalSubtype _ typeName _ _ _ -> typeName
       makeBaseTypeStr = show baseTypePackage ++ ".mkType'" ++ baseTypeName
       makeSubtypeStr = show packageName ++ ".mkType'" ++ typeName
       valueStr =
         case extractedValue of
            Value_Enum _ enum ->
               let baseTypeStr = show baseTypePackage ++ ".Type'" ++ baseTypeName
               in makeSubtypeStr ++ " " ++ showEnum baseTypeStr enum
            Value_Int intVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ show intVal
            Value_Float floatVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ (showFFloat Nothing floatVal "")
            Value_Physical physVal -> makeSubtypeStr ++ " $ " ++ makeBaseTypeStr ++ " " ++ show physVal
            --Value_Array
       constValueStr = constName ++ " = " ++ valueStr
   liftIO $ appendFile fileName $ newline ++ constTypeDeclStr ++ newline ++ constValueStr ++ newline
   outputConstants' fileName others
--   where convertValue :: Value -> String
--         convertValue 
outputConstants' _ [] = return ()
