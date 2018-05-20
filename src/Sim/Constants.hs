module Sim.Constants
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
import Sim.Types (showEnum)
import Sim.Error (SimOutputError(..))

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
   let valueStr =
         case extractedValue of
            Value_Enum _ enum ->
               let baseTypeStr = case subtype of EnumerationSubtype _ (baseTypePackage,baseTypeName) _ _ -> show baseTypePackage ++ ".Type'" ++ baseTypeName
               in showEnum baseTypeStr enum
            Value_Int intVal -> show packageName ++ ".mkType'" ++ typeName ++ " " ++ show intVal
            Value_Float floatVal -> show packageName ++ ".mkType'" ++ typeName ++ " " ++ (showFFloat Nothing floatVal "")
            Value_Physical physVal -> show packageName ++ ".mkType'" ++ typeName ++ " " ++ show physVal
            --Value_Array
       constValueStr = constName ++ " = " ++ valueStr
   liftIO $ appendFile fileName $ newline ++ constTypeDeclStr ++ newline ++ constValueStr ++ newline
   outputConstants' fileName others
--   where convertValue :: Value -> String
--         convertValue 
outputConstants' _ [] = return ()
