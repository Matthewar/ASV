module Manager.NewDesignUnit where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Except
         ( runExceptT
         , withExceptT
         , liftEither
         , lift
         )
import System.Exit (exitFailure)
import Data.Char (toUpper)

import qualified Manager.Args as Args (Options(..))
import Manager.Filing
         ( findDesignUnit
         , checkArguments
         )
import Netlister.Types.Stores (NetlistStore)
import Netlister.Types.Stores (NetlistName(..))
import qualified Parser.Parser as Parser (v1987)
import Parser.ErrorTypes (WrappedParserError)
import Parser.Happy.Types (DesignFile)
import Parser.Alex.Functions (runAlex)
import Parser.ErrorTypes (printParserError)
import qualified Netlister.Builtin.Netlist as InitialNetlist (netlist)
import Netlister.Types.Top (ConversionStack,ConverterError(..))
import Netlister.ParseTree (convertTree)

createTop :: Args.Options -> IO ()
createTop options = do
   result <- runExceptT $ do
      withExceptT (ConverterError_Filing) $ checkArguments options
      let (Args.Options workDir ieeeDir topModule) = options
          createNetlistUnit = create workDir ieeeDir [] "WORK" $ map toUpper topModule
      finalNetlist <- execStateT createNetlistUnit InitialNetlist.netlist

      -- ?? Other steps

      return finalNetlist
   case result of
      Left err -> printError err
      Right result -> return ()
-- Call create function for topmost file
--
-- Need state monad of current scoped and already parsed objects to be setup?
-- Need to use StateM or whatever to combine state with Either monad?

printError :: ConverterError -> IO ()
printError error = do
   let genericMessage = "Error: "
       customMessage = show error
       fullMessage = genericMessage ++ customMessage
   putStrLn fullMessage
   exitFailure

create :: FilePath -> FilePath -> [NetlistName] -> String -> String -> ConversionStack ()
create workPath ieeePath dependencies library unitName = do
   filePath <- lift $ withExceptT (ConverterError_Filing) $ findDesignUnit workPath ieeePath library unitName
   fileContents <- liftIO $ readFile filePath
   parseTree <- lift $ withExceptT (ConverterError_Parse) $ liftEither $ parse fileContents
   let netlistName = NetlistName library unitName
   convertTree (create workPath ieeePath (netlistName:dependencies)) netlistName dependencies parseTree
   -- ?? Build simulation files
   return ()
   
-- Check dependencies
-- Parse new files

parse :: String -> Either WrappedParserError DesignFile
parse s = runAlex s Parser.v1987
