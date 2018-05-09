module Manager.NewDesignUnit where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
         ( StateT
         , execStateT
         , withStateT
         )
import Control.Monad.Except
         ( ExceptT
         , runExceptT
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
import Lexer.Types.Error (WrappedParserError)
import Lexer.Alex.Functions (runAlex)
import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores (NetlistStore)
import qualified Parser.Netlist.Builtin.Netlist as InitialNetlist (netlist)
import Manager.Types.Error (ConverterError(..))
--import Netlister.ParseTree (convertTree)
import Parser.Functions.Parse.DesignFile (parseDesignFile)
import Sim.Top (outputTop)

createTop :: Args.Options -> IO ()
createTop options = do
   result <- runExceptT $ do
      withExceptT (ConverterError_Filing) $ checkArguments options
      let (Args.Options workDir ieeeDir buildDir topModule) = options
          createNetlistUnit = create workDir ieeeDir [] "WORK" $ map toUpper topModule
      finalNetlist <- execStateT createNetlistUnit InitialNetlist.netlist
      outputTop buildDir finalNetlist

      -- ?? Other steps

      return finalNetlist
   case result of
      Left err -> printError err
      Right result -> return () --putStrLn $ show result
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

create :: FilePath -> FilePath -> [NetlistName] -> String -> String -> StateT NetlistStore (ExceptT ConverterError IO) ()
create workPath ieeePath dependencies library unitName = do
   filePath <- lift $ withExceptT (ConverterError_Filing) $ findDesignUnit workPath ieeePath library unitName
   fileContents <- liftIO $ readFile filePath
   let netlistName = NetlistName library unitName
   runAlex fileContents $ parseDesignFile (create workPath ieeePath (netlistName:dependencies)) library dependencies
   -- ?? Build simulation files
   return ()
   
-- Check dependencies
-- Parse new files
