module Manager.NewDesignUnit where

import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (execStateT)
import System.Exit (exitFailure)

import qualified Manager.Args as Args (Options(..))
import Manager.Filing
         ( Library(..)
         , findDesignUnit
         , checkArguments
         )
import Netlister.TypeData (NetlistStore)
import qualified Parser.Parser as Parser (v1987)
import Parser.ErrorTypes (WrappedParserError)
import Parser.Happy.Types (DesignFile)
import Parser.Alex.Functions (runAlex)
import Parser.ErrorTypes (printParserError)
import qualified Netlister.Builtin.Netlist as InitialNetlist (netlist)

createTop :: Args.Options -> IO ()
createTop options = do
   checkArguments options
   let (Args.Options workDir ieeeDir topModule) = options
       createNetlistUnit = create workDir ieeeDir WorkLibrary topModule
   finalNetlist <- execStateT createNetlistUnit InitialNetlist.netlist
   return ()
-- Call create function for topmost file
--
-- Need state monad of current scoped and already parsed objects to be setup?
-- Need to use StateM or whatever to combine state with Either monad?

create :: FilePath -> FilePath -> Library -> String -> StateT NetlistStore IO ()
create workPath ieeePath library unitName = do
   filePath <- liftIO $ findDesignUnit workPath ieeePath library unitName
   fileContents <- liftIO $ readFile filePath
   parseTree <- case parse fileContents of
                  Left err -> liftIO $ do
                     putStrLn $ printParserError err
                     exitFailure
                  Right result -> return result
   return ()
   
-- Check dependencies
-- Parse new files

parse :: String -> Either WrappedParserError DesignFile
parse s = runAlex s Parser.v1987
