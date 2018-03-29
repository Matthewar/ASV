module Manager.NewDesignUnit where

import Control.Monad.Trans.State

import qualified Manager.Args as Args (Options)
import Netlister.TypeData (NetlistStore)
import qualified Parser.Parser as Parser (v1987)
import Parser.ErrorTypes (WrappedParserError)
import Parser.Happy.Types (DesignFile)
import Parser.Alex.Functions (runAlex)

createTop :: Args.Options -> StateT NetlistStore IO ()
createTop options = do
   return ()
-- Call create function for topmost file
--
-- Need state monad of current scoped and already parsed objects to be setup?
-- Need to use StateM or whatever to combine state with Either monad?

--create :: String -> StateT NetlistStore IO
-- Confirm file name
-- Parse file
-- Check dependencies
-- Parse new files

parse :: String -> Either WrappedParserError DesignFile
parse s = runAlex s Parser.v1987
