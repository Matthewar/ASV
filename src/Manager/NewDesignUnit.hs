module Manager.NewDesignUnit where

import qualified Manager.Args as Args (Options)

createTop :: Args.Options -> IO ()
createTop options = do
-- Call create function for topmost file

create :: String -> IO ()
-- Confirm file name
-- Parse file
-- Check dependencies
-- Parse new files

parse :: String -> Either WrapperParserError DesignFile
parse s = runAlex s Parser.v1987
