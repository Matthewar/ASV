module Manager.Filing where

import System.Directory (doesDirectoryExist,doesFileExist,findFile)
import System.Exit (exitFailure)

import qualified Manager.Args as Args (Options)

-- |Check file arguments to ensure they are valid directories
checkArguments :: Args.Options -> IO ()
checkArguments options =
   let workDir = workDir options
       validWorkDir = doesDirectoryExist workDir
       ieeeDir = ieeeDir options
       validIEEEDir = doesDirectoryExist ieeeDir
       validDirectories = validWorkDir && validIEEEDir
   in do
         if not validWorkDir then
            putStrLn $ "Error: Invalid \"work\" library directory: " ++ workDir
         else
            putStrLn "Success: Work directory found"
         if not validIEEEDir then
            putStrLn $ "Invalid \"ieee\" library directory: " ++ ieeeDir
         else
            putStrLn "Success: IEEE directory found"
         if not validDirectories then exitFailure else return

-- |Take name of module and library, if file exists then return path
findDesignUnit :: String -> String -> IO (Maybe FilePath)
findDesignUnit library unitName =
   let fileName = 
