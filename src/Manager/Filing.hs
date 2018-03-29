module Manager.Filing where

import System.Directory (doesDirectoryExist,doesFileExist,findFile)
import System.Exit (exitFailure)

import qualified Manager.Args as Args (Options(..))

-- |Check file arguments to ensure they are valid directories
checkArguments :: Args.Options -> IO ()
checkArguments options = do
   let workDir = Args.workDir options
   validWorkDir <- doesDirectoryExist workDir
   let ieeeDir = Args.ieeeDir options
   validIEEEDir <- doesDirectoryExist ieeeDir
   let validDirectories = validWorkDir && validIEEEDir
   if not validWorkDir then
      putStrLn $ "Error: Invalid \"work\" library directory: " ++ workDir
   else
      putStrLn "Success: Work directory found"
   if not validIEEEDir then
      putStrLn $ "Error: Invalid \"ieee\" library directory: " ++ ieeeDir
   else
      putStrLn "Success: IEEE directory found"
   if not validDirectories then exitFailure else return ()

-- |Take name of module and library, if file exists then return path
findDesignUnit :: FilePath -> FilePath -> String -> String -> IO (Maybe FilePath)
findDesignUnit library unitName =
   let fileName = 
