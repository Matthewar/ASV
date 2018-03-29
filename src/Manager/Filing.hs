module Manager.Filing
   ( checkArguments
   , Library(..)
   , findDesignUnit
   ) where

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

data Library =
   IEEELibrary
   | WorkLibrary

-- |Take name of module and library, if file exists then return path
findDesignUnit :: FilePath -> FilePath -> Library -> String -> IO FilePath
findDesignUnit workDir _ WorkLibrary unitName = findDesignUnit' workDir unitName
findDesignUnit _ ieeeDir IEEELibrary unitName = findDesignUnit' ieeeDir unitName

-- |Find unit within chosen directory
-- ?? NOTE: Temporary hack is to force unit to be defined in unitName.vhd
findDesignUnit' :: FilePath -> String -> IO FilePath
findDesignUnit' libDir unitName = do
   let fileName = libDir ++ unitName ++ ".vhd"
   checkFile <- doesFileExist fileName
   if checkFile then do
      putStrLn $ "Info: Found file " ++ fileName
      return fileName
   else do
      putStrLn $ "Error: Unable to find file: " ++ fileName
      exitFailure
