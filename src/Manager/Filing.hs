module Manager.Filing
   ( checkArguments
   , findDesignUnit
   , FilingError(..)
   ) where

import System.Directory (doesDirectoryExist,doesFileExist,findFile)
import Control.Monad.Except (ExceptT,throwError)
import Control.Monad.Trans (liftIO)
import Data.List (intersperse)

import qualified Manager.Args as Args (Options(..))

-- |Filing error
-- Errors that occur from reading files
data FilingError =
   -- |Cannot find library directory
   FilingError_InvalidLibDir [String]
   -- |Cannot find file
   | FilingError_NoFoundFile String
   deriving (Eq)

instance (Show FilingError) where
   show (FilingError_InvalidLibDir libs) =
      "Invalid library directory: "
      ++ listLibs libs
   show (FilingError_NoFoundFile fileName) =
      "Unable to find file: "
      ++ fileName

-- |Convert list of library names to human readable list
-- Intersperse with commas
listLibs :: [String] -> String
listLibs = concat . (intersperse ", ") . (map (\lib -> "\"" ++ lib ++ "\""))

-- |Check file arguments to ensure they are valid directories
checkArguments :: Args.Options -> ExceptT FilingError IO ()
checkArguments options = do
   let workDir = Args.workDir options
   validWorkDir <- liftIO $ doesDirectoryExist workDir
   let ieeeDir = Args.ieeeDir options
   validIEEEDir <- liftIO $ doesDirectoryExist ieeeDir
   case (validWorkDir,validIEEEDir) of
      (False,False) -> throwError $ FilingError_InvalidLibDir [workDir,ieeeDir]
      (False,True) -> throwError $ FilingError_InvalidLibDir [workDir]
      (True,False) -> throwError $ FilingError_InvalidLibDir [ieeeDir]
      (True,True) -> liftIO $ putStrLn "Success: Library directories found"

-- |Take name of module and library, if file exists then return path
findDesignUnit :: FilePath -> FilePath -> String -> String -> ExceptT FilingError IO FilePath
findDesignUnit workDir _ "WORK" unitName = findDesignUnit' workDir unitName
findDesignUnit _ ieeeDir "IEEE" unitName = findDesignUnit' ieeeDir unitName

-- |Find unit within chosen directory
-- ?? NOTE: Temporary hack is to force unit to be defined in unitName.vhd
findDesignUnit' :: FilePath -> String -> ExceptT FilingError IO FilePath
findDesignUnit' libDir unitName = do
   let fileName = libDir ++ unitName ++ ".vhd"
   checkFile <- liftIO $ doesFileExist fileName
   if checkFile then do
      liftIO $ putStrLn $ "Info: Found file " ++ fileName
      return fileName
   else throwError $ FilingError_NoFoundFile fileName
