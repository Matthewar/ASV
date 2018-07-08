{-|
   Module      : Manager.Args
   Description : Command line argument parser and associated types
|-}
module Manager.Args where

import Options.Applicative
import Data.Semigroup ((<>))

-- |All data collated from command line arguments
data Options = Options {
      -- |Work directory argument
      -- Where the VHDL WORK library files are stored
      -- Arg: "--work-dir"
      -- Default: "work/"
      workDir :: FilePath,
      -- |IEEE directory argument
      -- Where the VHDL IEEE library files are stored
      -- Arg: "--ieee-dir"
      -- Default: "ieee/"
      ieeeDir :: FilePath,
      -- |Build directory argument
      -- Where the tool outputs simulation files to
      -- Arg: "--build-dir"
      -- Default: "build/"
      buildDir :: FilePath,
      -- |Top module argument
      -- The name of the top level VHDL design entity
      -- Arg: "--top"
      -- Default: N/A
      topModule :: String
      }

-- |Command line argument parser
-- For details of the argument data stored, see type 'Options'
optionsGroup :: Parser Options
optionsGroup = Options
   <$> strOption
       ( long "work-dir"
      <> help "Work directory"
      <> showDefault
      <> value "work/"
      <> metavar "WORK_LIB_PATH"
      <> action "directory"
       )
   <*> strOption
       ( long "ieee-dir"
      <> help "IEEE Library Directory"
      <> showDefault
      <> value "ieee/"
      <> metavar "IEEE_LIB_PATH"
      <> action "directory"
       )
   <*> strOption
      ( long "build-dir"
     <> help "Output build directory"
     <> showDefault
     <> value "build/"
     <> metavar "BUILD_PATH"
     <> action "directory"
      )
   <*> strOption
       ( long "top"
      <> help "Top Module"
      <> metavar "TOP_ENTITY"
       )
