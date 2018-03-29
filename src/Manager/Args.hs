module Manager.Args where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
   { workDir :: FilePath
   , ieeeDir :: FilePath
   , topModule :: FilePath
   }

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
       ( long "top"
      <> help "Top Module"
      <> metavar "TOP_ENTITY"
       )
