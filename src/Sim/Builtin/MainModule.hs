module Sim.Builtin.MainModule
   ( mainModule
   ) where

import Data.Text

mainModule :: Text
mainModule = pack
   "module Main where\n\
   \\n\
   \import Options.Applicative\n\
   \import Data.Semigroup ((<>))\n\
   \\n\
   \import qualified Top\n\
   \\n\
   \main :: IO ()\n\
   \main = Top.top =<< execParser opts\n\
   \   where opts = info (optionsGroup <**> helper)\n\
   \             ( fullDesc\n\
   \            <> progDesc \"Simulation Binary\"\n\
   \            <> header \"Run this binary to run the simulation\"\n\
   \             )\n\
   \\n\
   \optionsGroup :: Parser Top.Options\n\
   \optionsGroup = Top.Options\n\
   \   <$> strOption\n\
   \       ( long \"max-time\"\n\
   \      <> help \"Maximum time for simulation to run (format: \\\"[0-9]+(fs|ps|ns|us|ms|sec|min|hr)\\\")\"\n\
   \       )"
