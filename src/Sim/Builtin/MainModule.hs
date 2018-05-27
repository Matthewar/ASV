module Sim.Builtin.MainModule
   ( mainModule
   ) where

import Data.Text

mainModule :: Text
mainModule = pack
   "module Main where\n\
   \\n\
   \import qualified Top\n\
   \\n\
   \main :: IO ()\n\
   \main = Top.top"
