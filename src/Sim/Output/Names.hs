module Sim.Output.Names
   ( printComponentActualName
   , printComponentSafeName
   ) where

import Data.List (intersperse)

-- |Print component name as it should appear to the designer
printComponentActualName :: [String] -> String
printComponentActualName = concat . (intersperse ".")

-- |Print component name without period separators
-- NOTE: Period separators are used in Haskell qualified names so shouldn't be used in function names, etc.
printComponentSafeName :: [String] -> String
printComponentSafeName = concat . (intersperse "'")
