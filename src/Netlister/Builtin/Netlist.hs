{-|
   Module      : Netlister.Builtin.Netlist
   Description : Prebuilt design units
-}
module Netlister.Builtin.Netlist 
   ( netlist
   ) where

import qualified Data.Map.Strict as MapS
import Data.Function ((&))

import Netlister.TypeData
         (NetlistStore(..)
         , PackageStore(..)
         )
import Netlister.Builtin.Standard
--import Netlister.Builtin.TextIO

builtinPackages :: PackageStore
builtinPackages =
   [  ( ["STD","STANDARD"]
      , standardPackage
      )
   --,  ( ["STD","TEXTIO"]
   --   , textioPackage
   --   )
   ]
   & MapS.fromList

netlist :: NetlistStore
netlist =
   NetlistStore
      { packages = builtinPackages
      }
