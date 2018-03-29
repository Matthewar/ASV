{-|
   Module      : Netlister.Builtin.Netlist
   Description : Prebuilt design units
-}
module Netlister.Builtin.Netlist 
   ( netlist
   , scope
   ) where

import qualified Data.Map.Strict as MapS
import Data.Function ((&))

import Netlister.TypeData
         ( NetlistStore(..)
         , PackageStore(..)
         )
import Netlister.Types.Scope
         ( Scope
         , DeclarationScope(..)
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

scope :: Scope
scope =
   [  ( "STD"
      ,  [  ( "STANDARD"
            , AllDeclares
            )
         ]
         & MapS.fromList
      )
   ,  ( "WORK"
      , MapS.empty
      )
   ]
   & MapS.fromList
