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

import Lexer.Types.PositionWrapper (PosnWrapper(..))
import Lexer.Alex.Types (AlexPosn(..))
import Netlister.Types.Stores
         ( NetlistStore(..)
         , PackageStore(..)
         , NetlistName(..)
         )
import Netlister.Types.Scope
         ( Scope(..)
         , DeclarationScopeItem(Declare_All)
         )
import Netlister.Builtin.Standard
--import Netlister.Builtin.TextIO

builtinPackages :: PackageStore
builtinPackages =
   [  ( NetlistName "STD" "STANDARD"
      , standardPackage
      )
   --,  ( NetlistName "STD" "TEXTIO"
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
   Scope
      [ "STD", "WORK" ]
      [  ( NetlistName "STD" "STANDARD"
         , PosnWrapper (AlexPn 0 0 0) Declare_All
         )
      ]
