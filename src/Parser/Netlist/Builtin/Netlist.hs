{-|
   Module      : Parser.Netlist.Builtin.Netlist
   Description : Prebuilt design units
-}
module Parser.Netlist.Builtin.Netlist 
   ( netlist
   , scope
   ) where

import qualified Data.Map.Strict as MapS
import Data.Function ((&))

import Lexer.Types.PositionWrapper (PosnWrapper(..))
import Lexer.Alex.Types (AlexPosn(..))
import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores
         ( NetlistStore(..)
         , PackageStore(..)
         )
import Parser.Netlist.Types.Scope
         ( Scope(..)
         , DeclarationScopeItem(Declare_All)
         )
import Parser.Netlist.Builtin.Standard
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
