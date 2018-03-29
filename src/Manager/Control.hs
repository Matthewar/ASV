module Manager.Control 
   ( control
   ) where

import Control.Monad.Trans.State (execStateT)

import Manager.Args (Options)
import qualified Manager.Filing as Filing
import qualified Manager.NewDesignUnit as Design (createTop)
import qualified Netlister.Builtin.Netlist as InitialNetlist (netlist)

control :: Options -> IO ()
control options = do
   Filing.checkArguments options
   finalNetlist <- execStateT (Design.createTop options) InitialNetlist.netlist
   return ()
