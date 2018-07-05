{-|
   Module      : Parser.Netlist.Types.Monad
   Description : Netlist stack type

   Stack for netlist design
|-}
module Parser.Netlist.Types.Monad
   ( NetlistStack
   ) where

import Control.Monad.Trans.State (StateT)
import Control.Monad.Except (ExceptT)

import Parser.Netlist.Types.Stores (NetlistStore)
import Manager.Types.Error (ConverterError)

-- |Netlist monad
-- Contains access to netlist design
type NetlistStack a = StateT NetlistStore (ExceptT ConverterError IO) a
