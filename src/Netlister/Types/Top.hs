{-|
   Module      : Netlister.Types.Top
   Description : Netlister top level types

   Types for top level control of netlist
-}
module Netlister.Types.Top
         ( ConversionStack
         ) where

import Control.Monad.Trans.State (StateT)

import Netlister.Types.Stores (NetlistStore)

type ConversionStack = StateT NetlistStore IO ()
