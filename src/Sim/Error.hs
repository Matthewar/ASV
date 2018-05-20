module Sim.Error
   ( SimOutputError(..)
   ) where

data SimOutputError =
   -- |No value was specified for the constant in either the package or its body
   SimErr_ConstantNoValue String
   deriving (Eq)
