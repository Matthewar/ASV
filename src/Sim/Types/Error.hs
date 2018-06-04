module Sim.Types.Error
   ( SimOutputError(..)
   ) where

import Parser.Netlist.Types.Representation (NetlistName)

data SimOutputError =
   -- |No value was specified for the constant in either the package or its body
   SimErr_ConstantNoValue String
   -- |Cannot find an entity or architecture with the specified top module name
   | SimErr_NoEntityOrArchWithTopModuleName NetlistName
   -- |Top module generics must have default values
   | SimErr_TopGenericsWithoutDefaultValues
   deriving (Eq)
