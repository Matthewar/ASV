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

instance Show SimOutputError where
   show (SimErr_ConstantNoValue constName) =
      "no value was specified for the deferred constant "
      ++ constName
   show (SimErr_NoEntityOrArchWithTopModuleName packageName) =
      "cannot find an entity or architecture with the name "
      ++ show packageName
   show SimErr_TopGenericsWithoutDefaultValues =
      "the top level module generics must have default values provided"
