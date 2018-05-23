{-|
   Module      : Manager.Types.Error
   Description : Top level error types

   Types for top level error control of program
-}
module Manager.Types.Error
   ( ConverterError(..)
   ) where

import Lexer.Types.Error
         ( WrappedParserError
         , getLineAndColErrStr
         )
import Lexer.Types.PositionWrapper
import Parser.Netlist.Types.Scope (WrappedScopeConverterError)
import Parser.Netlist.Types.Error (WrappedNetlistError)
import Manager.Filing (FilingError)
import Sim.Types.Error (SimOutputError)

-- |Errors that can occur within the converter
data ConverterError =
   -- |Error checking files
   ConverterError_Filing FilingError
   -- |Error in scope converter
   | ConverterError_Scope WrappedScopeConverterError
   -- |Error in parser
   | ConverterError_Parse WrappedParserError
   -- |Error in converter
   -- When reading the parse tree to convert it to netlist
   | ConverterError_Netlist WrappedNetlistError
   -- |Error for not implemented features
   | ConverterError_NotImplemented (PosnWrapper String)
   -- |Error for simulation output generation
   | ConverterError_Sim SimOutputError
   deriving (Eq)

instance (Show ConverterError) where
   show (ConverterError_Filing fileErr) =
      "Filer: "
      ++ show fileErr
   show (ConverterError_Scope scopeErr) =
      "Scope Conversion: "
      ++ show scopeErr
   show (ConverterError_Parse parseErr) =
      "Parser: "
      ++ show parseErr
   show (ConverterError_Netlist netlistErr) =
      "Netlister: "
      ++ show netlistErr
   show (ConverterError_NotImplemented (PosnWrapper pos info)) =
      "Not implemented: "
      ++ info
      ++ getLineAndColErrStr pos
