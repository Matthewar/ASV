{-|
   Module      : Netlister.Types.Top
   Description : Netlister top level types

   Types for top level control of netlist
-}
module Netlister.Types.Top
         ( ConversionStack
         , ConverterError(..)
         , throwWrappedError
         ) where

import Control.Monad.Trans.State (StateT)
import Control.Monad.Except (ExceptT)

import Netlister.Types.Stores (NetlistStore)
import Netlister.Types.Scope (WrappedScopeConverterError)
import Parser.Alex.BaseTypes (AlexPosn)
import Parser.PositionWrapper (PosnWrapper(..))
import Manager.Filing (FilingError)
import Parser.ErrorTypes (WrappedParserError)

-- |Monads required for conversion
type ConversionStack a = StateT NetlistStore (ExceptT ConverterError IO) a

-- |Errors that can occur within the converter
data ConverterError =
   -- |Error checking files
   ConverterError_Filing FilingError
   -- |Error in scope converter
   | ConverterError_Scope WrappedScopeConverterError
   -- |Error in parser
   | ConverterError_Parse WrappedParserError

throwWrappedError :: AlexPosn -> a -> Either (PosnWrapper a) b
throwWrappedError pos error = Left $ PosnWrapper { getPos = pos, unPos = error }
