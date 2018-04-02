{-|
   Module      : Netlister.Types.Top
   Description : Netlister top level types

   Types for top level control of netlist
-}
module Netlister.Types.Top
         ( ConversionStack
         , ConverterError(..)
         , throwWrappedError
         , NetlistError(..)
         ) where

import Control.Monad.Trans.State (StateT)
import Control.Monad.Except (ExceptT)

import Netlister.Types.Stores (NetlistStore)
import Netlister.Types.Scope (WrappedScopeConverterError)
import Parser.Alex.BaseTypes (AlexPosn)
import Parser.PositionWrapper (PosnWrapper(..))
import Manager.Filing (FilingError)
import Parser.ErrorTypes
         ( WrappedParserError
         , getLineAndColErrStr
         )
import Parser.Happy.Types (WrappedSimpleName)

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
   -- |Error in converter
   -- When reading the parse tree to convert it to netlist
   | ConverterError_Netlist NetlistError
   -- |Error for not implemented features
   | ConverterError_NotImplemented WrappedSimpleName

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

-- |Wrap error type in its position and throw it as an error
throwWrappedError :: AlexPosn -> a -> Either (PosnWrapper a) b
throwWrappedError pos error = Left $ PosnWrapper { getPos = pos, unPos = error }

-- |Errors in the netlist converter
data NetlistError =
   -- |Different identifiers in package declaration
   NetlistError_UnmatchedPackageName AlexPosn WrappedSimpleName WrappedSimpleName

instance (Show NetlistError) where
   show  (NetlistError_UnmatchedPackageName
            pos
            (PosnWrapper namePos1 name1)
            (PosnWrapper namePos2 name2)
         ) =
      "Package declaration "
      ++ getLineAndColErrStr pos
      ++ ". Second identifier "
      ++ name2
      ++ getLineAndColErrStr namePos2
      ++ ". Doesn't match expected identifier "
      ++ name1
      ++ getLineAndColErrStr namePos1
