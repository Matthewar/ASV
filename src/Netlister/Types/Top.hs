{-|
   Module      : Netlister.Types.Top
   Description : Netlister top level types

   Types for top level control of netlist
-}
module Netlister.Types.Top
         ( ConversionStack
         , ConverterError(..)
         , NetlistError(..)
         ) where

import Control.Monad.Trans.State (StateT)
import Control.Monad.Except
         ( ExceptT
         , MonadError
         , throwError
         )

import Netlister.Types.Stores (NetlistStore)
import Netlister.Types.Scope (WrappedScopeConverterError)
import Parser.Alex.BaseTypes (AlexPosn)
import Parser.PositionWrapper (PosnWrapper(..))
import Manager.Filing (FilingError)
import Parser.ErrorTypes
         ( WrappedParserError
         , getLineAndColErrStr
         )
import Parser.Happy.Types
         ( WrappedSimpleName
         , WrappedEnumerationLiteral
         )

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
   | ConverterError_Netlist WrappedNetlistError
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

-- |Wrapped netlist error
-- ?? Want to have file name along with position for better error messages
type WrappedNetlistError = PosnWrapper NetlistError

-- |Errors in the netlist converter
data NetlistError =
   -- |Different identifiers in package declaration
   NetlistError_UnmatchedPackageName WrappedSimpleName WrappedSimpleName
   -- |Type name deplaced in two places
   | NetlistError_DuplicateTypes String
   -- |Duplicate enumeration literals in a type declaration
   | NetlistError_DuplicateEnums [[WrappedEnumerationLiteral]]

instance (Show NetlistError) where
   show  (NetlistError_UnmatchedPackageName
            (PosnWrapper namePos1 name1)
            (PosnWrapper namePos2 name2)
         ) =
      "second identifier: \""
      ++ name1
      ++ "\""
      ++ getLineAndColErrStr namePos1
      ++ ", does not match first identifier: \""
      ++ name2
      ++ "\""
      ++ getLineAndColErrStr namePos2
      ++ " in package"
   show (NetlistError_DuplicateTypes typeName) =
      "duplicate type name within unit: "
      ++ typeName
   --show (NetlistError_DuplicateEnums
