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
import Data.Char (toUpper)
import Data.List (intersperse)

import Lexer.Types.PositionWrapper (PosnWrapper(..))
import Lexer.Types.Error (getLineAndColErrStr)
import Parser.Happy.Types
         ( WrappedSimpleName
         , WrappedEnumerationLiteral
         , EnumerationLiteral(..)
         )
import Netlister.Types.Stores (NetlistStore)

-- |Monads required for conversion
type ConversionStack a = StateT NetlistStore (ExceptT ConverterError IO) a

-- |Wrapped netlist error
-- ?? Want to have file name along with position for better error messages
type WrappedNetlistError = PosnWrapper NetlistError

-- |Errors in the netlist converter
data NetlistError =
   -- |Different identifiers in package declaration
   NetlistError_UnmatchedPackageName WrappedSimpleName WrappedSimpleName
   -- |Type name deplaced in two places
   | NetlistError_DuplicateTypes String
   -- |Enum literal has same name as its own type declaration name
   | NetlistError_EnumLitIsTypeName String
   -- |Enum name is in use for a different declaration in the unit
   | NetlistError_InvalidEnumName String
   -- |Duplicate enumeration literals in a type declaration
   | NetlistError_DuplicateEnums [[WrappedEnumerationLiteral]]
   -- |Constant names are duplicates of already defined in unit
   | NetlistError_DuplicateConstantNames [WrappedSimpleName]
   -- |Constant declaration has a resolution function specified
   | NetlistError_ConstantResolutionFunction
   -- |Constant declaration has a constraint specified
   | NetlistError_ConstantConstraint
   -- |Cannot find the type being referred to
   | NetlistError_TypeNotFound String
   -- |Type name cannot be an operator
   | NetlistError_InvalidTypeName_Operator
   deriving (Eq)

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
   show (NetlistError_EnumLitIsTypeName enum) =
      "enum literal is the same as its type declaration name "
      ++ enum
   show (NetlistError_InvalidEnumName name) =
      "enum name is already declared in this unit "
      ++ name
   show (NetlistError_DuplicateEnums enums) =
      "at least one set of duplicate enums have occurred: "
      ++ (concat $ intersperse "; " $ map printEnumGroup enums)
   show (NetlistError_DuplicateConstantNames names) =
      "duplicate constant name within unit: "
      ++ printNames names
      ++ ";"
   show NetlistError_ConstantResolutionFunction =
      "constants do not support resolution functions"
   show NetlistError_ConstantConstraint =
      "constants do not support constraints"
   show (NetlistError_TypeNotFound typeName) =
      "cannot find type named: "
      ++ typeName
   show NetlistError_InvalidTypeName_Operator =
      "type name referred to cannot be an operator "

-- |Print group of enum literals
printEnumGroup :: [WrappedEnumerationLiteral] -> String
printEnumGroup =
   let convToString :: WrappedEnumerationLiteral -> String
       convToString (PosnWrapper pos enum) =
         case enum of
            EnumerationLiteral_Identifier iden ->
               "identifier \""
               ++ (map toUpper iden)
               ++ "\""
            EnumerationLiteral_Char char ->
               "character '"
               ++ [char]
               ++ "'"
         ++ getLineAndColErrStr pos
   in concat . (intersperse ", ") . (map convToString)

-- |Print list of wrapped names
printNames :: [WrappedSimpleName] -> String
printNames = concat . (intersperse ", ") . (map show)
