{-|
   Module      : Parser.Netlist.Types.Error
   Description : Netlist error types

   Types for netlist errors
-}
module Parser.Netlist.Types.Error
         ( NetlistError(..)
         , WrappedNetlistError
         ) where

import Data.Char (toUpper)
import Data.List (intersperse)

import Lexer.Types.PositionWrapper (PosnWrapper(..))
import Lexer.Types.Error (getLineAndColErrStr)
import Parser.Netlist.Types.Representation (Enumerate)

-- |Wrapped netlist error
-- ?? Want to have file name along with position for better error messages
type WrappedNetlistError = PosnWrapper NetlistError

-- |Errors in the netlist converter
data NetlistError =
--   -- |Different identifiers in package declaration
--   NetlistError_UnmatchedPackageName WrappedSimpleName WrappedSimpleName
   -- |Type name deplaced in two places
   NetlistError_DuplicateTypeName String
   -- |Enum literal has same name as its own type declaration name
   | NetlistError_EnumLitIsTypeName String
   -- |Enum name is in use for a different declaration in the unit
   | NetlistError_InvalidEnumName String
   -- |Duplicate enumeration literals in a type declaration
   | NetlistError_DuplicateEnums Enumerate
--   -- |Constant names are duplicates of already defined in unit
--   | NetlistError_DuplicateConstantNames [WrappedSimpleName]
--   -- |Constant declaration has a resolution function specified
--   | NetlistError_ConstantResolutionFunction
--   -- |Constant declaration has a constraint specified
--   | NetlistError_ConstantConstraint
--   -- |Cannot find the type being referred to
--   | NetlistError_TypeNotFound String
--   -- |Type name cannot be an operator
--   | NetlistError_InvalidTypeName_Operator
   deriving (Eq)

instance (Show NetlistError) where
--   show  (NetlistError_UnmatchedPackageName
--            (PosnWrapper namePos1 name1)
--            (PosnWrapper namePos2 name2)
--         ) =
--      "second identifier: \""
--      ++ name1
--      ++ "\""
--      ++ getLineAndColErrStr namePos1
--      ++ ", does not match first identifier: \""
--      ++ name2
--      ++ "\""
--      ++ getLineAndColErrStr namePos2
--      ++ " in package"
   show (NetlistError_DuplicateTypeName typeName) =
      "duplicate type name within unit: "
      ++ typeName
   show (NetlistError_EnumLitIsTypeName enum) =
      "enum literal is the same as its type declaration name "
      ++ enum
   show (NetlistError_InvalidEnumName name) =
      "name is already declared in this unit "
      ++ name
   show (NetlistError_DuplicateEnums enum) =
      "enum name is already declared in this type"
      ++ show enum
--   show (NetlistError_DuplicateConstantNames names) =
--      "duplicate constant name within unit: "
--      ++ printNames names
--      ++ ";"
--   show NetlistError_ConstantResolutionFunction =
--      "constants do not support resolution functions"
--   show NetlistError_ConstantConstraint =
--      "constants do not support constraints"
--   show (NetlistError_TypeNotFound typeName) =
--      "cannot find type named: "
--      ++ typeName
--   show NetlistError_InvalidTypeName_Operator =
--      "type name referred to cannot be an operator "
--
---- |Print list of wrapped names
--printNames :: [WrappedSimpleName] -> String
--printNames = concat . (intersperse ", ") . (map show)
