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
import Data.Int (Int64)

import Lexer.Types.PositionWrapper (PosnWrapper(..))
import Lexer.Types.Error (getLineAndColErrStr)
import Parser.Netlist.Types.Representation
         ( Enumerate
         , IntegerRange
         , FloatRange
         )

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
   -- |Could not find any simple expressions that matched the type profile
   | NetlistError_FailedSimpleExpression -- ?? Add type data of potential inputs recognised
   -- |Physical literal unit name cannot be found
   | NetlistError_PhysicalUnitNotFound String
   -- |Enumerate character cannot be found
   | NetlistError_UnrecognisedEnumChar Char
   -- |Cannot recognise primary name
   | NetlistError_UnrecognisedName String
   -- |Deferred constant in a locally static expression
   | NetlistError_DeferredConst String
   -- |Expected an integer or floating point value in the left bound of a range definition
   | NetlistError_ExpectedIntOrFloatLeftBoundRange
   -- |Expected an integer or floating point value in the right bound of a range definition
   | NetlistError_ExpectedIntOrFloatRightBoundRange
   -- |Bound types of a range type definition do not match
   | NetlistError_RangeTypeNoMatch
   -- |Cannot determine type of the left bound of a range definition from context
   | NetlistError_CannotInferValueFromContextInRangeTypeLeftBound
   -- |Cannot determine type of the right bound of a range definition from context
   | NetlistError_CannotInferValueFromContextInRangeTypeRightBound
   -- |Cannot have a floating range in a physical type definition (must be integer)
   | NetlistError_FloatingRangeInPhysicalDefinition
   -- |Base unit has same name as its own type declaration name
   | NetlistError_BaseUnitNameIsTypeName String
   -- |Physical unit name already exists in the unit
   | NetlistError_InvalidPhysLitName String
   -- |Secondary unit has same name as its own type declaration name
   | NetlistError_SecondaryUnitNameIsTypeName String
   -- |Secondary unit name already exists in the physical type
   | NetlistError_DuplicateSecondaryUnitInPhysType String
   -- |No unit in physical type definition with this name
   | NetlistError_UnrecognisedPhysicalUnitName String
   -- |Could not find any terms that matched the type profile
   | NetlistError_FailedTerm -- ?? Add type data of potential inputs recognised
   -- |Values in integer range are out of acceptable bounds
   | NetlistError_IntegerTypeOutOfBounds Integer Integer
   -- |Values in floating range are out of acceptable bounds
   | NetlistError_FloatingTypeOutOfBounds Double Double
   -- |Could not find any factors that matched the type profile
   | NetlistError_FailedFactor -- ?? Add type data of potential inputs recognised
   -- |Could not find any relations that matched the type profile
   | NetlistError_FailedRelation -- ?? Add type data of potential inputs recognised
   -- |Could not find any expressions that matched the type profile
   | NetlistError_FailedExpression -- ?? Add type data of potential inputs recognised
   -- |Enumeration ranges are not permitted in type definitions
   | NetlistError_EnumRangeInTypeDef
   -- |Physical ranges are not permitted in type definitions
   | NetlistError_PhysRangeInTypeDef
   -- |Downto ranges are not permitted in enum ranges ?? Or are they
   | NetlistError_DownToInEnumRange
   -- |Resolution function specified in constant declaration
   | NetlistError_ResFuncNotAllowedInSubtypeIndic
   -- |Type mark in constant declaration is unrecognised
   | NetlistError_UnrecognisedNameInSubtypeIndic
   -- |Enum value is not within the type range ?? Add type name, other useful details
   | NetlistError_EnumValueOutOfRange Enumerate
   -- |Integer value in not within the type range
   | NetlistError_IntValueOutOfRange Integer
   -- |Real value is not within the type range
   | NetlistError_FloatValueOutOfRange Double
   -- |Physical value is not within the type range
   | NetlistError_PhysValueOutOfRange Integer
   -- |Multiple possible values of the type found for a calculation
   | NetlistError_CannotFindValueWithContext
   -- |Constant name is already in use within the unit
   | NetlistError_ConstNameAlreadyDefined String
   -- |Duplicate constant names defined in the identifier list
   | NetlistError_DuplicateConstName String
   -- |New constraint for subtype of integer subtype is not within original
   | NetlistError_InvalidSubtypeIntConstraint IntegerRange IntegerRange
   -- |New constraint for subtype of floating subtype is not within original
   | NetlistError_InvalidSubtypeFloatConstraint FloatRange FloatRange
   -- |New constraint for subtype of physical subtype is not within original
   | NetlistError_InvalidSubtypePhysConstraint IntegerRange IntegerRange
   -- |No valid enumerate ranges found from constraint for specified subtype
   | NetlistError_NoValidEnumRangeFound
   -- |Subtype name has already been defined in the unit
   | NetlistError_SubtypeNameAlreadyDefined String
   -- |Expected type mark in subtype indication but got a function name
   | NetlistError_ExpectedTypeMarkButGotFunctionName
   -- |No expression found with a boolean result
   | NetlistError_NoConditionFound
   -- |No expression found with a time result
   | NetlistError_NoTimeResultExpressionFound
   -- |No expression found with a severity_level result
   | NetlistError_NoSeverityResultExpressionFound
   -- |Expected a signal name in the sensitivity list
   | NetlistError_ExpectedSignalNameInSensitivityList String
   -- |Expected a readable signal in the sensitivity list
   | NetlistError_ExpectedReadableSignalInSensitivityList
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
   show NetlistError_FailedSimpleExpression = -- ?? Add type data of potential inputs recognised
      "no simple expressions found that meet required type profile"
   show (NetlistError_PhysicalUnitNotFound unitName) =
      "the physical unit name "
      ++ unitName
      ++ " has not been defined"
   show (NetlistError_UnrecognisedEnumChar enumChar) =
      "undefined enumerate character '"
      ++ [enumChar]
      ++ "'"
   show (NetlistError_UnrecognisedName name) =
      "unrecognised primary name "
      ++ name
      ++ " in expression"
   show (NetlistError_DeferredConst name) =
      "the deferred constant \""
      ++ name
      ++ "\" cannot exist in a locally static expression"
   show NetlistError_ExpectedIntOrFloatLeftBoundRange =
      "expected an integer or floating point value in the left bound of a range"
   show NetlistError_ExpectedIntOrFloatRightBoundRange =
      "expected an integer or floating point value in the right bound of a range"
   show NetlistError_RangeTypeNoMatch =
      "the types of the left and right bounds do not match, in the range definition"
   show NetlistError_CannotInferValueFromContextInRangeTypeLeftBound =
      "cannot choose value from context; multiple possible values found for left bound of range"
   show NetlistError_CannotInferValueFromContextInRangeTypeRightBound =
      "cannot choose value from context; multiple possible values found for right bound of range"
   show NetlistError_FloatingRangeInPhysicalDefinition =
      "cannot have a floating point range in a physical type definition"
   show (NetlistError_BaseUnitNameIsTypeName unit) =
      "the base unit name \""
      ++ unit
      ++ "\" cannot have the same name as the type name"
   show (NetlistError_InvalidPhysLitName unit) =
      "the unit name \""
      ++ unit
      ++ "\" is already defined in this unit"
   show (NetlistError_SecondaryUnitNameIsTypeName unit) =
      "the secondary unit name \""
      ++ unit
      ++ "\" cannot have the same name as the type name"
   show (NetlistError_DuplicateSecondaryUnitInPhysType unit) =
      "the secondary unit declaration name \""
      ++ unit
      ++ "\" is already defined within this type"
   show (NetlistError_UnrecognisedPhysicalUnitName unit) =
      "the unit name \""
      ++ unit
      ++ "\" does not exist within this type definition"
   show NetlistError_FailedTerm = -- ?? Add type data of potential inputs recognised
      "no terms found that meet required type profile"
   show (NetlistError_IntegerTypeOutOfBounds leftVal rightVal) =
      let checkVal val boundName = if val > toInteger (maxBound :: Int64) || val < toInteger (minBound :: Int64)
                                    then boundName ++ " bound: " ++ show val
                                    else ""
      in "the values in the integer type range are out of bounds ("
         ++ (concat $ intersperse "," $ filter (not . null) [checkVal leftVal "left",checkVal rightVal "right"])
         ++ ")"
   -- |Values in floating type definition range are out of acceptable range
   show (NetlistError_FloatingTypeOutOfBounds leftVal rightVal) =
      "the values in the floating type range are out of bounds ("
      ++ if isInfinite leftVal
            then "left,"
            else ""
      ++ if isInfinite rightVal
            then "right"
            else ""
      ++ ")"
   show NetlistError_FailedFactor = -- ?? Add type data of potential inputs recognised
      "no factors found that meet required type profile"
   show NetlistError_FailedRelation = -- ?? Add type data of potential inputs recognised
      "no relations found that meet required type profile"
   show NetlistError_FailedExpression = -- ?? Add type data of potential inputs recognised
      "no expressions found that meet required type profile"
   show NetlistError_EnumRangeInTypeDef =
      "enumerated ranges cannot occur in a type definition"
   show NetlistError_PhysRangeInTypeDef =
      "physical ranges cannot occur in a type definition"
   show NetlistError_DownToInEnumRange =
      "downto range cannot occur in an enumerated range"

---- |Print list of wrapped names
--printNames :: [WrappedSimpleName] -> String
--printNames = concat . (intersperse ", ") . (map show)
