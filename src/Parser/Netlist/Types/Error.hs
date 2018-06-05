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
import Data.Maybe
         ( isJust
         , fromJust
         )
import Numeric (showFFloat)

import Lexer.Types.PositionWrapper (PosnWrapper(..))
import Lexer.Types.Error (getLineAndColErrStr)
import Parser.Netlist.Types.Representation
         ( Enumerate
         , IntegerRange(..)
         , FloatRange(..)
         , RangeDirection(..)
         , NetlistName
         )
import Sim.Output.Names (showEnum)

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
   -- |Enum value is not within the type range
   | NetlistError_EnumValueOutOfRange (NetlistName,String) Enumerate -- ?? Add type name, other useful details to all out of range ones
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
   -- |No entity found for an associated architecture body
   | NetlistError_UnableToFindEntityForArch NetlistName String
   -- |Cannot find an expression of the expected type
   | NetlistError_CannotFindCalculationOfType (NetlistName,String)
   -- |Cannot have a signal assignment in a passive process
   | NetlistError_SignalAssignmentInPassiveProcess
   -- |Could not find any type conversions that matched the type profile
   | NetlistError_FailedTypeConversion -- ?? Add type data of potential inputs recognised
   -- |Discrete subtype attribute applied to non-discrete subtype
   | NetlistError_NonDiscreteTypeDiscreteAttr String
   -- |Subtype attribute argument is out of range
   | NetlistError_SubtypeAttributeArgOutOfRange -- ?? Branch this into different errors depending on attribute name (succ,pred,leftof,rightof)
   -- |Signal name cannot occur in a static expression
   | NetlistError_SignalNameInStaticExpression String
   -- |Signal attribute function argument must be a positive time
   | NetlistError_NegativeTimeInSignalAttribute
   -- |Attempt to read an output port (not possible)
   | NetlistError_CannotReadOutputPort String
   -- |Attempt to write to an input port (not possible)
   | NetlistError_CannotAssignToInputPort String
   -- |Cannot assign to a signal in a package
   | NetlistError_PackageSignalInSignalAssign
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
   show NetlistError_ResFuncNotAllowedInSubtypeIndic =
      "resolution functions cannot occur in the subtype indication of a constant declaration"
   show NetlistError_UnrecognisedNameInSubtypeIndic =
      "unrecognised type mark in constant delcaration"
   show (NetlistError_EnumValueOutOfRange (typePackage,typeName) enum) =
      "enum value ("
      ++ show typePackage ++ "." ++ showEnum typeName enum
      ++ ") is not within the type range"
   show (NetlistError_IntValueOutOfRange int) =
      "integer value ("
      ++ show int
      ++ ") is not within the type range"
   -- |Real value is not within the type range
   show (NetlistError_FloatValueOutOfRange flt) =
      "real value ("
      ++ (showFFloat Nothing flt) ""
      ++ ") is not within the type range"
   show (NetlistError_PhysValueOutOfRange int) =
      "physical value ("
      ++ show int
      ++ " <BASE UNIT HERE>) is not within the type range"
   show NetlistError_CannotFindValueWithContext =
      "multiple possible values of expected type found for expression"
   show (NetlistError_ConstNameAlreadyDefined str) =
      "constant name ("
      ++ str
      ++ ") is already defined in the unit"
   show (NetlistError_DuplicateConstName str) =
      "duplicate constant name ("
      ++ str
      ++ ") in the identifier list"
   show (NetlistError_InvalidSubtypeIntConstraint (IntegerRange oldLeft oldRight direction) (IntegerRange newLeft newRight _)) =
      let checkVal val = case direction of
                           To -> val >= oldLeft && val <= oldRight
                           Downto -> val <= oldLeft && val >= oldRight
      in "New constraint in integer subtype is out of range ("
         ++ ( concat
            $ intersperse ", "
            $ map fromJust
            $ filter isJust
            [ if checkVal newLeft
               then Just $ "left bound " ++ show newLeft
               else Nothing
            , if checkVal newRight
               then Just $ "right bound " ++ show newRight
               else Nothing
            ]
            )
         ++ ")"
   show (NetlistError_InvalidSubtypeFloatConstraint (FloatRange oldLeft oldRight direction) (FloatRange newLeft newRight _)) =
      let checkVal val = case direction of
                           To -> val >= oldLeft && val <= oldRight
                           Downto -> val <= oldLeft && val >= oldRight
      in "New constraint in floating subtype is out of range ("
         ++ ( concat
            $ intersperse ", "
            $ map fromJust
            $ filter isJust
            [ if checkVal newLeft
               then Just $ "left bound " ++ (showFFloat Nothing newLeft) ""
               else Nothing
            , if checkVal newRight
               then Just $ "right bound " ++ (showFFloat Nothing newRight) ""
               else Nothing
            ]
            )
         ++ ")"
   show (NetlistError_InvalidSubtypePhysConstraint (IntegerRange oldLeft oldRight direction) (IntegerRange newLeft newRight _)) = -- ?? Need base unit
      let checkVal val = case direction of
                           To -> val >= oldLeft && val <= oldRight
                           Downto -> val <= oldLeft && val >= oldRight
      in "New constraint in physical subtype is out of range ("
         ++ ( concat
            $ intersperse ", "
            $ map fromJust
            $ filter isJust
            [ if checkVal newLeft
               then Just $ "left bound " ++ show newLeft
               else Nothing
            , if checkVal newRight
               then Just $ "right bound " ++ show newRight
               else Nothing
            ]
            )
         ++ ")"
   ---- |No valid enumerate ranges found from constraint for specified subtype
   --show NetlistError_NoValidEnumRangeFound = -- ?? Need more data here
   show (NetlistError_SubtypeNameAlreadyDefined name) =
      "The subtype name "
      ++ name
      ++ " has already been defined in the unit"
   show NetlistError_ExpectedTypeMarkButGotFunctionName =
      "Expected a type mark in the subtype indication, but found a function name"
   show NetlistError_NoConditionFound =
      "No expression found with a boolean result"
   show NetlistError_NoTimeResultExpressionFound =
      "No expression found with a time (STD.STANDARD.TIME) result"
   show NetlistError_NoSeverityResultExpressionFound =
      "No expression found with a severity level (STD.STANDARD.SEVERITY_LEVEL) result"
   show (NetlistError_ExpectedSignalNameInSensitivityList name) =
      "Expected a signal name in the sensitivity list, but got name \""
      ++ name
      ++ "\""
   show NetlistError_ExpectedReadableSignalInSensitivityList =
      "Expected a readable signal in the sensitivty list"
   show (NetlistError_UnableToFindEntityForArch entityName archName) =
      "unable to find entity "
      ++ show entityName
      ++ " for architecture "
      ++ archName
   show (NetlistError_CannotFindCalculationOfType (typePackage,typeName)) =
      "Cannot find an expression of the type "
      ++ show typePackage ++ "." ++ typeName
   show NetlistError_SignalAssignmentInPassiveProcess =
      "Cannot have a signal assignment within a passive process"
   show NetlistError_FailedTypeConversion = -- ?? Add type data of potential inputs recognised
      "Could not find any types with valid conversions to target type in type conversion"
   show (NetlistError_NonDiscreteTypeDiscreteAttr attr) =
      "The discrete subtype attribute "
      ++ attr
      ++ " cannot be applied to a non-discrete subtype"
   ---- |Subtype attribute argument is out of range
   --show NetlistError_SubtypeAttributeArgOutOfRange = -- ?? Branch this into different errors depending on attribute name (succ,pred,leftof,rightof)
   show (NetlistError_SignalNameInStaticExpression name) =
      "Signal name "
      ++ name
      ++ " is not allowed in a static expression"
   show NetlistError_NegativeTimeInSignalAttribute =
      "Negative time in signal (function type) attribute argument"
   show (NetlistError_CannotReadOutputPort name) =
      "Attempt to read output port "
      ++ name
      ++ " is not allowed"
   show (NetlistError_CannotAssignToInputPort name) =
      "Attempt to assign to input port "
      ++ name
      ++ " is not allowed"
   show NetlistError_PackageSignalInSignalAssign =
      "Cannot assign to a signal within a package"

---- |Print list of wrapped names
--printNames :: [WrappedSimpleName] -> String
--printNames = concat . (intersperse ", ") . (map show)
