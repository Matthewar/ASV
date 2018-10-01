# Netlister Error
Errors that occur in the netlister.
The errors are contained in the netlist [error types](../../src/Parser/Netlist/Types/Error.hs) file.
They are held in the type `NetlistError`.
All these errors are wrapped with the position where the error occured in the file.
Note that all these errors have an associated position printed after their error message.

## Type name has already been defined
When attempting to declare a new type but the name already exists within the unit.

### Constructor
`NetlistError_DuplicateTypeName`

### Data Contained
- `String`: Type name

### Error Message
"duplicate type name within unit: \<name\>"

## Enumeration literal replicates the type name
The enumeration literal (identifier form) has the same name as its own type name.

### Constructor
`NetlistError_EnumLitIsTypeName`

### Data Contained
- `String`: Enum/type name

### Error Message
"enum literal is the same as its type declaration name \<name\>"

## Enum name has already been defined
When attempting to declare a new enumeration value but the name already exists (for a different declaration) within the unit.

### Constructor
`NetlistError_InvalidEnumName`

### Data Contained
- `String`: Enum name

### Error Message
"name is already declared in this unit \<name\>"

## Enumeration literal occurs twice in a type declaration
When an enumeration literal appears twice in the same type declaration.
Overloading of enumeration literals is allowed provided they are in different type declarations.

### Constructor
`NetlistError_DuplicateEnums`

### Data Contained
- `Enumerate`: Enumerate literal

### Error Message
"enum name is already declared in this type \<literal\>"

## Failed to find a simple expression
Could not find any simple expressions that matched the type profile.

### Constructor
`NetlistError_FailedSimpleExpression`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"no simple expressions found that meet the required type profile"

## Physical unit not found
The unit used in a physical literal cannot be found.

### Constructor
`NetlistError_PhysicalUnitNotFound`

### Data Contained
- `String`: Physical unit name

### Error Message
"the physical unit name \<unit name\> has not been defined"

## Enumerate character literal cannot be found
Enumerate character cannot be found.

### Constructor
`NetlistError_UnrecognisedEnumChar`

### Data Contained
- `Char`: Enumerate character

### Error Message
"undefined enumerate character '\<char\>'"

## Unrecognised primary identifier
When a primary (see 1076-1987) is a name (identifier), it should represent some declaration.
Error occurs if such a primary occurs and it has no linked declaration.

### Constructor
`NetlistError_UnrecognisedName`

### Data Contained
- `String`: Identifier primary

### Error Message
"unrecognised primary name \<name\> in expression"

## Deferred Constant used in expression
Deferred constants are allowed to exist in package headers.
These are where the constant value is declared in the package body.
In a locally static expression, all values must be evaluated immediately and therefore deferred constants are not allowed.

### Constructor
`NetlistError_DeferredConst`

### Data Contained
- `String`: Constant name

### Error Message
"the deferred constant "\<name\>" cannot exist in a locally static expression"

## Expected correct value type in left bound of a range
Expected an integer or floating point value in the left bound of a range definition.

### Constructor
`NetlistError_ExpectedIntOrFloatLeftBoundRange`

### Data Contained
N/A

### Error Message
"expected an integer of floating point value in the left bound of a range"

## Expected correct value type in right bound of a range
Expected an integer or floating point value in the right bound of a range definition.

### Constructor
`NetlistError_ExpectedIntOrFloatRightBoundRange`

### Data Contained
N/A

### Error Message
"expected an integer or floating point value in the right bound of a range"

## Unmatched types in a range
The types of the left and right bounds of a range do not match.

### Constructor
`NetlistError_RangeTypeNoMatch`

### Data Contained
N/A

### Error Message
"the types of the left and right bounds do not match, in the range definition"

## Too many possible values found for left bound of a range
Cannot determine the type of the left bound of a range definition from context.
If more than one value of type integer/float is found in a range definition left bound.

### Constructor
`NetlistError_CannotInferValueFromContextInRangeTypeLeftBound`

### Data Contained
N/A

### Error Message
"cannot choose value from context; multiple possible values found for left bound of range"

## Too many possible values found for right bound of a range
Cannot determine the type of the right bound of a range definition from context.
If more than one value of type integer/float is found in a range definition right bound.

### Constructor
`NetlistError_CannotInferValueFromContextInRangeTypeRightBound`

### Data Contained
N/A

### Error Message
"cannot choose value from context; multiple possible values found for right bound of range"

## Physical type definition must have an integer range
When a floating point range appears in a physical type definition.
Physical type definitions have an integer range.

### Constructor
`NetlistError_FloatingRangeInPhysicalDefinition`

### Data Contained
N/A

### Error Message
"cannot have a floating point range in a physical type definition"

## Base unit used as type name
When the base unit has the same name as its own declared type name.

### Constructor
`NetlistError_BaseUnitNameIsTypeName`

### Data Contained
- `String`: Base unit/type name

### Error Message
"the base unit name "\<unit name\>" cannot have the same name as the type name"

## Physical literal unit already declared
When a physical unit name already exists elsewhere in a declaration in the design entity.

### Constructor
`NetlistError_InvalidPhysLitName`

### Data Contained
- `String`: Unit name

### Error Message
"the unit name "\<unit name\>" is already defined in this unit"

## Physical literal secondary unit same as type name
When the secondary unit in a physical type definition has the same name as its own declared type name.

### Constructor
`NetlistError_SecondaryUnitNameIsTypeName`

### Data Contained
- `String`: Secondary unit name

### Error Message
"the secondary unit name "\<unit name\>" cannot have the same name as the type name"

## Physical literal secondary unit already declared in the type
When the secondary unit in a physical type definition has already been defined in the type.

### Constructor
`NetlistError_DuplicateSecondaryUnitInPhysType`

### Data Contained
- `String`: Secondary unit name

### Error Message
"the secondary unit declaration name "\<unit name\>" cannot have the same name as the type name"

## Unrecognsied physical unit name
The unit name used in a physical type definition has not been defined.

### Constructor
`NetlistError_UnrecognisedPhysicalUnitName`

### Data Contained
- `String`: Unit name

### Error Message
"the unit name "\<unit name\>" does not exist within this type definition"

## Failed to find a term
Could not find any terms that matched the type profile.

### Constructor
`NetlistError_FailedTerm`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"no terms found that meet required type profile"

## Integer type is outside of Haskell bounds
The values in an integer range definition must be within acceptable bounds.
These bounds are of the Haskell `Int64` type.

### Constructor
`NetlistError_IntegerTypeOutOfBounds`

### Data Contained
- `Integer`: Left bound value
- `Integer`: Right bound value

### Error Message
"the values in the integer type range are out of bounds (\<invalid bounds\>)"

## Floating type is outside of Haskell bounds
The values in a floating point range definition must be within acceptable bounds.
These bounds are of the Haskell `Double` type.

### Constructor
`NetlistError_FloatingTypeOutOfBounds`

### Data Contained
- `Double`: Left bound value
- `Double`: Right bound value

### Error Message
"the values in the floating type range are out of bounds (\<invalid bounds\>)"

## Failed to find a factor
Could not find any factors that matched the type profile.

### Constructor
`NetlistError_FailedFactor`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"no factors found that meet required type profile"

## Failed to find a relation
Could not find any relations that matched the type profile.

### Constructor
`NetlistError_FailedRelation`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"no relations found that meet required type profile"

## Failed to find an expression
Could not find any expressions that matched the type profile.

### Constructor
`NetlistError_FailedExpression`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"no expressions found that meet required type profile"

## Enumeration type ranges in a type definition
Ranges with enumerated values are not permitted in type definitions, must be integer or real.

### Constructor
`NetlistError_EnumRangeInTypeDef`

### Data Contained
N/A

### Error Message
"enumerated ranges cannot occur in a type definition"

## Physical type ranges in a type definition
Ranges with physical values are not permitted in type definitions, must be integer or real.

### Constructor
`NetlistError_PhysRangeInTypeDef`

### Data Contained
N/A

### Error Message
"physical ranges cannot occur in an enumerated range"

## Enumerated range with a downwards direction
As enumerated values are always defined with a positive range, downto ranges are not permitted.

### Constructor
`NetlistError_DownToInEnumRange`

### Data Contained
N/A

### Error Message
"downto range cannot occur in an enumerated range"

## Resolution function in constant subtype indication
When defining which type a constant has (subtype indication), a resolution function cannot be used (only relevant to signals).

### Constructor
`NetlistError_ResFuncNotAllowedInSubtypeIndic`

### Data Contained
N/A

### Error Message
"resolution functions cannot occur in the subtype indication of a constant declaration"

## Unknown type name in a constant subtype indication
When defining which type a constant has (subtype indication), the type mark must name a defined type or subtype.

### Constructor
`NetlistError_UnrecognisedNameInSubtypeIndic`

### Data Contained
N/A

### Error Message
"unrecognised type mark in constant declaration"

## Enumerated value out of defined range
When an enumerated value is not within the declared enumerated range.

### Constructor
`NetlistError_EnumValueOutOfRange`

### Data Contained
- `(NetlistName,String)`: (Type package name,type name)
- `Enumerate`: Enumeration literal

### Error Message
"enum value (\<enumerate full name\>) is not within the type range"

## Integer value out of defined range
When an integer value is not within the declared integer range.

### Constructor
`NetlistError_IntValueOutOfRange`

### Data Contained
- `Integer`: Integer value

### Error Message
"integer value (\<integer\>) is not within the type range"

## Floating value out of defined range
When a floating point value is not within the declared floating point range.

### Constructor
`NetlistError_FloatValueOutOfRange`

### Data Contained
- `Double`: Real value

### Error Message
"real value (\<float\>) is not within the type range"

## Physical value out of defined range
When a physical value is not within the declared physical type range.

### Constructor
`NetlistError_PhysValueOutOfRange`

### Data Contained
- `Integer`: Physical value (in base units)
- TODO: Add base unit

### Error Message
"physical value (\<physical value\>) is not within the type range"

## Multiple valid interpretations of an expression
Multiple possible values of the expected type found for a calculation.
Cannot choose a single one of these to use.

### Constructor
`NetlistError_CannotFindValueWithContext`

### Data Contained
N/A

### Error Message
"multiple possible values of expected type found for expression"

## Constant name already defined
When declaring a constant, if its name is already in use within the unit.

### Constructor
`NetlistError_ConstNameAlreadyDefined`

### Data Contained
- `String`: Constant name

### Error Message
"constant name (\<name\>) is already defined in the unit"

## Constant name already defined in constant declaration
When declaring multiple constants in a declaration list, if two names in the list are the same.

### Constructor
`NetlistError_DuplicateConstName`

### Data Contained
- `String`: Constant name

### Error Message
"duplicate constant name (\<name\>) in the identifier list"

## Invalid integer constraint: not within previous constraint
When declaring a new integer subtype (explicitly or implicitly in a subtype indication), if a bound of the declared range is not within the range of the type that this is a subtype of (named in type mark).

### Constructor
`NetlistError_InvalidSubtypeIntConstraint`

### Data Contained
- `IntegerRange`: Original integer type range
- `IntegerRange`: New integer type range

### Error Message
"New constraint in integer subtype is out of range (\<invalid constraint data\>)"

## Invalid floating point constraint: not within previous constraint
When declaring a new floating subtype (explicitly or implicitly in a subtype indication), if a bound of the declared range is not within the range of the type that this is a subtype of (named in type mark).

### Constructor
`NetlistError_InvalidSubtypeFloatConstraint`

### Data Contained
- `FloatRange`: Original floating type range
- `FloatRange`: New floating type range

### Error Message
"New constraint in floating subtype is out of range (\<invalid constraint data\>)"

## Invalid physical constraint: not within previous constraint
When declaring a new physical subtype (explicitly or implicitly in a subtype indication), if a bound of the declared range is not within the range of the type that this is a subtype of (named in type mark).

### Constructor
`NetlistError_InvalidSubtypePhysConstraint`

### Data Contained
- `IntegerRange`: Original physical type range (in base units)
- `IntegerRange`: New physical type range (in base units)

### Error Message
"New constraint in physical subtype is out of range (\<invalid constraint data\>)"

## No valid enumeration ranges found
Using the enumerated values provided in a range, no valid constraint corresponds for the specified subtype.

### Constructor
`NetlistError_NoValidEnumRangeFound`

### Data Contained
N/A

### Error Message
N/A

## Subtype name already defined
When the name in a subtype declaration has already been defined in the design entity.

### Constructor
`NetlistError_SubtypeNameAlreadyDefined`

### Data Contained
- `String`: Subtype name

### Error Message
"The subtype name \<name\> has already been defined in the unit"

## Expected a type mark in a subtype indication
In a subtype indication, after a resolution function, a type mark is expected.
When the identifier provided is a function name instead of a type name.

### Constructor
`NetlistError_ExpectedTypeMarkButGotFunctionName`

### Data Contained
N/A

### Error Message
"Expected a type mark in the subtype indication, but found a function name"

## Failed to find a condition
Could not find any conditions: expressions that have a VHDL `STD.STANDARD.BOOLEAN` result.

### Constructor
`NetlistError_NoConditionFound`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"No expression found with a boolean result"

## Failed to find a time expression
Could not find any expressions with a VHDL `STD.STANDARD.TIME` result.

### Constructor
`NetlistError_NoTimeResultExpressionFound`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"No expression found with a time (STD.STANDARD.TIME) result"

## Failed to find a SEVERITY_LEVEL expression
Could not find any expressions with a VHDL `STD.STANDARD.SEVERITY_LEVEL` result.

### Constructor
`NetlistError_NoSeverityResultExpressionFound`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"No expression found with a severity level (STD.STANDARD.SEVERITY_LEVEL) result"

## Sensitivity list must have signal names
All the names in the sensitivity list must represent signals.

### Constructor
`NetlistError_ExpectedSignalNameInSensitivityList`

### Data Contained
- `String`: Name

### Error Message
"Expected a signal name in the sensitivty list, but got name "\<name\>""

## Sensitivity list must have readable signals
All the signals in the sensitivity list must be readable (not mode out).

### Constructor
`NetlistError_ExpectedReadableSignalInSensitivityList`

### Data Contained
N/A

### Error Message
"Expected a readable signal in the sensitivity list"

## Architecture must have existing linked entity
No entity found for an associated architecture body.

### Constructor
`NetlistError_UnableToFindEntityForArch`

### Data Contained
- `NetlistName`: Entity package and name
- `String`: Architecture simple name

### Error Message
"unable to find entity \<entity full name\> for architecture \<architecture name\>"

## Failed to find an expression with the expected type
Could not find any expressions with the provided expected type.

### Constructor
`NetlistError_CannotFindCalculationOfType`

### Data Contained
- `(NetlistName,String)`: (Type package name,type name)
- TODO: Add type data of potential expressions found.

### Error Message
"Cannot find an expression of the type \<full type name\>"

## Signal assignment in a passive process
When a signal assignment occur in a passive process.

### Constructor
`NetlistError_SignalAssignmentInPassiveProcess`

### Data Contained
N/A

### Error Message
"Cannot have a signal assignment within a passive process"

## Failed to find a type conversion
Could not find any type conversions that matched the type profile.

### Constructor
`NetlistError_FailedTypeConversion`

### Data Contained
N/A
- TODO: Add type data of potential expressions found, expected type.

### Error Message
"Coud not find any types with valid conversions to target type in type conversion"

## Discrete subtype attribute used on non-discrete subtype
A subtype attribute intended for discrete subtypes has been applied to a non-discrete subtype.

### Constructor
`NetlistError_NonDiscreteTypeDiscreteAttr`

### Data Contained
- `String`: Attribute name

### Error Message
"The discrete subtype attribute \<attribute name\> cannot be applied to a non-discrete subtype"

## Subtype attribute argument is out of range
When a subtype attribute is a function type and its argument is out of acceptable range.

### Constructor
`NetlistError_SubtypeAttributeArgOutOfRange`

### Data Contained
N/A

### Error Message
N/A

## Signal name in a static expression
Signals are not static, so signal names cannot appear in a (locally or globally) static expression.

### Constructor
`NetlistError_SignalNameInStaticExpression`

### Data Contained
- `String`: Signal name

### Error Message
"Signal name \<name\> is not allowed in a static expression"

## Negative time in a signal attribute
When a negative time appears in the argument of a function type signal attribute.
Must be positive or zero time.

### Constructor
`NetlistError_NegativeTimeInSignalAttribute`

### Data Contained
N/A

### Error Message
"Negative time in signal (function type) attribute argument"

## Cannot read an output type port
When attempting to read a port of type output.

### Constructor
`NetlistError_CannotReadOutputPort`

### Data Contained
- `String`: Port name

### Error Message
"Attempt to read output port \<name\> is not allowed"

## Cannot write to a input type port
When attempting to write to a port of type input.

### Constructor
`NetlistError_CannotAssignToInputPort`

### Data Contained
- `String`: Port name

### Error Message
"Attempt to assign to input port \<name\> is not allowed"

## Cannot assign to a signal package
When attempting to assign to a signal from a package.

### Constructor
`NetlistError_PackageSignalInSignalAssign`

### Data Contained
N/A

### Error Message
"Cannot assign to a signal within a package"
