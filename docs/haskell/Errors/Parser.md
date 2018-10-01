# Parser Error
Errors that occur in the parser or the lexer.
The errors are contained in the lexer [error types](../../src/Lexer/Types/Error.hs) file.
They are held in the type `ParserError`.
All these errors are wrapped with the position where the error occured in the file.
Note that all these errors have an associated position printed after their error message.

## Generic Lexer Error
When an unknown error has occurred in the lexer.

### Constructor
`GenericLexError`

### Data Contained
N/A

### Error Message
"Some lexer error occured"

## Invalid Base Value in Based Literal
When an invalid base value is used for a based literal.

### Constructor
`LexErr_BasedLiteral_InvalidBaseValue`

### Data Contained
- `Int`: Invalid base value
- `String`: Literal string

### Error Message
"Lexer found invalid base value \<base\> in based literal \<literal\>"

## Invalid Character in Based Literal
When an invalid character apppears in a based literal.

### Constructor
`LexErr_BasedLiteral_InvalidBaseChar`

### Data Contained
- `Char`: Invalid base string character
- `String`: Literal string

### Error Message
"Lexer found invalid base character \<char\> in based literal \<literal\>"

## Invalid Based Literal Value
When a based value has an invalid format.
This could be incorrect underscore formatting, or invalid characters found, or both.

### Constructor
`LexErr_BasedLiteral_InvalidValue`

### Data Contained
- `String`: Literal string

### Error Message
"Some lexer error occurred lexing based literal "\<literal\>""

## Invalid Format of Decimal Literal
Decimal literal lexer error, incorrect formatting of decimal.
This could be empty section e.g. ".3" instead of "0.3".
Also includes invalid underscore formatting.

### Constructor
`LexErr_DecimalLiteral_InvalidFormat`

### Data Contained
- `String`: Decimal literal string

### Error Message
"Some lexer error occurred lexing decimal literal "\<literal\>""

## Out of Bounds Universal Integer
When a universal integer value is out of bounds.
This occurs when a number is outside of the Haskell `Int64` type.

### Constructor
`LexErr_UniversalInt_OutOfBounds`

### Data Contained
- `String`: Literal string (based or decimal)

### Error Message
"Lexer found out of bounds integer value \<literal\> note that integer range is \<lower bound\> to \<upper bound\>"

## Out of Bounds Universal Real
When a universal real value is out of bounds.
This occurs when a number is outside of the Haskell `Double` type.

### Constructor
`LexErr_UniversalReal_OutOfBounds`

### Data Contained
- `String`: Literal string (based or decimal)

### Error Message
"Lexer found out of bounds real value \<literal\> note that floating range is \<upper bound\> to \<lower bound\>"

## Invalid bit string literal
Invalid bit string literal in the lexer.
This could be incorrect underscore formatting, invalid characters found, or both.

### Constructor
`LexErr_BitStrLiteral_InvalidStr`

### Data Contained
- `String`: Bit string literal

### Error Message
"Some lexer error occurred lexing bit string literal "\<literal\>""

## Invalid base character in bit string literal
When the base character of a bit string literal is not one of 'B', 'O', 'X', 'b', 'o', 'x'.

### Constructor
`LexErr_BitStrLiteral_InvalidBase`

### Data Contained
- `Char`: Base character
- `String`: Bit string literal

### Error Message
"Lexer found invalid base character \<base character\> in bit string literal \<literal\>"

## Empty bit string literal
Bit string literal cannot be empty.

### Constructor
`LexErr_BitStrLiteral_EmptyStr`

### Data Contained
- `String`: Bit string literal

### Error Message
"Lexer found empty bit string \<literal\>"

## Generic Parser Error
When an unknown error has occurred in the parser.

### Constructor
`GenericParseError`

### Data Contained
N/A

### Error Message
"Some parser error occurred"

## Expected library name in library clause
Expecting a library name (identifier) in a library clause.

### Constructor
`ParseErr_ExpectedLibraryName`

### Data Contained
- `Token`: Token found

### Error Message
"Expected library name (identifier), but got \<token\>"

## Expected continuation of library clause
Expected either a:
- comma (operator ',') to mark the continuation of the library clause
- semicolon (operator ';') to mark the end of the library clause

### Constructor
`ParseErr_ExpectedLibraryClauseContiue`

### Data Contained
- `Token`: Token found

### Error Message
"Expected ',' or ';' to continue or end library clause, but got \<token\>"

## Expected library name in use clause
Expected a library name (identifier) in the first element of a selected name in a use clause.

### Constructor
`ParseErr_ExpectedLibraryNameInUseClause`

### Data Contained
- `Token`: Token found

### Error Message
"Expected library name (identifier) in first selected name, in use clause, but got \<token\>"

## Expected period in use clause
Expected period (operator '.') in second or fourth element of a selected name in a use clause.

### Constructor
`ParseErr_ExpectedPeriodInUseClause`

### Data Contained
- `Token`: Token found

### Error Message
"Expected '.' to continue selected name (after library or entity name) in use clause, but got \<token\>"

## Expected package name in use clause
Expected package name (identifier) in third element of a selected name in a use clause.

### Constructor
`ParseErr_ExpectedPackageNameInUseClause`

### Data Contained
- `Token`: Token found

### Error Message
"Expected package name (identifier) in selected name (after library name) in use clause, but got \<token\>"

## Expected valid operator in use clause
When a string literal is the fifth element of a selected name in a use clause.
String literal should name an operator, and it must be an operator that already exists.

### Constructor
`ParseErr_ExpectedOperatorInUseClause`

### Data Contained
- `String`: Operator in string literal

### Error Message
"Expected the string literal in selected name to name a valid operator, but got "\<string\>""

## Expected suffix in use clause
Expected the fifth element of a selected name in a use clause to be a suffix.
Either a:
- Name of a declaration (identifier)
- Operator name (string literal)
- Select all of package (keyword all)

### Constructor
`ParseErr_ExpectedSuffixInUseClause`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a selected name suffix (identifier, string literal containing an operator, or the keyword all) in the final element of use clause, but got \<token\>"

## Expected library unit keyword
Expected a keyword denoting a library unit in a design unit.
- Keyword entity
- Keyword configuration
- Keyword architecture
- Keyword package

### Constructor
`ParseErr_ExpectedFirstKeywordInLibraryUnit`

### Data Contained
- `Token`: Token found

### Error Message
"Expected design unit declaration (entity, configuration, architecture or package), but got \<token\>"

## Expected package continuation
Expected token after a package keyword to be either:
- Name of package (identifier)
- Marker of package body (body keyword)

### Constructor
`ParseErr_ExpectedPackageBodyKeywordInLibraryUnit`

### Data Contained
- `Token`: Token found

### Error Message
"Expected either package name (identifier) or body keyword to follow package keyword, but got \<token\>"

## Expected package declarative item or end of package
Expected either:
- Package declarative item (certain keywords)
- Marker for end of declarative region (keyword end)

### Constructor
`ParseErr_ExpectedPackageDeclItemOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected package declaration or end keyword to make end of declarative region, but got \<token\>"

## Expected start of package declarative region
Expected keyword is to mark start of package declarative region.

### Constructor
`ParseErr_ExpectedKeywordIsInPackage`

### Data Contained
- `Token`: Token found

### Error Message
"Expected keyword is, marking the start of the package declarative region, but got \<token\>"

## Package identifiers not matching
Package names (identifiers), if both provided, must match.

### Constructor
`ParseErr_PackageNamesNoMatch`

### Data Contained
- `(PosnWrapper String)`: First identifier and position
- `(PosnWrapper String)`: Second identifier and position

### Error Message
"Package names (identifiers) must match when both are provided. First identifier \<first name at position\>; second identifier \<second name at position\>. In package declaration"

## Expected semicolon in package declaration
Expected semicolon (operator ';') to mark end of package declaration.

### Constructor
`ParseErr_ExpectedSemicolonInPackage`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon to end package design entity, but got \<token\>"

## Expected end of package declaration
Expected either a:
- End of package declaration (operator ';')
- Final package declaration name (identifier)

### Constructor
`ParseErr_ExpectedPackageEndOfDec`

### Data Contained
- `Token`: Token found

### Error Message
"Expected either a semicolon or the repeated package name (identifier) at the end of a package design entity, but got \<token\>"

## Expected type name in type declaration
Expected type name (identifier) in type declaration.

### Constructor
`ParseErr_ExpectedTypeName`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a name (identifier) in the type declaration, but got \<token\>"

## Expected end of type definition
Expected semicolon (operator ';') to mark end of type definition.

### Constructor
`ParseErr_ExpectedSemicolonInTypeDef`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon at the end of the type definition, but got \<token\>"

## Expected start of type definition
Expected certain keywords to mark start of type definition in a type declaration.
- TODO: List these keywords

### Constructor
`ParseErr_ExpectedTypeDefinition`

### Data Contained
- `Token`: Token found

### Error Message
"Expected type definition token, but got \<token\>"

## Expected enumeration literal in enumeration type definition
Expected enumeration literal:
- Identifier
- Character literal

### Constructor
`ParseErr_ExpectedEnumLiteral`

### Data Contained
- `Token`: Token found

### Error Message
"Expected enumeration literal (identifier, or character literal), but got \<token\>"

## Expected enumeration continuation in enumeration type definition
Expected either a:
- Marker to continue enumerates (operator ',')
- Marker to end enumerates (operator ')')

### Constructor
`ParseErr_ExpectedEnumCont`

### Data Contained
- `Token`: Token found

### Error Message
"Expected ',' or ')' to continue or end enumeration literal definition, but got \<token\>"

## Expected range direction in range declaration
Expected range direction in range declaration.
Either:
- Keyword to
- Keyword downto

### Constructor
`ParseErr_ExpectedDirection`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a range direction keyword (keyword to/keyword downto) in a range, but got \<token\>"

## Expected base unit name in a physical type definition
Expected base unit name (identifier) in a physical type definition.

### Constructor
`ParseErr_ExpectedBaseUnitIdentifierInTypeDef`

### Data Contained
- `Token`: Token found

### Error Message
"Expected base unit name (identifier) in the physical type definition, but got \<token\>"

## Expected a semicolon in unit declaration in a physical type definition
Expected a semicolon (operator ';') in the base or secondary unit declaration, in a physical type definition.

### Constructor
`ParseErr_ExpectedSemicolonInPhysTypeDef`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon in the base or secondary unit declaration in a physical type definition, but got \<token\>"

## Expected equals in secondary unit declaration in a physical type definition
Expected equals (operator '=') in a secondary unit declaration in a physical type definition.

### Constructor
`ParseErr_ExpectedEqualInSecondaryUnitDecl`

### Data Contained
- `Token`: Token found

### Error Message
"Expected an equals operator in the secondary unit declaration in a physical type definition, but got \<token\>"

## Expected a physical unit name in a secondary unit declaration in a physical type definition
Expected a unit name (identifier) in the secondary unit declaration of a physical type definition.

### Constructor
`ParseErr_ExpectedPhysicalUnitName`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a unit name (identifier) in the secondary unit declaration in a physical type definition, but got \<token\>"

## Expected unit keyword to end the secondary units region in a physical type definition
Expected keyword unit at the end of the secondary units region in a physical type definition.

### Constructor
`ParseErr_ExpectedEndUnitsInPhysType`

### Data Contained
- `Token`: Token found

### Error Message
"Expected the unit keyword at the end of the secondary units in a physical type definition, but got \<token\>"

## Expected secondary units continuation
Expected either:
- Next secondary unit declaration (integer type literal or unit name identifier)
- Marker of end of secondary units region (end keyword)
In physical type definition.

### Constructor
`ParseErr_ExpectedSecondaryDeclarationOrEndInPhysType`

### Data Contained
- `Token`: Token found

### Error Message
"Expected the next secondary unit declaration or the end keyword to mark end of secondary units in the physical type definition, but got \<token\>"

## Expected right parenthesis in primary
Expected a right parenthesis (operator ')') to end either an expression or aggregate in a primary.

### Constructor
`ParseErr_ExpectedRightParenInPrimary`

### Data Contained
- `Token`: Token found

### Error Message
"Expected end of primary expression or aggregate (right parenthesis), but got \<token\>"

## Expected colon in a constant declaration
Expected a colon (operator ':') to indicate the start of the subtype indication in a constant declaration.

### Constructor
`ParseErr_ExpectedColonInConstDecl`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a colon in the constant declaration to indicate start of subtybe indication, but got \<token\>"

## Expected a type name in a constant declaration
Expected a type mark (identifier) in a constant declaration.

### Constructor
`ParseErr_ExpectedTypeMarkInSubtypeIndic`

### Data Contained
- `Token`: Token found

### Error Message
"Expected type mark (identifier) in constant declaration, but got \<token\>"

## Expected a semicolon in a constant declaration
Expected a semicolon (operator ';') to end a constant declaration

### Constructor
`ParseErr_ExpectedConstEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon to end a constant declaration, but got \<token\>"

## Expected constant declaration value or end
In a constant declaration, expected either a:
- Indicator of the start of the constant's value (operator ':=')
- End of declartaion for deferred constant (operator ';')

### Constructor
`ParseErr_ExpectedConstValueOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected the indicator of the start of the constant value (:= operator) or end of declaration for deferred constant (semicolon operator), but got \<token\>"

## Expected a constant name in a constant declaration
Expected constant name (identifier) in constant declaration.

### Constructor
`ParseErr_ExpectedConstName`

### Data Contained
- `Token`: Token found

### Error Message
"Expected constant name (identifier) in constant declaration, but got \<token\>"

## Not permitted to have range constraint for array type constraint in subtype indication
Range constraint not permitted for array subtype indication (should be discrete constraint).

### Constructor
`ParseErr_RangeConstraintForArrayType`

### Data Contained
- `Token`: Token found

### Error Message
"Range constraint not permitted for array subtype indication (should be discrete constraint), but got \<token\>"

## Expected subtype name in a subtype declaration
Expected subtype name (identifier) in a subtype declaration.

### Constructor
`ParseErr_ExpectedSubtypeName`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a name (identifier) in the subtype declaration, but got \<token\>"

## Expected keyword is in subtype indication
Expected keyword is to mark the start of the subtype indication in a subtype declaration.

### Constructor
`ParseErr_ExpectedKeywordIsInSubtypeDecl`

### Data Contained
- `Token`: Token found

### Error Message
"Expected keyword is, marking the start of the subtype indication in a subtype declaration, but got \<token\>"

## Expected semicolon in a subtype declaration
Expected semicolon (operator ';') to mark the end of a subtype declaration

### Constructor
`ParseErr_ExpectedSemicolonInSubtypeDecl`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon at the end of the subtype declaration, but got \<token\>"

## Expected colon in signal declaration
Expected colon (keyword ':') in a signal declaration to indicate the start of a subtype indication.

### Constructor
`ParseErr_ExpectedColonInSigDecl`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a colon to mark the start of the subtype indication in a signal declaration, but got \<token\>"

## Expected a semicolon in a signal declaration
Expected a semicolon (operator ';') to end a signal declaration.

### Constructor
`ParseErr_ExpectedSigDeclEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon to end a signal declaration, but got \<token\>"

## Expected signal declaration continuation
In a signal declaration, expected either a:
- Indicator of the start of the signal default value (operator ':=')
- End of the signal declaration (operator ';')

### Constructor
`ParseErr_ExpectedSigValueOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a signal value (marked by variable assignment symbol) or end of signal declaration (marked by semicolon), but got \<token\>"

## Expected entity name in an entity declaration
Expected entity name (identifier) in an entity declaration.

### Constructor
`ParseErr_ExpectedEntityName`

### Data Contained
- `Token`: Token found

### Error Message
"Expected an entity name (identifier), but got \<token\>"

## Expected keyword is in entity declaration
Expected keyword is to signify the start of the entity internal sections.

### Constructor
`ParseErr_ExpectedKeywordIsInEntity`

### Data Contained
- `Token`: Token found

### Error Message
"Expected keyword is to mark the start of entity inner region, but got \<token\>"

## Entity names must match if both provided in an entity declaration
Entity names (identifiers), if both provided, must match.

### Constructor
`ParseErr_EntityNamesNoMatch`

### Data Contained
- `(PosnWrapper String)`: First identifier and position
- `(PosnWrapper String)`: Second identifier and position

### Error Message
"Entity names (identifiers) must match when both are provided. First identifier \<first name at position\>; second identifier \<second name at position\>. In entity declaration"

## Expected semicolon in entity declaration
Expected semicolon (operator ';') to signify the end of an entity declaration.

### Constructor
`ParseErr_ExpectedSemicolonInEntity`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon at the end of an entity declaration, but got \<token\>"

## Expected entity end token in entity declaration
Expected either a:
- Semicolon to mark end of entity declaration (operator ';')
- Entity name (identifier)

### Constructor
`ParseErr_ExpectedEntityEndOfDec`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon or entity name after end keyword at end of entity declaration, but got \<token\>"

## Expected entity statement region marker in entity declaration
Expected either a:
- Marker for the start of entity statements (keyword begin)
- Marker for the end of the entity inner region (keyword end)

### Constructor
`ParseErr_ExpectedKeywordBeginOrEndInEntity`

### Data Contained
- `Token`: Token found

### Error Message
"Expected either begin keyword to mark start of entity statement region, or end keyword to mark end of entity inner region, but got \<token\>"

## Expected left parenthesis in entity generic clause
Expected left parenthesis (operator '(') to mark the beginning of entity generic clause generics.

### Constructor
`ParseErr_ExpectedLeftParenInEntityGenericHeader`

### Data Contained
- `Token`: Token found

### Error Message
"Expected left parenthesis to mark the start of an entity generic clause, but got \<token\>"

## Expected a semicolon in entity generic clause
Expected semicolon (operator ';') at the end of a entity generic clause.

### Constructor
`ParseErr_ExpectedSemicolonInEntityGenericHeaderEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon to end an entity generic clause, but got \<token\>"

## Expected a colon in interface declaration
Expected a colon (operator ':') to mark the start of the subtype indication in an interface declaration.

### Constructor
`ParseErr_ExpectedColonInInterfaceDecl`

### Data Contained
- `Token`: Token found

### Error Message
"Expected colon in interface declaration, to mark the start of the subtype indication, but got \<token\>"

## Expected interface declarations continuation
In a interface declaration, expected either a:
- Semicolon to mark continuation of interface declarations (operator ';')
- Marker for end of interface declarations (operator ')')

### Constructor
`ParseErr_ExpectedInterfaceContOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon to indicate interface declarations continue, or right parenthesis to mark end of the interface declarations, but got \<token\>"

## Expected left parenthsis in an entity port clause
Expected left parenthesis (operator '(') at the beginning of an entity port clause.

### Constructor
`ParseErr_ExpectedLeftParenInEntityPortHeader`

### Data Contained
- `Token`: Token found

### Error Message
"Expected left parenthesis to mark the start of an entity port clause, but got \<token\>"

## Expected a semicolon in an entity port clause
Expected semicolon (operator ';') to end an entity port clause.

### Constructor
`ParseErr_ExpectedSemicolonInEntityPortHeaderEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon to end an entity port clause, but got \<token\>"

## Expected entity declaration region token
In the entity declarative region, expected either a:
- Entity declarative item (certain keywords)
- End of declarative region (keyword begin or keyword end)

### Constructor
`ParseErr_ExpectedEntityDeclItemOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected entity declaration marker (certain keywords) or keywords begin or end to mark the end of the entity declarative region, but got \<token\>"

## Expected a colon in an entity statement
Expected a colon (operator ':') in an entity statement (following a label).

### Constructor
`ParseErr_ExpectedColonInEntityStatement`

### Data Contained
- `Token`: Token found

### Error Message
"Expected colon after a label for an entity statement, but got \<token\>"

## Expected entity statement token
Expected entity statement item (marked by certain keywords) after label of entity statement.

### Constructor
`ParseErr_ExpectedEntityStatementItem`

### Data Contained
- `Token`: Token found

### Error Message
"Expected entity statement item (certain keywords) after a label, but got \<token\>"

## Expected an entity statement token
In an entity statement region, expected either a:
- Entity statement item (certain keywords or identifier)
- Marker for the end of the statement region (keyword end)

### Constructor
`ParseErr_ExpectedEntityStatementItemOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected entity statement item (certain keywords), entity statement label (identifier), or end keyword to mark end of entity statement region, but got \<token\>"

## Expected a semicolon in a context statement
Expected a semicolon (operator ';') at the end of a context statement.

### Constructor
`ParseErr_ExpectedSemicolonInContextStatement`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon to end a context statement (library or use clause), but got \<token\>"

## Expected a semicolon in a wait statement
Expected a semicolon (operator ';') at the end of a wait statement.

### Constructor
`ParseErr_ExpectedSemicolonInWaitStatement`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon at the end of a wait statement, but got \<token\>"

## Expected a semicolon in an assertion statement
Expected a semicolon (operator ';') at the end of an assertion statement.

### Constructor
`ParseErr_ExpectedSemicolonInAssert`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon at the end of an assertion statement, but got \<token\>"

## Expected an assertion statement clause token
In an assertion statement, expected either a:
- Report clause (keyword report)
- Severity clause (keyword severity)
- End of assertion statement (operator ';')

### Constructor
`ParseErr_ExpectedReportOrSeverityOrEndInAssert`

### Data Contained
- `Token`: Token found

### Error Message
"Expected report clause (report keyword), severity clause (severity keyword), or semicolon to end an assertion statement, but got \<token\>"

## Expected a signal name in a sensitivity list
Expected a signal name (identifier) in a sensitivity list.

### Constructor
`ParseErr_ExpectedSignalNameInSensitivityList`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a signal name (identifier) in a sensitivity list, but got \<token\>"

## Expected right parenthesis in a sensitivity list
Expected a right parenthesis (operator ')') at the end of a sensitivity list in a process.

### Constructor
`ParseErr_ExpectedRightParenToEndProcessSensitivityList`

### Data Contained
- `Token`: Token found

### Error Message
"Expected right parenthesis to mark the end of a sensitivity list in a process, but got \<token\>"

## Expected process keyword in a process statement
Expected the keyword process at the end of a process statement.

### Constructor
`ParseErr_ExpectedKeywordProcessInEndProcess`

### Data Contained
- `Token`: Token found

### Error Message
"Expected the keyword process at the end of a process statement, but got \<token\>"

## Expected a semicolon at the end of a process statement
Expected a semicolon (operator ';') at the end of a process statement.

### Constructor
`ParseErr_ExpectedSemicolonInEndProcess`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon at the end of a process statement, but got \<token\>"

## Label not permitted at the end of a process statement
Expected no label (identifier) at the end of a process statement if there is no label at the start of the process statement.

### Constructor
`ParseErr_ExpectedNoLabelInEndProcess`

### Data Contained
- `Token`: Token found

### Error Message
"Expected no label at the end of a process statement because there was no label at the start of the process, but got \<token\>"

## Process labels must match if both provided in a process statement
Process labels (identifiers), if both provided, must match.

### Constructor
`ParseErr_ProcessLabelsNoMatch`

### Data Contained
- `(PosnWrapper String)`: First identifier and position
- `(PosnWrapper String)`: Second identifier and position

### Error Message
"Process labels (identifiers) must match when both are provided. First identifier \<first label and position\>; second identifier \<second label and position\>. In process declaration"

## Expected a process declaration token
In the process declarative region, expected either a:
- Process declarative item (certain keywords)
- End of declarative region (keyword begin or keyword end)

### Constructor
`ParseErr_ExpectedProcessDeclItemOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a process declaration item (certain keywords) or the begin keyword to mark the end of the declarative region, but got \<token\>"

## Wait statement not permitted with sensivity list
The sequential wait statement can only occur in a process without a sensitivity list.

### Constructor
`ParseErr_WaitSeqStatementNotAllowedWithSensitivityList`

### Data Contained
- `Token`: Token found

### Error Message
"Wait sequential statement cannot occur with a sensitivity list, but got \<token\>"

## Expected an architecture simple name in an architecture declaration
Expected an architecture simple name (identifier) in an architecture declaration.

### Constructor
`ParseErr_ExpectedArchitectureSimpleName`

### Data Contained
- `Token`: Token found

### Error Message
"Expected architecture simple name (identifier) in architecture declaration, but got \<token\>"

## Expected of keyword in an architecture declaration
Expected keyword of in an architecture declaration.

### Constructor
`ParseErr_ExpectedKeywordOfInArch`

### Data Contained
- `Token`: Token found

### Error Message
"Expected keyword of after architecture simple name in an architecture declaration, but got \<token\>"

## Expected entity name in an architecture declaration
Expected an entity name (identifier) in an architecture declaration.

### Constructor
`ParseErr_ExpectedEntityNameInArch`

### Data Contained
- `Token`: Token found

### Error Message
"Expected entity name (identifier) in architecture declaration after keyword of, but got \<token\>"

## Expected is keyword in an architecture declaration
Expected keyword is to mark the start of the inner region of an architecture declaration.

### Constructor
`ParseErr_ExpectedKeywordIsInArch`

### Data Contained
- `Token`: Token found

### Error Message
"Expected keyword is after entity name in architecture declaration, but got \<token\>"

## Architecture simple names must match if both provided in an architecture declaration
Architecture simple names (identifiers), if both provided, must match.

### Constructor
`ParseErr_ArchitectureNamesNoMatch`

### Data Contained
- `(PosnWrapper String)`: First identifier and position
- `(PosnWrapper String)`: Second identifier and position

### Error Message
"Architecture names (identifiers) must match when both are provided. First identifier \<first name and position\>; second identifier \<second name and position\>. In entity declaration"

## Expected a semicolon in an architecture declaration
Expected a semicolon (operator ';') at the end of an architecture declaration.

### Constructor
`ParseErr_ExpectedSemicolonInArchitecture`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon at the end of an architecture body declaration, but got \<token\>"

## Expected an architecture end token
At the end of an architecture declaration, expected either:
- An architecture simple name (identifier)
- End of the architecture declaration (operator ';')

### Constructor
`ParseErr_ExpectedArchitectureEndOfDec`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon or the repeated architecture name (identifier) to mark the end of an architecture body declaration, but got \<token\>"

## Expected an architecture declarative token
In the declarative region of an architecture declaration, expected either a:
- Architecture declarative item (certain keywords)
- Marker of the end of the declarative region (keyword begin)

### Constructor
`ParseErr_ExpectedArchDeclItemOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected architecture declarative item, or the keyword begin to mark the end of the architecture declaration region, but got \<token\>"

## Expected an architecture statement
Expected an architecture statement item (marked by certain keywords) following a statement label.

### Constructor
`ParseErr_ExpectedArchStatementItem`

### Data Contained
- `Token`: Token found

### Error Message
"Expected an architecture statement item after a label, but got \<token\>"

## Expected a colon in an architecture statement
Expected a colon (operator ':') in an architecture statement, following a label.

### Constructor
`ParseErr_ExpectedColonInArchStatement`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a colon after a label in an architecture statement, but got \<token\>"

## Expected an architecture statement token
In an architecture statement region, expected either a:
- Architecture statement item (certain keywords)
- Marker of the end of the statement region (keyword end)

### Constructor
`ParseErr_ExpectedArchStatementItemOrEnd`

### Data Contained
- `Token`: Token found

### Error Message
"Expected architecture statement (certain keywords or identifier), or the keyword end to mark the end of the architecture statement region, but got \<token\>"

## Expected a signal assignment symbol in a signal assignment statement
Expected the signal assignment symbol (operator '<=') after the signal name in a signal assignment statement.

### Constructor
`ParseErr_ExpectedSignalAssignInSignalAssignment`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a signal assignment symbol (<=) after signal name in signal assignment statement, but got \<token\>"

## Expected a semicolon in a signal assignment
Expected a semicolon (operator ';') at the end of a signal assignment statement.

### Constructor
`ParseErr_ExpectedSemicolonInSignalAssignment`

### Data Contained
- `Token`: Token found

### Error Message
"Expected a semicolon at the end of a signal assignment statement, but got \<token\>"

## Expected a value expression in a waveform element
In a waveform element, expected either a:
- Null value (keyword null)
- Value expression (parse expression)

### Constructor
`ParseErr_ExpectedNullOrValueExpressionInWaveform`

### Data Contained
- `Token`: Token found

### Error Message
"Expected null keyword or value expression in waveform element, but got \<token\>"

## Expected subtype primary
After a subtype name, expected either a:
- Qualified expression (operator ''' then operator '(')
- Type conversion (operator '(')
- Attribute (operator ''')

### Constructor
`ParseErr_ExpectedQualExpOrTypeConvOrAttrAfterTypeNamePrimary`

### Data Contained
- `Token`: Token found

### Error Message
"Expected qualified expression (apostrophe followed by left parenthesis); type conversion (left parenthesis); or attribute (apostrophe) after subtype name, but got \<token\>"

## Expected right parenthesis in a qualified expression
Expected a right parenthesis (operator ')') at the end of a qualified expression.

### Constructor
`ParseErr_ExpectedRightParenInQualifiedExpression`

### Data Contained
- `Token`: Token found

### Error Message
"Expected right parenthesis in qualified expression, but got \<token\>"

## Expected a right parenthesis in a type conversion
Expected a right parenthesis (operataor ')') at the end of a type conversion.

### Constructor
`ParseErr_ExpectedRightParenInTypeConversion`

### Data Contained
- `Token`: Token found

### Error Message
"Expected right parenthesis in type conversion, but got \<token\>"

## Expected a subtype attribute name
Expected a subtype attribute (identifier) in an attribute name.

### Constructor
`ParseErr_ExpectedSubtypeAttribute`

### Data Contained
- `Token`: Token found

### Error Message
"Expected subtype attribute (identifier) in subtype attribute name, but got \<token\>"

## Expected left parenthesis in a subtype attribute
Expected a left parenthesis (operator '(') in a subtype attribute (function type).

### Constructor
`ParseErr_ExpectedLeftParenInSubtypeAttrFunc`

### Data Contained
- `Token`: Token found

### Error Message
"Expected left parenthesis in subtype (function type) attribute, but got \<token\>"

## Expected right parenthesis in a subtype attribute
Expected a right parenthesis (operator ')') in a subtype attribute (function type).

### Constructor
`ParseErr_ExpectedRightParenInSubtypeAttrFunc`

### Data Contained
- `Token`: Token found

### Error Message
"Expected right parenthesis in subtype (function type) attribute, but got \<token\>"

## Expected a signal attribute name
Expected a signal attribute (identifier) in an attribute name.

### Constructor
`ParseErr_ExpectedSignalAttribute`

### Data Contained
- `Token`: Token found

### Error Message
"Expected signal attribute (identifier) in signal attribute name, but got \<token\>"

## Expected then keyword in an if statement
Expected keyword then after the condition of an if statement.

### Constructor
`ParseErr_ExpectedKeywordThenInIfStatement`

### Data Contained
- `Token`: Token found

### Error Message
"Expected keyword then after an if statement condition, but got \<token\>"

## Expected if keyword at the end of an if statement
Expected keyword if at the end of an if statement.

### Constructor
`ParseErr_ExpectedKeywordIfInEndIfStatement`

### Data Contained
- `Token`: Token found

### Error Message
"Expected keyword if at the end of an if statement, but got \<token\>"

## Expected semicolon in an if statement
Expected a semicolon (operator ';') at the end of an if statement.

### Constructor
`ParseErr_ExpectedSemicolonInEndIfStatement`

### Data Contained
- `Token`: Token found

### Error Message
"Expected semicolon at the end of an if statement, but got \<token\>"

## Expected if statement token
In an if statement, expected either a:
- Else if statement (keyword elsif)
- Else statement (keyword else)
- End of statements (keyword end)

### Constructor
`ParseErr_ExpectedIfStatmentContinuation`

### Data Contained
- `Token`: Token found

### Error Message
"Expected continuation of if statement (elsif, else, or end keywords), but got \<token\>"
