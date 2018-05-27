module Lexer.Types.Error
( 
   ParserError(..)
   , WrappedParserError
   , printParserError
   , getFloatBound
   , getLineAndColErrStr
)
where

import Data.Char as Char
import Data.Int (Int64)
import Data.Function ((&))

import Lexer.Types.Token (Token)
import Lexer.Types.PositionWrapper
import Lexer.Alex.Types (AlexPosn(..))

-- |Errors that the parser and lexer can output
-- All errors contain the position where the error occurred.
data ParserError
   -- |Used when an unknown error has occurred in the lexer
   = GenericLexError
   -- |Based literal lexer error, invalid base value
   -- Contains base value, entire literal
   | LexErr_BasedLiteral_InvalidBaseValue Int String
   -- |Based literal lexer error, invalid base chararacter
   -- Contains base character, entire literal
   | LexErr_BasedLiteral_InvalidBaseChar Char String
   -- |Based literal lexer error, invalid based value
   -- This could be incorrect underscore formatting, or invalid characters found, or both
   -- Contains entire literal
   | LexErr_BasedLiteral_InvalidValue String
-- ??   | LexErr_BasedLiteral_Generic String
   -- |Decimal literal lexer error, incorrect formatting of decimal
   -- This could be empty section e.g. ".3" instead of "0.3"
   -- Also includes invalid underscore formatting
   -- Contains entire literal
-- ?? Should include incorrect characters and wrong exponent characters?
   | LexErr_DecimalLiteral_InvalidFormat String
   -- |Universal integer type lexer error, out of bounds value
   -- This occurs when a number is larger than the Haskell Int64 type
   -- Contains entire literal (based or decimal)
   | LexErr_UniversalInt_OutOfBounds String
   -- |Universal real type lexer error, out of bounds value
   -- This occurs when a number is larger than the Haskell Double type
   -- Contains entire literal (based or decimal)
   | LexErr_UniversalReal_OutOfBounds String
   -- |Bit string literal lexer error, invalid string
   -- This could be incorrect underscore formatting, invalid characters found, or both
   -- Contains entire literal
   | LexErr_BitStrLiteral_InvalidStr String
   -- |Bit string literal lexer error, invalid base character
   -- Contains base character, entire literal
   | LexErr_BitStrLiteral_InvalidBase Char String
   -- |Bit string literal lexer error, empty string
   -- Contains entire literal
   | LexErr_BitStrLiteral_EmptyStr String
   -- |Used when an unknown error has occurred in the parser
   | GenericParseError
   -- |Expected library name (identifier) but got something else
   | ParseErr_ExpectedLibraryName Token
   -- |Expected continuation or completion of library clause
   -- ',' or ';'
   | ParseErr_ExpectedLibraryClauseContiue Token
   -- |Expected library name (identifier) in first element of use clause
   | ParseErr_ExpectedLibraryNameInUseClause Token
   -- |Expected full stop (period) in second or fourth element of use clause
   | ParseErr_ExpectedPeriodInUseClause Token
   -- |Expected package name (identifier) in fifth element of use clause
   | ParseErr_ExpectedPackageNameInUseClause Token
   -- |Invalid operator (string literal) in sixth element of use clause
   | ParseErr_ExpectedOperatorInUseClause String
   -- |Invalid suffix (identifier, string literal, keyword all) in sixth element of use clause
   | ParseErr_ExpectedSuffixInUseClause Token
   -- |Expected first token of library unit (certain keywords)
   | ParseErr_ExpectedFirstKeywordInLibraryUnit Token
   -- |Expected second token to be remainder of package body (body keyword)
   | ParseErr_ExpectedPackageBodyKeywordInLibraryUnit Token
   -- |Expected package declarative item marker (certain keywords) or keyword end to mark end of declarative region
   | ParseErr_ExpectedPackageDeclItemOrEnd Token
   -- |Expected keyword is to mark start of package declarative region
   | ParseErr_ExpectedKeywordIsInPackage Token
   -- |Package identifiers (if both provided) must match
   | ParseErr_PackageNamesNoMatch (PosnWrapper String) (PosnWrapper String)
   -- |Expected semicolon to end package
   | ParseErr_ExpectedSemicolonInPackage Token
   -- |Expected semicolon or identifier at the end of a package declaration
   | ParseErr_ExpectedPackageEndOfDec Token
   -- |Expected type name (identifier) in type declaration
   | ParseErr_ExpectedTypeName Token
   -- |Expected semicolon to mark end of type definition
   | ParseErr_ExpectedSemicolonInTypeDef Token
   -- |Expected token to mark start of type definition (certain keywords)
   | ParseErr_ExpectedTypeDefinition Token
   -- |Expected enumeration literal (identifier or character literal)
   | ParseErr_ExpectedEnumLiteral Token
   -- |Expected enumeration continuation (comma to continue, right parenthesis to end)
   | ParseErr_ExpectedEnumCont Token
   -- |Expected range direction (keywords to/downto)
   | ParseErr_ExpectedDirection Token
   -- |Expected base unit name (identifier) in a physical type definition
   | ParseErr_ExpectedBaseUnitIdentifierInTypeDef Token
   -- |Expected a semicolon in base or secondary unit declaration in a physical type definition
   | ParseErr_ExpectedSemicolonInPhysTypeDef Token
   -- |Expected a equals operator in the secondary unit declaration of a physical type definition
   | ParseErr_ExpectedEqualInSecondaryUnitDecl Token
   -- |Expected a unit name (identifier) in the secondary unit declaration of a physical type definition
   | ParseErr_ExpectedPhysicalUnitName Token
   -- |Expected the unit keyword in end of secondary units in a physical type definition
   | ParseErr_ExpectedEndUnitsInPhysType Token
   -- |Expected the next secondary unit declaration or the key keyword to mark end of secondary units in the physical type definition
   | ParseErr_ExpectedSecondaryDeclarationOrEndInPhysType Token
   -- |Expected end of primary expression or aggregate (right parenthesis)
   | ParseErr_ExpectedRightParenInPrimary Token
   -- |Expected colon in constant declaration to indicate start of subtybe indication
   | ParseErr_ExpectedColonInConstDecl Token
   -- |Expected type mark (identifier) in constant declaration
   | ParseErr_ExpectedTypeMarkInSubtypeIndic Token
   -- |Expected a semicolon to end a constant declaration
   | ParseErr_ExpectedConstEnd Token
   -- |Expected the indicator of the start of the constant value (:= operator) or end of declaration for deferred constant (semicolon operator)
   | ParseErr_ExpectedConstValueOrEnd Token
   -- |Expected constant name (identifier) in constant declaration
   | ParseErr_ExpectedConstName Token
   -- |Range constraint not permitted for array subtype indication (should be discrete constraint)
   | ParseErr_RangeConstraintForArrayType Token
   -- |Expected subtype name (identifier) in subtype declaration
   | ParseErr_ExpectedSubtypeName Token
   -- |Expected keyword is to mark start of subtype indication in subtype declaration
   | ParseErr_ExpectedKeywordIsInSubtypeDecl Token
   -- |Expected semicolon to mark end of subtype declaration
   | ParseErr_ExpectedSemicolonInSubtypeDecl Token
   -- |Expected colon in signal declaration to indicate start of subtype indication
   | ParseErr_ExpectedColonInSigDecl Token
   -- |Expected a semicolon to end a signal declaration
   | ParseErr_ExpectedSigDeclEnd Token
   -- |Expected the indicator of the start of the signal value (:= operator) or end of declaration for default signal (semicolon operator)
   | ParseErr_ExpectedSigValueOrEnd Token
   -- |Expected entity name (identifier)
   | ParseErr_ExpectedEntityName Token
   -- |Expected is keyword to signify start of entity internals
   | ParseErr_ExpectedKeywordIsInEntity Token
   -- |Entity identifiers (if both provided) must match
   | ParseErr_EntityNamesNoMatch (PosnWrapper String) (PosnWrapper String)
   -- |Expected semicolon to signify end of an entity
   | ParseErr_ExpectedSemicolonInEntity Token
   -- |Expected semicolon or entity name at end of entity declaration
   | ParseErr_ExpectedEntityEndOfDec Token
   -- |Expected begin keyword to mark start of entity statements, or end keyword to mark end of entity inner region
   | ParseErr_ExpectedKeywordBeginOrEndInEntity Token
   -- |Expected left parenthesis in beginning of entity generic clause
   | ParseErr_ExpectedLeftParenInEntityGenericHeader Token
   -- |Expected semicolon to end entity generic clause
   | ParseErr_ExpectedSemicolonInEntityGenericHeaderEnd Token
   -- |Expected colon in interface declaration to mark start of subtype indication
   | ParseErr_ExpectedColonInInterfaceDecl Token
   -- |Expected interface continuation (semicolon) or end (right parenthesis)
   | ParseErr_ExpectedInterfaceContOrEnd Token
   -- |Expected left parenthesis in beginning of entity port clause
   | ParseErr_ExpectedLeftParenInEntityPortHeader Token
   -- |Expected semicolon to end entity port clause
   | ParseErr_ExpectedSemicolonInEntityPortHeaderEnd Token
   -- |Expected entity declarative item marker (certain keywords) or keywords begin or end to mark end of declarative region
   | ParseErr_ExpectedEntityDeclItemOrEnd Token
   -- |Expected colon in entity statement (following label)
   | ParseErr_ExpectedColonInEntityStatement Token
   -- |Expected entity statement item marker (certain keywords) after label of entity statement
   | ParseErr_ExpectedEntityStatementItem Token
   -- |Expected entity statement item marker (certain keywords or identifier) or keyword end to mark end of statement region
   | ParseErr_ExpectedEntityStatementItemOrEnd Token
   -- |Expected semicolon to end a context statement
   | ParseErr_ExpectedSemicolonInContextStatement Token
   -- |Expected semicolon to end wait statement
   | ParseErr_ExpectedSemicolonInWaitStatement Token
   -- |Expected semicolon to end assertion statement
   | ParseErr_ExpectedSemicolonInAssert Token
   -- |Expected report clause (report keyword), severity clause (severity keyword), or semicolon to end assertion statement
   | ParseErr_ExpectedReportOrSeverityOrEndInAssert Token
   -- |Expected signal name (identifier) in sensitivity list
   | ParseErr_ExpectedSignalNameInSensitivityList Token
   -- |Expected right parenthesis to terminate the sensitivity list of a process
   | ParseErr_ExpectedRightParenToEndProcessSensitivityList Token
   -- |Expected the process keyword in the end of a process statement
   | ParseErr_ExpectedKeywordProcessInEndProcess Token
   -- |Expected semicolon to end a process statement
   | ParseErr_ExpectedSemicolonInEndProcess Token
   -- |Expected no label in end of a process statement (because no label at the start)
   | ParseErr_ExpectedNoLabelInEndProcess Token
   -- |Process labels (if both provided) must match
   | ParseErr_ProcessLabelsNoMatch (PosnWrapper String) (PosnWrapper String)
   -- |Expected process declarative item marker (certain keywords) or keywords begin to mark end of declarative region
   | ParseErr_ExpectedProcessDeclItemOrEnd Token
   -- |The wait sequential statement can only occur in a sensitivity list under certain circumstances
   | ParseErr_WaitSeqStatementNotAllowedWithSensitivityList Token
   -- |Expected architecture simple name (identifier) in architecture declaration
   | ParseErr_ExpectedArchitectureSimpleName Token
   -- |Expected of keyword in architecture declaration
   | ParseErr_ExpectedKeywordOfInArch Token
   -- |Expected enitty name (identifier) in architecture declaration
   | ParseErr_ExpectedEntityNameInArch Token
   -- |Expected is keyword in architecture declaration
   | ParseErr_ExpectedKeywordIsInArch Token
   -- |Architecture simple names (if both provided) must match
   | ParseErr_ArchitectureNamesNoMatch (PosnWrapper String) (PosnWrapper String)
   -- |Expected semicolon to mark the end of the architecture declaration
   | ParseErr_ExpectedSemicolonInArchitecture Token
   -- |Expected either architecture simple name (identifier) or semicolon to mark the end of the architecture declaration
   | ParseErr_ExpectedArchitectureEndOfDec Token
   -- |Expected architecture declarative item (certain keywords) or the begin keyword to mark the end of the declarative region
   | ParseErr_ExpectedArchDeclItemOrEnd Token
   -- |Expected architecture statement item (certain keywords) following a statement label
   | ParseErr_ExpectedArchStatementItem Token
   -- |Expected colon in architecture statement following label
   | ParseErr_ExpectedColonInArchStatement Token
   -- |Expected architecture statement item (certain keywords) or the end keyword to mark the end of the statement region
   | ParseErr_ExpectedArchStatementItemOrEnd Token
   deriving (Eq)

instance (Show ParserError) where
   show (GenericLexError) =
      "Some lexer error occurred "
   show (LexErr_BasedLiteral_InvalidBaseValue base str) =
      "Lexer found invalid base value "
      ++ (show base)
      ++ " in based literal "
      ++ str
   show (LexErr_BasedLiteral_InvalidBaseChar base str) =
      "Lexer found invalid base character "
      ++ [base]
      ++ " in based literal "
      ++ str
   show (LexErr_BasedLiteral_InvalidValue str) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing based literal \""
      ++ str
      ++ "\""
--   show (LexErr_BasedLiteral_Generic str) =
--      "Some lexer error occurred lexing bit string "
--      ++ str
   show (LexErr_DecimalLiteral_InvalidFormat str) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing decimal literal \""
      ++ str
      ++ "\""
   show (LexErr_UniversalInt_OutOfBounds str) =
      "Lexer found out of bounds integer value "
      ++ str
      ++ " note that integer range is "
      ++ (show $ (minBound :: Int64))
      ++ " to "
      ++ (show $ (maxBound :: Int64))
   show (LexErr_UniversalReal_OutOfBounds str) =
      let maxVal = show $ getFloatBound (0.0 :: Double)
      in "Lexer found out of bounds real value "
         ++ str
         ++ " note that integer range is "
         ++ maxVal
         ++ " to -"
         ++ maxVal
   show (LexErr_BitStrLiteral_InvalidStr str) = -- ?? Replace with analysis and then print from this
      "Some lexer error occurred lexing bit string literal \""
      ++ str
      ++ "\""
   show (LexErr_BitStrLiteral_InvalidBase base str) =
      "Lexer found invalid base character "
      ++ [base]
      ++ " in bit string literal "
      ++ str
   show (LexErr_BitStrLiteral_EmptyStr str) =
      "Lexer found empty bit string "
      ++ str
   show (GenericParseError) =
      "Some parser error occurred"
   show (ParseErr_ExpectedLibraryName token) =
      "Expected library name (identifier), but got "
      ++ show token
   show (ParseErr_ExpectedLibraryClauseContiue token) =
      "Expected ',' or ';' to continue or end library clause, but got "
      ++ show token
   show (ParseErr_ExpectedLibraryNameInUseClause token) =
      "Expected library name (identifier) in first selected name, in use clause, but got "
      ++ show token
   show (ParseErr_ExpectedPeriodInUseClause token) =
      "Expected '.' to continue selected name (after library or entity name) in use clause, but got "
      ++ show token
   show (ParseErr_ExpectedPackageNameInUseClause token) =
      "Expected package name (identifier) in selected name (after library name) in use clause, but got "
      ++ show token
   show (ParseErr_ExpectedOperatorInUseClause strLit) =
      "Expected the string literal in selected name to name a valid operator, but got \""
      ++ strLit
      ++ "\""
   show (ParseErr_ExpectedSuffixInUseClause token) =
      "Expected a selected name suffix (identifier, string literal containing an operator, or the keyword all) in the final element of use clause, but got "
      ++ show token
   show (ParseErr_ExpectedFirstKeywordInLibraryUnit token) =
      "Expected design unit declaration (entity, configuration, architecture or package), but got "
      ++ show token
   show (ParseErr_ExpectedPackageBodyKeywordInLibraryUnit token) =
      "Expected either package name (identifier) or body keyword to follow package keyword, but got "
      ++ show token
   show (ParseErr_ExpectedPackageDeclItemOrEnd token) =
      "Expected package declaration or end keyword to make end of declarative region, but got "
      ++ show token
   show (ParseErr_ExpectedKeywordIsInPackage token) =
      "Expected keyword is, marking the start of the package declarative region, but got "
      ++ show token
   show (ParseErr_PackageNamesNoMatch (PosnWrapper pos1 name1) (PosnWrapper pos2 name2)) =
      "Package names (identifiers) must match when both are provided. First identifier "
      ++ name1
      ++ getLineAndColErrStr pos1
      ++ "; second identifier "
      ++ name2
      ++ getLineAndColErrStr pos2
      ++ ". In package declaration"
   show (ParseErr_ExpectedSemicolonInPackage token) =
      "Expected semicolon to end package design entity, but got "
      ++ show token
   show (ParseErr_ExpectedPackageEndOfDec token) =
      "Expected either a semicolon or the repeated package name (identifier) at the end of a package design entity, but got "
      ++ show token
   show (ParseErr_ExpectedTypeName token) =
      "Expected a name (identifier) in the type declaration, but got "
      ++ show token
   show (ParseErr_ExpectedSemicolonInTypeDef token) =
      "Expected semicolon at the end of the type definition, but got "
      ++ show token
   show (ParseErr_ExpectedTypeDefinition token) =
      "Expected type definition token, but got "
      ++ show token
   show (ParseErr_ExpectedEnumLiteral token) =
      "Expected enumeration literal (identifier, or character literal), but got "
      ++ show token
   show (ParseErr_ExpectedEnumCont token) =
      "Expected ',' or ')' to continue or end enumeration literal definition, but got "
      ++ show token
   show (ParseErr_ExpectedSubtypeName token) =
      "Expected a name (identifier) in the subtype declaration, but got "
      ++ show token
   show (ParseErr_ExpectedKeywordIsInSubtypeDecl token) =
      "Expected keyword is, marking the start of the subtype indication in a subtype declaration, but got "
      ++ show token
   show (ParseErr_ExpectedSemicolonInSubtypeDecl token) =
      "Expected semicolon at the end of the subtype declaration, but got "
      ++ show token
   show (ParseErr_ExpectedColonInSigDecl token) =
      "Expected a colon to mark the start of the subtype indication in a signal declaration, but got "
      ++ show token
   show (ParseErr_ExpectedSigDeclEnd token) =
      "Expected a semicolon to end a signal declaration, but got "
      ++ show token
   show (ParseErr_ExpectedSigValueOrEnd token) =
      "Expected a signal value (marked by variable assignment symbol) or end of signal declaration (marked by semicolon), but got "
      ++ show token
   show (ParseErr_ExpectedEntityName token) =
      "Expected an entity name (identifier), but got "
      ++ show token
   show (ParseErr_ExpectedKeywordIsInEntity token) =
      "Expected keyword is to mark the start of entity inner region, but got "
      ++ show token
   show (ParseErr_EntityNamesNoMatch (PosnWrapper pos1 name1) (PosnWrapper pos2 name2)) =
      "Entity names (identifiers) must match when both are provided. First identifier "
      ++ name1
      ++ getLineAndColErrStr pos1
      ++ "; second identifier "
      ++ name2
      ++ getLineAndColErrStr pos2
      ++ ". In entity declaration"
   show (ParseErr_ExpectedSemicolonInEntity token) =
      "Expected semicolon at the end of an entity declaration, but got "
      ++ show token
   show (ParseErr_ExpectedEntityEndOfDec token) =
      "Expected semicolon or entity name after end keyword at end of entity declaration, but got "
      ++ show token
   show (ParseErr_ExpectedKeywordBeginOrEndInEntity token) =
      "Expected either begin keyword to mark start of entity statement region, or end keyword to mark end of entity inner region, but got "
      ++ show token
   show (ParseErr_ExpectedLeftParenInEntityGenericHeader token) =
      "Expected left parenthesis to mark the start of an entity generic clause, but got "
      ++ show token
   show (ParseErr_ExpectedSemicolonInEntityGenericHeaderEnd token) =
      "Expected semicolon to end an entity generic clause, but got "
      ++ show token
   show (ParseErr_ExpectedColonInInterfaceDecl token) =
      "Expected colon in interface declaration, to mark the start of the subtype indication, but got "
      ++ show token
   show (ParseErr_ExpectedInterfaceContOrEnd token) =
      "Expected semicolon to indicate interface declarations continue, or right parenthesis to mark end of the interface declarations, but got "
      ++ show token
   show (ParseErr_ExpectedLeftParenInEntityPortHeader token) =
      "Expected left parenthesis to mark the start of an entity port clause, but got "
      ++ show token
   show (ParseErr_ExpectedSemicolonInEntityPortHeaderEnd token) =
      "Expected semicolon to end an entity port clause, but got "
      ++ show token
   show (ParseErr_ExpectedEntityDeclItemOrEnd token) =
      "Expected entity declaration marker (certain keywords) or keywords begin or end to mark the end of the entity declarative region, but got "
      ++ show token
   show (ParseErr_ExpectedColonInEntityStatement token) =
      "Expected colon after a label for an entity statement, but got "
      ++ show token
   show (ParseErr_ExpectedEntityStatementItem token) =
      "Expected entity statement item (certain keywords) after a label, but got "
      ++ show token
   show (ParseErr_ExpectedEntityStatementItemOrEnd token) =
      "Expected entity statement item (certain keywords), entity statement label (identifier), or end keyword to mark end of entity statement region, but got "
      ++ show token
   show (ParseErr_ExpectedSemicolonInContextStatement token) =
      "Expected a semicolon to end a context statement (library or use clause), but got "
      ++ show token
   show (ParseErr_ExpectedSemicolonInWaitStatement token) =
      "Expected a semicolon at the end of a wait statement, but got "
      ++ show token
   show (ParseErr_ExpectedSemicolonInAssert token) =
      "Expected a semicolon at the end of an assertion statement, but got "
      ++ show token
   show (ParseErr_ExpectedReportOrSeverityOrEndInAssert token) =
      "Expected report clause (report keyword), severity clause (severity keyword), or semicolon to end an assertion statement, but got "
      ++ show token
   show (ParseErr_ExpectedSignalNameInSensitivityList token) =
      "Expected a signal name (identifier) in a sensitivity list, but got "
      ++ show token
   show (ParseErr_ExpectedRightParenToEndProcessSensitivityList token) =
      "Expected right parenthesis to mark the end of a sensitivity list in a process, but got "
      ++ show token
   show (ParseErr_ExpectedKeywordProcessInEndProcess token) =
      "Expected the keyword process at the end of a process statement, but got "
      ++ show token
   show (ParseErr_ExpectedSemicolonInEndProcess token) =
      "Expected a semicolon at the end of a process statement, but got "
      ++ show token
   show (ParseErr_ExpectedNoLabelInEndProcess token) =
      "Expected no label at the end of a process statement because there was no label at the start of the process, but got "
      ++ show token
   show (ParseErr_ProcessLabelsNoMatch (PosnWrapper pos1 label1) (PosnWrapper pos2 label2)) =
      "Process labels (identifiers) must match when both are provided. First identifier "
      ++ label1
      ++ getLineAndColErrStr pos1
      ++ "; second identifier "
      ++ label2
      ++ getLineAndColErrStr pos2
      ++ ". In process declaration"
   show (ParseErr_ExpectedProcessDeclItemOrEnd token) =
      "Expected a process declaration item (certain keywords) or the begin keyword to mark the end of the declarative region, but got "
      ++ show token
   show (ParseErr_WaitSeqStatementNotAllowedWithSensitivityList token) =
      "Wait sequential statement cannot occur with a sensitivity list, but got "
      ++ show token
   show (ParseErr_ExpectedArchitectureSimpleName token) =
      "Expected architecture simple name (identifier) in architecture declaration, but got "
      ++ show token
   show (ParseErr_ExpectedKeywordOfInArch token) =
      "Expected keyword of after architecture simple name in an architecture declaration, but got "
      ++ show token
   show (ParseErr_ExpectedEntityNameInArch token) =
      "Expected entity name (identifier) in architecture declaration after keyword of, but got "
      ++ show token
   show (ParseErr_ExpectedKeywordIsInArch token) =
      "Expected keyword is after entity name in architecture declaration, but got "
      ++ show token
   show (ParseErr_ArchitectureNamesNoMatch (PosnWrapper pos1 name1) (PosnWrapper pos2 name2)) =
      "Architecture names (identifiers) must match when both are provided. First identifier "
      ++ name1
      ++ getLineAndColErrStr pos1
      ++ "; second identifier "
      ++ name2
      ++ getLineAndColErrStr pos2
      ++ ". In entity declaration"
   show (ParseErr_ExpectedSemicolonInArchitecture token) =
      "Expected semicolon at the end of an architecture body declaration, but got "
      ++ show token
   show (ParseErr_ExpectedArchitectureEndOfDec token) =
      "Expected a semicolon of the repeated architecture name (identifier) to mark the end of an architecture body declaration, but got "
      ++ show token
   show (ParseErr_ExpectedArchDeclItemOrEnd token) =
      "Expected architecture declarative item, or the keyword begin to mark the end of the architecture declaration region, but got "
      ++ show token
   show (ParseErr_ExpectedArchStatementItem token) =
      "Expected an architecture statement item after a label, but got "
      ++ show token
   show (ParseErr_ExpectedColonInArchStatement token) =
      "Expected a colon after a label in an architecture statement, but got "
      ++ show token
   show (ParseErr_ExpectedArchStatementItemOrEnd token) =
      "Expected architecture statement (certain keywords or identifier), or the keyword end to mark the end of the architecture statement region, but got "
      ++ show token

-- | Find largest (and by extension smallest) possible value of double
getFloatBound :: RealFloat a => a -> a
getFloatBound val =
   let radix = floatRadix val
       maxExp = snd $ floatRange val
       numBitsSig = floatDigits val
       mantissa = (radix ^ numBitsSig) - 1
       exp = maxExp - numBitsSig
   in encodeFloat mantissa exp

type WrappedParserError = PosnWrapper ParserError

-- |Print parser error
-- Extracts position of error and error type to produce message.
printParserError err =
   let position = getPos err
       contents = unPos err
   in show contents ++ getLineAndColErrStr position

-- | Get the string containing the line and column string
getLineAndColErrStr :: AlexPosn -> String
getLineAndColErrStr (AlexPn _ line column) =
   " at line "
   ++ (show line)
   ++ ", column "
   ++ (show column)
