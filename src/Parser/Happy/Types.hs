module Parser.Happy.Types where

import Data.Int (Int64)
import Data.ByteString.Char8 (ByteString)

import qualified Lexer.Types.Token as TokenTypes
import Lexer.Types.PositionWrapper

------------------------------------------
-- Design Units
------------------------------------------

-- |Design file
-- > design_file ::= design_unit { design_unit }
newtype DesignFile = DesignFile [WrappedDesignUnit]
                   deriving (Show)

-- |Wrapped design unit
-- Contains 'DesignUnit' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedDesignUnit = PosnWrapper DesignUnit

-- |Design unit
-- > design_unit ::= context_clause library_unit
data DesignUnit = DesignUnit ContextClause WrappedLibraryUnit
                deriving (Show)

-- |Wrapped library unit
-- Contains 'LibraryUnit' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedLibraryUnit = PosnWrapper LibraryUnit

-- |Library unit
-- @
--    library_unit ::=
--       primary_unit
--       | secondary_unit
-- @
data LibraryUnit =
   -- |Primary unit
   -- > library_unit ::= primary_unit
   Library_PrimaryUnit PrimaryUnit
   -- |Secondary unit
   -- > library_unit ::= secondary_unit
   | Library_SecondaryUnit SecondaryUnit
   deriving(Show)

-- |Primary unit
-- @
--    primary_unit ::=
--       entity_declaration
--       | configuration_declaration
--       | package_declaration
-- @
data PrimaryUnit =
   -- |Entity declaration
   -- > primary_unit ::= entity_declaration
   PrimaryUnit_EntityDeclaration EntityDeclaration
   -- |Configuration declaration
   -- > primary_unit ::= configuration_declaration
   | PrimaryUnit_ConfigurationDeclaration ConfigurationDeclaration
   -- |Package declaration
   -- > primary_unit ::= package_declaration
   | PrimaryUnit_PackageDeclaration PackageDeclaration
   deriving(Show)

-- |Secondary unit
-- @
--    secondary_unit ::=
--       architecture_body
--       | package_body
-- @
data SecondaryUnit =
   -- |Architecture body
   -- > secondary_unit ::= architecture_body
   Secondary_ArchitectureBody ArchitectureBody
   -- |Package body
   -- > secondary_unit ::= package_body
   | Secondary_PackageBody PackageBody
   deriving(Show)

-- |Wrapped library clause
-- Contains 'LibraryClause' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedLibraryClause = PosnWrapper LibraryClause

-- |Library clause
-- @
--    library_clause ::= __library__ logical_name_list ;
--    logical_name_list ::= logical_name { , logical_name }
--    logical_name ::= identifier
-- @
newtype LibraryClause = LibraryClause [WrappedSimpleName]
                      deriving (Show)

-- |Wrapped use clause
-- Contains 'UseClause' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedUseClause = PosnWrapper UseClause

-- |Use clause
-- @
--    use_clause ::=
--       __use__ selected_name { , selected_name } ;
-- @
newtype UseClause = UseClause [WrappedSelectedName]
                  deriving (Show)

-- |Context clause
-- > context_clause ::= { context_item }
type ContextClause = [WrappedContextItem]

-- |Wrapped context item
-- Contains 'ContextItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedContextItem = PosnWrapper ContextItem

-- |Context item
-- @
--    context_item ::=
--       library_clause
--       | use_clause
-- @
data ContextItem =
   -- |Library clause
   -- > context_item ::= library_clause
   Context_LibraryClause LibraryClause
   -- |Use clause
   -- > context_item ::= use_clause
   | Context_UseClause UseClause
   deriving(Show)

------------------------------------------
-- Design Entities and Configurations
------------------------------------------

-- |Wrapped entity declaration
-- Contains 'EntityDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedEntityDeclaration = PosnWrapper EntityDeclaration

-- |Entity Declaration
-- @
--    entity_declaration ::=
--       entity identifier is
--          entity_header
--          entity_declarative_part
--     [ begin
--          entity_statement_part ]
--       end [ /entity/_simple_name ] ;
-- @
-- /entity/_simple_name must repeat identifier
data EntityDeclaration = EntityDeclaration WrappedSimpleName EntityHeader EntityDeclarativePart (Maybe EntityStatementPart) (Maybe WrappedSimpleName)
                       deriving (Show)

-- |Entity Header
-- @
--    entity_header ::=
--       [ /formal/_generic_clause ]
--       [ /formal/_port_clause ]
-- @
data EntityHeader = EntityHeader (Maybe WrappedGenericClause) (Maybe WrappedPortClause)
                  deriving (Show)

-- |Wrapped generic clause
-- Contains 'GenericClause' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedGenericClause = PosnWrapper GenericClause

-- |Generic Clause
-- @
--    generic_clause ::=
--       __generic__ ( generic_list ) ;
--    generic_list ::= /generic/_interface_list
-- @
type GenericClause = InterfaceList

-- |Wrapped port clause
-- Contains 'PortClause' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPortClause = PosnWrapper PortClause

-- |Port Clause
-- @
--    port_clause ::=
--       __port__ ( port_list ) ;
--    port_list ::= /port/_interface_list
-- @
type PortClause = InterfaceList

-- |Entity declarative part
-- @
--    entity_declarative_part ::=
--       { entity_declarative_item }
-- @
type EntityDeclarativePart = [WrappedEntityDeclarativeItem]

-- |Wrapped entity declarative part
-- Contains 'EntityDeclartiveItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedEntityDeclarativeItem = PosnWrapper EntityDeclarativeItem

-- |Entity declarative item
-- @
--    entity_declarative_item ::=
--       subprogram_declaration
--       | subprogram_body
--       | type_declaration
--       | subtype_declaration
--       | constant_declaration
--       | signal_declaration
--       | file_declaration
--       | alias_declaration
--       | attribute_declaration
--       | attribute_specification
--       | disconnection_specification
--       | use_clause
-- @
data EntityDeclarativeItem =
   -- |Entity declaration: Subprogram declaration
   -- > entity_declarative_item ::= subprogram_declaration
   EntityDeclaration_SubprogramDeclaration         SubprogramDeclaration
   -- |Entity declaration: Subprogram body
   -- > entity_declarative_item ::= subprogram_body
   | EntityDeclaration_SubprogramBody              SubprogramBody
   -- |Entity declaration: Type declaration
   -- > entity_declarative_item ::= type_declaration
   | EntityDeclaration_TypeDeclaration             TypeDeclaration
   -- |Entity declaration: Subtype declaration
   -- > entity_declarative_item ::= subtype_declaration
   | EntityDeclaration_SubtypeDeclaration          SubtypeDeclaration
   -- |Entity declaration: Constant declaration
   -- > entity_declarative_item ::= constant_declaration
   | EntityDeclaration_ConstantDeclaration         ConstantDeclaration
   -- |Entity declaration: Signal declaration
   -- > entity_declarative_item ::= signal_declaration
   | EntityDeclaration_SignalDeclaration           SignalDeclaration
   -- |Entity declaration: File declaration
   -- > entity_declarative_item ::= file_declaration
   | EntityDeclaration_FileDeclaration             FileDeclaration
   -- |Entity declaration: Alias declaration
   -- > entity_declarative_item ::= alias_declaration
   | EntityDeclaration_AliasDeclaration            AliasDeclaration
   -- |Entity declaration: Attribute declaration
   -- > entity_declarative_item ::= attribute_declaration
   | EntityDeclaration_AttributeDeclaration        AttributeDeclaration
   -- |Entity declaration: Attribute specification
   -- > entity_declarative_item ::= attribute_specification
   | EntityDeclaration_AttributeSpecification      AttributeSpecification
   -- |Entity declaration: Disconnection specification
   -- > entity_declarative_item ::= disconnection_specification
   | EntityDeclaration_DisconnectionSpecification  DisconnectionSpecification
   -- |Entity declaration: Use clause
   -- > entity_declarative_item ::= use_clause
   | EntityDeclaration_UseClause                   UseClause
   deriving(Show)

-- |Entity statement part
-- @
--    entity_statement_part ::=
--       { entity_statement }
-- @
type EntityStatementPart = [WrappedEntityStatement]

-- |Wrapped entity statement
-- Contains 'EntityStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedEntityStatement = PosnWrapper EntityStatement

-- |Entity statement
-- @
--    entity_statement ::=
--       concurrent_assertion_statement
--       | /passive/_concurrent_procedure_call
--       | /passive/_process_statement
-- @
data EntityStatement =
   -- |Entity statement: concurrent assertion statement
   -- > entity_statement ::= concurrent_assertion_statement
   EntityStatement_ConcurrentAssertionStatement  ConcurrentAssertionStatement
   -- |Entity statement: concurrent procedure call
   -- > entity_statement ::= /passive/_concurrent_procedure_call
   | EntityStatement_ConcurrentProcedureCall       ConcurrentProcedureCall
   -- |Entity statement: process statement
   -- > entity_statement ::= /passive/_process_statement
   | EntityStatement_ProcessStatement              ProcessStatement
   deriving(Show)

-- |Wrapped architecture body
-- Contains 'ArchitectureBody' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedArchitectureBody = PosnWrapper ArchitectureBody

-- |Architecture body
-- @
--    architecture_body ::=
--       __architecture__ identifier __of__ /entity/_name __is__
--          architecture_declarative_part
--       __begin__
--          architecture_statement_part
--       __end__ [ /architecture/_simple_name ] ;
-- @
data ArchitectureBody = ArchitectureBody WrappedSimpleName WrappedName ArchitectureDeclarativePart ArchitectureStatementPart (Maybe WrappedSimpleName)
                      deriving (Show)

-- |Architecture declarative part
-- @
--    architecture_declarative_part ::=
--       { block_declarative_item }
-- @
type ArchitectureDeclarativePart = [WrappedBlockDeclarativeItem]

-- |Wrapped block declarative item
-- Contains 'BlockDeclarativeItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedBlockDeclarativeItem = PosnWrapper BlockDeclarativeItem

-- |Block declarative item
-- @
--    block_declarative_item ::=
--       subprogram_declaration
--       | subprogram_body
--       | type_declaration
--       | subtype_declaration
--       | constant_declaration
--       | signal_declaration
--       | file_declaration
--       | alias_declaration
--       | component_declaration
--       | attribute_declaration
--       | attribute_specification
--       | configuration_specification
--       | disconnection_specification
--       | use_clause
-- @
data BlockDeclarativeItem =
   -- |Subprogram declaration
   -- > block_declarative_item ::= subprogram_declaration
   BlockDeclarativeItem_SubprogramDeclaration        SubprogramDeclaration
   -- |Subprogram body
   -- > block_declarative_item ::= subprogram_body
   | BlockDeclarativeItem_SubprogramBody               SubprogramBody
   -- |Type declaration
   -- > block_declarative_item ::= type_declaration
   | BlockDeclarativeItem_TypeDeclaration              TypeDeclaration
   -- |Subtype declaration
   -- > block_declarative_item ::= subtype_declaration
   | BlockDeclarativeItem_SubtypeDeclaration           SubtypeDeclaration
   -- |Constant declaration
   -- > block_declarative_item ::= constant_declaration
   | BlockDeclarativeItem_ConstantDeclaration          ConstantDeclaration
   -- |Signal declaration
   -- > block_declarative_item ::= signal_declaration
   | BlockDeclarativeItem_SignalDeclaration            SignalDeclaration
   -- |File declaration
   -- > block_declarative_item ::= file_declaration
   | BlockDeclarativeItem_FileDeclaration              FileDeclaration
   -- |Alias declaration
   -- > block_declarative_item ::= alias_declaration
   | BlockDeclarativeItem_AliasDeclaration             AliasDeclaration
   -- |Component declaration
   -- > block_declarative_item ::= component_declaration
   | BlockDeclarativeItem_ComponentDeclaration         ComponentDeclaration
   -- |Attribute declaration
   -- > block_declarative_item ::= attribute_declaration
   | BlockDeclarativeItem_AttributeDeclaration         AttributeDeclaration
   -- |Attribute specification
   -- > block_declarative_item ::= attribute_specification
   | BlockDeclarativeItem_AttributeSpecification       AttributeSpecification
   -- |Configuration specification
   -- > block_declarative_item ::= configuration_specification
   | BlockDeclarativeItem_ConfigurationSpecification   ConfigurationSpecification
   -- |Disconnection specification
   -- > block_declarative_item ::= disconnection_specification
   | BlockDeclarativeItem_DisconnectionSpecification   DisconnectionSpecification
   -- |Use clause
   -- > block_declarative_item ::= use_clause
   | BlockDeclarativeItem_UseClause                    UseClause
   deriving(Show)

-- |Architecture statement part
-- @
--    architecture_statement_part ::=
--       { concurrent_statement }
-- @
type ArchitectureStatementPart = [WrappedConcurrentStatement]

-- |Wrapped configuration declaration
-- Contains 'ConfigurationDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConfigurationDeclaration = PosnWrapper ConfigurationDeclaration

-- |Configuration declaration
-- @
--    configuration_declaration ::=
--       __configuration__ identifier __of__ /entity/_name __is__
--          configuration_declarative_part
--          block_configuration
--       __end__ [ /configuration/_simple_name ] ;
-- @
data ConfigurationDeclaration = ConfigurationDeclaration WrappedSimpleName WrappedName ConfigurationDeclarativePart WrappedBlockConfiguration (Maybe WrappedSimpleName)
                              deriving (Show)

-- |Configuration declarative part
-- @
--    configuration_declarative_part ::=
--       { configuration_declarative_item }
-- @
type ConfigurationDeclarativePart = [WrappedConfigurationDeclarativeItem]

-- |Wrapped configuration declarative item
-- Contains 'ConfigurationDeclarativeItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConfigurationDeclarativeItem = PosnWrapper ConfigurationDeclarativeItem

-- |Configuration declarative item
-- @
--    configuration_declarative_item ::=
--       use_clause
--       | attribute_specification
-- @
data ConfigurationDeclarativeItem =
   -- |Use clause
   -- > configuration_declarative_item ::= use_clause
   ConfigurationDeclarativeItem_UseClause                UseClause
   -- |Use clause
   -- > configuration_declarative_item ::= attribute_specification
   | ConfigurationDeclarativeItem_AttributeSpecification AttributeSpecification
   deriving(Show)

-- |Wrapped block configuration
-- Contains 'BlockConfiguration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedBlockConfiguration = PosnWrapper BlockConfiguration

-- |Block Configuration
-- @
--    block_configuration ::=
--       for block_specification
--          { use_clause }
--          { configuration_item }
--       end for ;
-- @
data BlockConfiguration = BlockConfiguration WrappedBlockSpecification [WrappedUseClause] [WrappedConfigurationItem]
                        deriving (Show)

-- |Wrapped block specification
-- Contains 'BlockSpecification' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedBlockSpecification = PosnWrapper BlockSpecification

-- |Block Specification
-- @
--    block_specification ::=
--       /architecture/_name
--       | /block_statement/_label
--       | /generate_statement/_label [ ( index_specification ) ]
-- @
-- = NOTE
-- 'BlockSpecification' does not exactly match BNF
-- * 'BlockSpecification_Generate' covers the exact case
--     > /generate_statement/_label ( index_specification )
-- * 'BlockSpecification_Name' covers all other cases
data BlockSpecification =
   -- |Names and labels
   -- @
   --    BlockSpecification_Name ::=
   --       /architecture/_name
   --       | /block_statement/_label
   --       | /generate_statement/_label
   -- @
   BlockSpecification_Name Name
   -- |Generate statement
   -- @
   --    BlockSpecification_Generate ::=
   --       /generate_statement/_label ( index_specification )
   -- @
   | BlockSpecification_Generate WrappedSimpleName WrappedIndexSpecification
   deriving(Show)

-- |Wrapped index specification
-- Contains 'IndexSpecification' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedIndexSpecification = PosnWrapper IndexSpecification

-- |Index specification
-- @
--    index_specification ::=
--       discrete_range
--       | /static/_expression
-- @
data IndexSpecification =
   -- |Discrete range
   -- > index_specification ::= discrete_range
   IndexSpecification_DiscreteRange DiscreteRange
   -- |Static expression
   -- > index_specification ::= /static/_expression
   | IndexSpecification_Expression  Expression
   deriving(Show)

-- |Wrapped configuration item
-- Contains 'ConfigurationItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConfigurationItem = PosnWrapper ConfigurationItem

-- |Configuration item
-- @
--    configuration_item ::=
--       block_configuration
--       | component_configuration
-- @
data ConfigurationItem =
   ConfigurationItem_BlockConfiguration         BlockConfiguration
   | ConfigurationItem_ComponentConfiguration   ComponentConfiguration
   deriving(Show)

-- |Wrapped component configuration
-- Contains 'ComponentConfiguration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedComponentConfiguration = PosnWrapper ComponentConfiguration

-- |Component configuration
-- @
--    component_configuration ::=
--       __for__ component_specification
--          [ __use__ binding_indication ; ]
--          [ block_configuration ]
--       __end__ __for__ ;
-- @
data ComponentConfiguration = ComponentConfiguration WrappedComponentSpecification (Maybe WrappedBindingIndication) (Maybe WrappedBlockConfiguration)
                            deriving (Show)

------------------------------------------
-- Sequential Statements
------------------------------------------

-- |Wrapped sequential statement
-- Contains 'SequentialStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSequentialStatement = PosnWrapper SequentialStatement

-- |Sequential statement
-- @
--    sequential_statement ::=
--       wait_statement
--       | assertion_statement
--       | signal_assignment_statement
--       | variable_assignment_statement
--       | procedure_call_statement
--       | if_statement
--       | case_statement
--       | loop_statement
--       | next_statement
--       | exit_statement
--       | return_statement
--       | null_statement
--    null_statement ::= __null__ ;
-- @
data SequentialStatement =
   -- |Wait statement
   -- > sequential_statement ::= wait_statement
   SequentialStatement_Wait WaitStatement
   -- |Assertion statement
   -- > sequential_statement ::= assertion_statement
   | SequentialStatement_Assertion AssertionStatement
   -- |Signal assignment statement
   -- > sequential_statement ::= signal_assignment_statement
   | SequentialStatement_SignalAssignment SignalAssignmentStatement
   -- |Variable assignment statement
   -- > sequential_statement ::= variable_assignment_statement
   | SequentialStatement_VariableAssignment VariableAssignmentStatement
   -- |Procedure call statement
   -- > sequential_statement ::= procedure_call_statement
   | SequentialStatement_ProcedureCall ProcedureCallStatement
   -- |If statement
   -- > sequential_statement ::= if_statement
   | SequentialStatement_If IfStatement
   -- |Case statement
   -- > sequential_statement ::= case_statement
   | SequentialStatement_Case CaseStatement
   -- |Loop statement
   -- > sequential_statement ::= loop_statement
   | SequentialStatement_Loop LoopStatement
   -- |Next statement
   -- > sequential_statement ::= next_statement
   | SequentialStatement_Next NextStatement
   -- |Exit statement
   -- > sequential_statement ::= exit_statement
   | SequentialStatement_Exit ExitStatement
   -- |Return statement
   -- > sequential_statement ::= return_statement
   | SequentialStatement_Return ReturnStatement
   -- |Null statement
   -- @
   --    sequential_statement ::= null_statement
   --    null_statement ::= __null__ ;
   -- @
   | SequentialStatement_Null
   deriving(Show)

-- |Sequence of statements
-- @
--    sequence_of_statements ::=
--       { sequential_statement }
-- @
type SequenceOfStatements = [WrappedSequentialStatement]

-- |Wrapped wait statement
-- Contains 'WaitStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedWaitStatement = PosnWrapper WaitStatement

-- |Wait statement
-- @
--    wait_statement ::=
--       __wait__ [ sensitivity_clause ] [ condition_clause ] [ timeout_clause ] ;
--    sensitivity_clause ::= __on__ sensitivity_list
--    condition_clause ::= __until__ condition
--    condition ::= /boolean/_expression
--    timeout_clause ::= __for__ /time/_expression
-- @
data WaitStatement = WaitStatement (Maybe SensitivityList) (Maybe WrappedExpression) (Maybe WrappedExpression)
                   deriving (Show)

-- |Sensitivity list
-- > sensitivity_list ::= /signal/_name { , /signal/_name }
type SensitivityList = [WrappedName]

-- |Wrapped assertion statement
-- Contains 'AssertionStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAssertionStatement = PosnWrapper AssertionStatement

-- |Assertion statement
-- @
--    assertion_statement ::=
--       __assert__ condition
--          [ __report__ expression ]
--          [ __severity__ expression ] ;
--    condition ::= /boolean/_expression
-- @
data AssertionStatement = AssertionStatement WrappedExpression (Maybe WrappedExpression) (Maybe WrappedExpression)
                        deriving (Show)

-- |Wrapped signal assignment statement
-- Contains 'SignalAssignmentStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSignalAssignmentStatement = PosnWrapper SignalAssignmentStatement

-- |Signal assignment statement
-- @
--    signal_assignment_statement ::=
--       target <= [ __transport__ ] waveform ;
-- @
data SignalAssignmentStatement = SignalAssignmentStatement WrappedTarget (Maybe WrappedSignalAssignmentTransport) Waveform
                               deriving (Show)

-- |Wrapped target
-- Contains 'Target' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedTarget = PosnWrapper Target

-- |Target
-- @
--    target ::=
--       name
--       | aggregate
-- @
data Target =
   -- |Name
   -- > target ::= name
   Target_Name Name
   -- |Aggregate
   -- > target ::= aggregate
   | Target_Aggregate Aggregate
   deriving(Show)

-- |Wrapped signal assignment transport
-- Contains 'SignalAssignmentTransport' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSignalAssignmentTransport = PosnWrapper SignalAssignmentTransport

-- |Signal assignment transport
-- > __transport__
data SignalAssignmentTransport = SignalAssignmentTransport
                               deriving(Show)

-- |Waveform
-- @
--    waveform ::=
--       waveform_element { , waveform_element }
-- @
type Waveform = [WrappedWaveformElement]

-- |Wrapped waveform element
-- Contains 'WaveformElement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedWaveformElement = PosnWrapper WaveformElement

-- |Waveform element
-- @
--    waveform_element ::=
--       /value/_expression [ __after__ /time/_expression ]
--       | __null__ [ __after__ /time/_expression ]
-- @
data WaveformElement = Waveform_Expression WrappedExpression (Maybe WrappedExpression)
                     | Waveform_Null (Maybe WrappedExpression)
                     deriving(Show)

-- |Wrapped variable assignment statement
-- Contains 'VariableAssignmentStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedVariableAssignmentStatement = PosnWrapper VariableAssignmentStatement

-- |Variable assignment statement
-- @
--    variable_assignment_statement ::=
--       target := expression ;
-- @
data VariableAssignmentStatement = VariableAssignmentStatement WrappedTarget WrappedExpression
                                 deriving (Show)

-- |Wrapped procedure call statement
-- Contains 'ProcedureCallStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedProcedureCallStatement = PosnWrapper ProcedureCallStatement

-- |Procedure call statement
-- @
--    procedure_call_statement ::=
--       /procedure/_name [ ( actual_parameter_part ) ] ;
-- @
data ProcedureCallStatement = ProcedureCallStatement WrappedName (Maybe ActualParameterPart)
                            deriving (Show)

-- |Wrapped if statement
-- Contains 'IfStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedIfStatement = PosnWrapper IfStatement

-- |If statement
-- @
--    if_statement ::=
--       __if__ condition __then__
--          sequence_of_statements
--       { __elsif__ condition __then__
--          sequence_of_statements }
--       [ __else__
--          sequence_of_statements ]
--       __end__ __if__ ;
-- @
-- = NOTE
-- Implemented with elsif repeated section extracted
data IfStatement = IfStatement WrappedExpression SequenceOfStatements [WrappedElsifStatement] (Maybe SequenceOfStatements)
                 deriving (Show)

-- |Wrapped elsif statement
-- Contains 'ElsifStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedElsifStatement = PosnWrapper ElsifStatement

-- |Elsif statement
-- @
--    elsif_statement ::=
--       __elsif__ condition __then__
--          sequence_of_statements
-- @
data ElsifStatement = ElsifStatement WrappedExpression SequenceOfStatements
                    deriving (Show)

-- |Wrapped case statement
-- Contains 'CaseStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedCaseStatement = PosnWrapper CaseStatement

-- |Case statement
-- @
--    case_statement ::=
--       __case__ expression __is__
--          case_statement_alternative
--          { case_statement_alternative }
--       __end__ __case__ ;
-- @
data CaseStatement = CaseStatement WrappedExpression [WrappedCaseStatementAlternative]
                   deriving (Show)

-- |Wrapped case statement alternative
-- Contains 'CaseStatementAlternative' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedCaseStatementAlternative = PosnWrapper CaseStatementAlternative

-- |Case statement alternative
-- @
--    case_statement_alternative ::=
--       __when__ choices =>
--          sequence_of_statements
-- @
data CaseStatementAlternative = CaseStatementAlternative Choices SequenceOfStatements
                              deriving (Show)

-- |Wrapped loop statement
-- Contains 'LoopStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedLoopStatement = PosnWrapper LoopStatement

-- |Loop statement
-- @
--    loop_statement ::=
--       [ /loop/_label : ]
--          [ iteration_scheme ] __loop__
--             sequence_of_statements
--          __end__ __loop__ [ /loop/_label ] ;
-- @
data LoopStatement = LoopStatement (Maybe WrappedSimpleName) (Maybe WrappedIterationScheme) SequenceOfStatements (Maybe WrappedSimpleName)
                   deriving (Show)

-- |Wrapped iteration scheme
-- Contains 'IterationScheme' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedIterationScheme = PosnWrapper IterationScheme

-- |Iteration scheme
-- @
--    iteration_scheme ::=
--       __while__ condition
--       | __for__ /loop/_parameter_specification
--    parameter_specification ::=
--       identifier __in__ discrete_range
-- @
data IterationScheme = IterationScheme_While WrappedExpression
                     | IterationScheme_For WrappedSimpleName WrappedDiscreteRange
                     deriving(Show)

-- |Wrapped next statement
-- Contains 'NextStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedNextStatement = PosnWrapper NextStatement

-- |Next statement
-- @
--    next_statement ::=
--       __next__ [ /loop/_label ] [ __when__ condition ] ;
-- @
data NextStatement = NextStatement (Maybe WrappedSimpleName) (Maybe WrappedExpression)
                   deriving (Show)

-- |Wrapped exit statement
-- Contains 'ExitStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedExitStatement = PosnWrapper ExitStatement

-- |Exit statement
-- @
--    exit_statement ::=
--       __exit__ [ /loop/_label ] [ __when__ condition ] ;
-- @
data ExitStatement = ExitStatement (Maybe WrappedSimpleName) (Maybe WrappedExpression)
                   deriving (Show)

-- |Wrapped return statement
-- Contains 'ReturnStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedReturnStatement = PosnWrapper ReturnStatement

-- |Return statement
-- @
--    return_statement ::=
--       __return__ [ expression ] ;
-- @
newtype ReturnStatement = ReturnStatement (Maybe WrappedExpression)
                        deriving (Show)

------------------------------------------
-- Concurrent Statements
------------------------------------------

-- |Wrapped concurrent statement
-- Contains 'ConcurrentStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConcurrentStatement = PosnWrapper ConcurrentStatement

-- |Concurrent statement
-- @
--    concurrent_statement ::=
--       block_statement
--       | process_statement
--       | concurrent_procedure_call
--       | concurrent_assertion_statement
--       | concurrent_signal_assignment_statement
--       | component_instantiation_statement
--       | generate_statement
-- @
data ConcurrentStatement =
   -- |Block statement
   -- > concurrent_statement ::= block_statement
   Concurrent_BlockStatement BlockStatement
   -- |Process statement
   -- > concurrent_statement ::= process_statement
   | Concurrent_ProcessStatement ProcessStatement
   -- |Concurrent procedure call
   -- > concurrent_statement ::= concurrent_procedure_call
   | Concurrent_ProcedureCall ConcurrentProcedureCall
   -- |Concurrent assertion statement
   -- > concurrent_statement ::= concurrent_assertion_statement
   | Concurrent_AssertionStatement ConcurrentAssertionStatement
   -- |Concurrent signal assignment statement
   -- > concurrent_statement ::= concurrent_signal_assignment_statement
   | Concurrent_SignalAssignmentStatement ConcurrentSignalAssignmentStatement
   -- |Component instantiation statement
   -- > concurrent_statement ::= component_instantiation_statement
   | Concurrent_ComponentInstantiationStatement ComponentInstantiationStatement
   -- |Generate statement
   -- > concurrent_statement ::= generate_statement
   | Concurrent_GenerateStatement GenerateStatement
   deriving(Show)

-- |Wrapped block statement
-- Contains 'BlockStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedBlockStatement = PosnWrapper BlockStatement

-- |Block statement
-- @
--    block_statement ::=
--       /block/_label :
--          __block__ [ ( /guard/_expression ) ]
--             block_header
--             block_declarative_part
--          __begin__
--             block_statement_part
--          __end__ __block__ [ /block/_label ] ;
-- @
data BlockStatement = BlockStatement WrappedSimpleName (Maybe WrappedExpression) BlockHeader BlockDeclarativePart BlockStatementPart (Maybe WrappedSimpleName)
                    deriving (Show)

-- |Block header
-- @
--    block_header ::=
--       [ generic_clause
--       [ generic_map_aspect ; ] ]
--       [ port_clause
--       [ port_map_aspect ; ] ]
-- @
-- = NOTE
-- Implemented as
-- @
--    block_header ::=
--       [ block_header_generic ]
--       [ block_header_port ]
-- @
data BlockHeader = BlockHeader (Maybe WrappedBlockHeader_Generic) (Maybe WrappedBlockHeader_Port)
                 deriving (Show)

-- |Wrapped block header generic
-- Contains 'BlockHeader_Generic' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedBlockHeader_Generic = PosnWrapper BlockHeader_Generic

-- |Block header generic
-- @
--    block_header_generic ::=
--       generic_clause
--       [ generic_map_aspect ; ]
-- @
data BlockHeader_Generic = BlockHeader_Generic WrappedGenericClause (Maybe WrappedGenericMapAspect)
                         deriving (Show)

-- |Wrapped block header port
-- Contains 'BlockHeader_Port' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedBlockHeader_Port = PosnWrapper BlockHeader_Port

-- |Block header port
-- @
--    block_header_port ::=
--       port_clause
--       [ port_map_aspect ; ]
-- @
data BlockHeader_Port = BlockHeader_Port WrappedPortClause (Maybe WrappedPortMapAspect)
                      deriving (Show)

-- |Block declarative part
-- @
--    block_declarative_part ::=
--       { block_declarative_item }
-- @
type BlockDeclarativePart = [WrappedBlockDeclarativeItem]

-- |Block statement part
-- @
--    block_statement_part ::=
--       { concurrent_statement }
-- @
type BlockStatementPart = [WrappedConcurrentStatement]

-- |Wrapped process statement
-- Contains 'ProcessStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedProcessStatement = PosnWrapper ProcessStatement

-- |Process statement
-- @
--    process_statement ::=
--       [ /process/_label : ]
--          __process__ [ ( sensitivity_list ) ]
--             process_declarative_part
--          __begin__
--             process_statement_part
--          __end__ __process__ [ /process/_label ] ;
--    label ::= identifier
-- @
data ProcessStatement = ProcessStatement (Maybe WrappedSimpleName) (Maybe SensitivityList) ProcessDeclarativePart ProcessStatementPart (Maybe WrappedSimpleName)
                      deriving (Show)

-- |Process declarative part
-- @
--    process_declarative_part ::=
--       { process_declarative_item }
-- @
type ProcessDeclarativePart = [WrappedProcessDeclarativeItem]

-- |Wrapped process declarative item
-- Contains 'ProcessDeclarativeItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedProcessDeclarativeItem = PosnWrapper ProcessDeclarativeItem

-- |Process declarative item
-- @
--    process_declarative_item ::=
--       subprogram_declaration
--       | subprogram_body
--       | type_declaration
--       | subtype_declaration
--       | constant_declaration
--       | variable_declaration
--       | file_declaration
--       | alias_declaration
--       | attribute_declaration
--       | attribute_specification
--       | use_clause
-- @
data ProcessDeclarativeItem =
   -- |Subprogram declaration
   -- > process_declarative_item ::= subprogram_declaration
   ProcessDeclarative_SubprogramDeclaration     SubprogramDeclaration
   -- |Subprogram body
   -- > process_declarative_item ::= subprogram_body
   | ProcessDeclarative_SubprogramBody            SubprogramBody
   -- |Type declaration
   -- > process_declarative_item ::= type_declaration
   | ProcessDeclarative_TypeDeclaration           TypeDeclaration
   -- |Subtype declaration
   -- > process_declarative_item ::= subtype_declaration
   | ProcessDeclarative_SubtypeDeclaration        SubtypeDeclaration
   -- |Constant declaration
   -- > process_declarative_item ::= constant_declaration
   | ProcessDeclarative_ConstantDeclaration       ConstantDeclaration
   -- |Variable declaration
   -- > process_declarative_item ::= variable_declaration
   | ProcessDeclarative_VariableDeclaration       VariableDeclaration
   -- |File declaration
   -- > process_declarative_item ::= file_declaration
   | ProcessDeclarative_FileDeclaration           FileDeclaration
   -- |Alias declaration
   -- > process_declarative_item ::= alias_declaration
   | ProcessDeclarative_AliasDeclaration          AliasDeclaration
   -- |Attribute declaration
   -- > process_declarative_item ::= attribute_declaration
   | ProcessDeclarative_AttributeDeclaration      AttributeDeclaration
   -- |Attribute specification
   -- > process_declarative_item ::= attribute_specification
   | ProcessDeclarative_AttributeSpecification    AttributeSpecification
   -- |Use clause
   -- > process_declarative_item ::= use_clause
   | ProcessDeclarative_UseClause                 UseClause
   deriving(Show)

-- |Process statement part
-- @
--    process_statement_part ::=
--       { sequential_statement }
-- @
type ProcessStatementPart = [WrappedSequentialStatement]

-- |Wrapped concurrent procedure call
-- Contains 'ConcurrentProcedureCall' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConcurrentProcedureCall = PosnWrapper ConcurrentProcedureCall

-- |Concurrent procedure call
-- @
--    concurrent_procedure_call ::=
--       [ label : ] procedure_call_statement
-- @
data ConcurrentProcedureCall = ConcurrentProcedureCall (Maybe WrappedSimpleName) WrappedProcedureCallStatement
                             deriving (Show)

-- |Wrapped concurrent assertion statement
-- Contains 'ConcurrentAssertionStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConcurrentAssertionStatement = PosnWrapper ConcurrentAssertionStatement

-- |Concurrent assertion statement
-- @
--    concurrent_assertion_statement ::=
--       [ label : ] assertion_statement
-- @
data ConcurrentAssertionStatement = ConcurrentAssertionStatement (Maybe WrappedSimpleName) WrappedAssertionStatement
                                  deriving (Show)

-- |Wrapped concurrent signal assignment statement
-- Contains 'ConcurrentSignalAssignmentStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConcurrentSignalAssignmentStatement = PosnWrapper ConcurrentSignalAssignmentStatement

-- | Concurrent signal assignment statement
-- @
--    concurrent_signal_assignment_statement ::=
--          [ label : ] conditional_signal_assignment
--       |  [ label : ] selected_signal_assignment
--    conditional_signal_assignment ::=
--       target <= options conditional_waveforms ;
--    conditional_waveforms ::=
--       { waveform __when__ condition __else__ }
--       waveform
--    selected_signal_assignment ::=
--       __with__ expression __select__
--          target <= options selected_waveforms ;
--    selected_waveforms ::=
--       { waveform __when__ choices , }
--       waveform __when__ choices
-- @
-- = NOTE
-- Implemented as
-- @
--    conditional_waveforms ::=
--       { conditional_waveform_pair } waveform
--    selected_waveforms ::=
--       { waveform __when__ choices , } waveform __when__ choices
-- @
data ConcurrentSignalAssignmentStatement =
   ConditionalSignalAssignment (Maybe WrappedSimpleName) WrappedTarget SignalAssignmentOptions [WrappedConditionalWaveformPair] Waveform
   | SelectedSignalAssignment (Maybe WrappedSimpleName) WrappedExpression WrappedTarget SignalAssignmentOptions [WrappedSelectedWaveformPair]
   deriving(Show)

-- |Signal assignment options
-- > options ::= [ __guarded__ ] [ __transport__ ]
data SignalAssignmentOptions = SignalAssignmentOptions (Maybe WrappedSignalAssignment_Guarded) (Maybe WrappedSignalAssignment_Transport)
                             deriving (Show)

-- |Wrapped signal assignment guarded
-- Contains 'SignalAssignment_Guarded' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSignalAssignment_Guarded = PosnWrapper SignalAssignment_Guarded

-- |Signal assignment option
-- > __guarded__
data SignalAssignment_Guarded = SignalAssignment_Guarded
                              deriving (Show)

-- |Wrapped signal assignment transport
-- Contains 'SignalAssignment_Transport' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSignalAssignment_Transport = PosnWrapper SignalAssignment_Transport

-- |Signal assignment option
-- > __transport__
data SignalAssignment_Transport = SignalAssignment_Transport
                                deriving (Show)

-- |Wrapped conditional waveform pair
-- Contains 'ConditionalWaveformPair' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConditionalWaveformPair = PosnWrapper ConditionalWaveformPair

-- |Conditional waveform pair
-- @
--    conditional_waveform_pair ::=
--       { waveform __when__ condition __else__ }
-- @
data ConditionalWaveformPair = ConditionalWaveformPair Waveform WrappedExpression
                             deriving (Show)

-- |Wrapped selected waveform pair
-- Contains 'SelectedWaveformPair' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSelectedWaveformPair = PosnWrapper SelectedWaveformPair

-- |Selected waveform pair
-- @
--    selected_waveform_pair ::=
--       { waveform __when__ choices , }
-- @
data SelectedWaveformPair = SelectedWaveformPair Waveform Choices
                          deriving (Show)

-- |Wrapped component instantiation statement
-- Contains 'ComponentInstantiationStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedComponentInstantiationStatement = PosnWrapper ComponentInstantiationStatement

-- |Component instantiation statement
-- @
--    component_instantiation_statement ::=
--       /instantiation/_label :
--          /component/_name
--             [ generic_map_aspect ]
--             [ port_map_aspect ] ;
-- @
data ComponentInstantiationStatement = ComponentInstantiationStatement WrappedSimpleName WrappedName (Maybe WrappedGenericMapAspect) (Maybe WrappedPortMapAspect)
                                     deriving (Show)

-- |Wrapped generate statement
-- Contains 'GenerateStatement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedGenerateStatement = PosnWrapper GenerateStatement

-- |Generate statement
-- @
--    generate_statement ::=
--       /generate/_label :
--          generation_scheme __generate__
--             { concurrent_statement }
--          __end__ __generate__ [ /generate/_label ] ;
--    label ::= identifier
-- @
data GenerateStatement = GenerateStatement WrappedSimpleName WrappedGenerationScheme [WrappedConcurrentStatement] (Maybe WrappedSimpleName)
                       deriving (Show)

-- |Wrapped generation scheme
-- Contains 'GenerationScheme' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedGenerationScheme = PosnWrapper GenerationScheme

-- |Generation scheme
-- @
--    generation_scheme ::=
--       __for__ /generate/_parameter_specification
--       | __if__ condition
--    parameter_specification ::=
--       identifier __in__ discrete_range
-- @
data GenerationScheme =
   -- |Generation scheme: for
   -- @
   --    generation_scheme ::=
   --       __for__ /generate/_parameter_specification
   --    parameter_specification ::=
   --       identifier __in__ discrete_range
   -- @
   GenerationScheme_For WrappedSimpleName WrappedDiscreteRange
   -- |Generation scheme: if
   -- > generation_scheme ::= __if__ condition
   | GenerationScheme_If WrappedExpression
   deriving(Show)

------------------------------------------
-- Subprograms and Packages
------------------------------------------

-- |Wrapped subprogram declaration
-- Contains 'SubprogramDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSubprogramDeclaration = PosnWrapper SubprogramDeclaration

-- |Subprogram Declaration
-- @
--    subprogram_declaration ::=
--       subprogram_specification ;
-- @
newtype SubprogramDeclaration = SubprogramDeclaration SubprogramSpecification
                              deriving (Show)

-- |Wrapped subprogram specification
-- Contains 'SubprogramSpecification' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSubprogramSpecification = PosnWrapper SubprogramSpecification

-- |Subprogram Specification
-- @
--    subprogram_specification ::=
--       __procedure__ designator [ ( formal_parameter_list ) ]
--       | __function__ designator [ ( formal_parameter_list ) ] __return__ type_mark
--    type_mark ::=
--       /type/_name
--       | /subtype/_name
-- @
data SubprogramSpecification =
   -- |Procedure declaration
   -- @
   --    subprogram_specification ::=
   --       __procedure__ designator [ ( formal_parameter_list ) ]
   -- @
   ProcedureDeclaration WrappedDesignator (Maybe FormalParameterList)
   -- |Function declaration
   -- @
   --    subprogram_specification ::=
   --       | __function__ designator [ ( formal_parameter_list ) ] __return__ type_mark
   --    type_mark ::=
   --       /type/_name
   --       | /subtype/_name
   -- @
   | FunctionDeclaration WrappedDesignator (Maybe FormalParameterList) WrappedName
   deriving(Show)

-- |Wrapped designator
-- Contains 'Designator' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedDesignator = PosnWrapper Designator

-- |Designator
-- @
--    designator ::= identifier | operator_symbol
--    operator_symbol ::= string_literal
-- @
data Designator =
   -- |Designator identifier
   -- > designator ::= identifier
   Designator_Identifier String
   -- |Designator operator
   -- @
   --    designator ::= operator_symbol
   --    operator_symbol ::= string_literal
   -- @
   | Designator_Operator String
   deriving(Show)

-- |Formal parameter list
-- > formal_parameter_list ::= /parameter/_interface_list
type FormalParameterList = InterfaceList

-- |Wrapped subprogram body
-- Contains 'SubprogramBody' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSubprogramBody = PosnWrapper SubprogramBody

-- |Subprogram Body
-- @
--    subprogram_body ::=
--       subprogram_specification __is__
--          subprogram_declarative_part
--       __begin__
--          subprogram_statement_part
--       __end__ [ designator ] ;
-- @
data SubprogramBody = SubprogramBody WrappedSubprogramSpecification SubprogramDeclarativePart SubprogramStatementPart (Maybe WrappedDesignator)
                    deriving (Show)

-- |Subprogram declarative part
-- @
--    subprogram_declarative_part ::=
--       { subprogram_declarative_item }
-- @
type SubprogramDeclarativePart = [WrappedSubprogramDeclarativeItem]

-- |Wrapped subprogram declarative item
-- Contains 'SubprogramDeclarativeItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSubprogramDeclarativeItem = PosnWrapper SubprogramDeclarativeItem

-- |Subprogram declarative item
-- @
--    subprogram_declarative_item ::=
--       subprogram_declaration
--       | subprogram_body
--       | type_declaration
--       | subtype_declaration
--       | constant_declaration
--       | variable_declaration
--       | file_declaration
--       | alias_declaration
--       | attribute_declaration
--       | attribute_specification
--       | use_clause
-- @
data SubprogramDeclarativeItem =
   -- |Subprogram declaration
   -- > subprogram_declarative_item ::= subprogram_declaration
   SubprogramDeclarativeItem_SubprogramDeclaration SubprogramDeclaration
   -- |Subprogram body
   -- > subprogram_declarative_item ::= subprogram_body
   | SubprogramDeclarativeItem_SubprogramBody SubprogramBody
   -- |Type declaration
   -- > subprogram_declarative_item ::= type_declaration
   | SubprogramDeclarativeItem_TypeDeclaration TypeDeclaration
   -- |Subtype declaration
   -- > subprogram_declarative_item ::= subtype_declaration
   | SubprogramDeclarativeItem_SubtypeDeclaration SubtypeDeclaration
   -- |Subtype declaration
   -- > subprogram_declarative_item ::= constant_declaration
   | SubprogramDeclarativeItem_ConstantDeclaration ConstantDeclaration
   -- |Variable declaration
   -- > subprogram_declarative_item ::= variable_declaration
   | SubprogramDeclarativeItem_VariableDeclaration VariableDeclaration
   -- |File declaration
   -- > subprogram_declarative_item ::= file_declaration
   | SubprogramDeclarativeItem_FileDeclaration FileDeclaration
   -- |Alias declaration
   -- > subprogram_declarative_item ::= alias_declaration
   | SubprogramDeclarativeItem_AliasDeclaration AliasDeclaration
   -- |Attribute declaration
   -- > subprogram_declarative_item ::= attribute_declaration
   | SubprogramDeclarativeItem_AttributeDeclaration AttributeDeclaration
   -- |Attribute specification
   -- > subprogram_declarative_item ::= attribute_specification
   | SubprogramDeclarativeItem_AttributeSpecification AttributeSpecification
   -- |Use clause
   -- > subprogram_declarative_item ::= use_clause
   | SubprogramDeclarativeItem_UseClause UseClause
   deriving(Show)

-- |Subprogram statement part
-- @
--    subprogram_statement_part ::=
--       { sequential_statement }
-- @
type SubprogramStatementPart = [WrappedSequentialStatement]

-- |Wrapped package declaration
-- Contains 'PackageDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPackageDeclaration = PosnWrapper PackageDeclaration

-- |Package declaration
-- @
--    package_declaration ::=
--       __package__ identifier __is__
--          package_declarative_part
--       __end__ [ /package/_simple_name ] ;
-- @
data PackageDeclaration = PackageDeclaration WrappedSimpleName PackageDeclarativePart (Maybe WrappedSimpleName)
                        deriving (Show)

-- |Package declarative part
-- @
--    package_declarative_part ::=
--       { package_declarative_item }
-- @
type PackageDeclarativePart = [WrappedPackageDeclarativeItem]

-- |Wrapped package declarative item
-- Contains 'PackageDeclarativeItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPackageDeclarativeItem = PosnWrapper PackageDeclarativeItem

-- |Package declarative item
-- @
--    package_declarative_item ::=
--       subprogram_declaration
--       | type_declaration
--       | subtype_declaration
--       | constant_declaration
--       | signal_declaration
--       | file_declaration
--       | alias_declaration
--       | component_declaration
--       | attribute_declaration
--       | attribute_specification
--       | disconnection_specification
--       | use_clause
-- @
data PackageDeclarativeItem =
   -- |Subprogram declaration
   -- > package_declarative_item ::= subprogram_declaration
   PackageDeclarativeItem_SubprogramDeclaration          SubprogramDeclaration
   -- |Type declaration
   -- > package_declarative_item ::= type_declaration
   | PackageDeclarativeItem_TypeDeclaration              TypeDeclaration
   -- |Subtype declaration
   -- > package_declarative_item ::= subtype_declaration
   | PackageDeclarativeItem_SubtypeDeclaration           SubtypeDeclaration
   -- |Constant declaration
   -- > package_declarative_item ::= constant_declaration
   | PackageDeclarativeItem_ConstantDeclaration          ConstantDeclaration
   -- |Signal declaration
   -- > package_declarative_item ::= signal_declaration
   | PackageDeclarativeItem_SignalDeclaration            SignalDeclaration
   -- |File declaration
   -- > package_declarative_item ::= file_declaration
   | PackageDeclarativeItem_FileDeclaration              FileDeclaration
   -- |Alias declaration
   -- > package_declarative_item ::= alias_declaration
   | PackageDeclarativeItem_AliasDeclaration             AliasDeclaration
   -- |Component declaration
   -- > package_declarative_item ::= component_declaration
   | PackageDeclarativeItem_ComponentDeclaration         ComponentDeclaration
   -- |Attribute declaration
   -- > package_declarative_item ::= attribute_declaration
   | PackageDeclarativeItem_AttributeDeclaration         AttributeDeclaration
   -- |Attribute specification
   -- > package_declarative_item ::= attribute_specification
   | PackageDeclarativeItem_AttributeSpecification       AttributeSpecification
   -- |Disconnection specification
   -- > package_declarative_item ::= disconnection_specification
   | PackageDeclarativeItem_DisconnectionSpecification   DisconnectionSpecification
   -- |Use clause
   -- > package_declarative_item ::= use_clause
   | PackageDeclarativeItem_UseClause                    UseClause
   deriving(Show)

-- |Wrapped package body
-- Contains 'PackageBody' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPackageBody = PosnWrapper PackageBody

-- |Package body
-- @
--    package_body ::=
--       __package__ __body__ /package/_simple_name __is__
--          package_body_declarative_part
--       __end__ [ /package/_simple_name ] ;
-- @
data PackageBody = PackageBody WrappedSimpleName PackageBodyDeclarativePart (Maybe WrappedSimpleName)
                 deriving (Show)

-- |Package body declarative part
-- @
--    package_body_declarative_part ::=
--       { package_body_declarative_item }
-- @
type PackageBodyDeclarativePart = [WrappedPackageBodyDeclarativeItem]

-- |Wrapped package body declarative item
-- Contains 'PackageBodyDeclarativeItem' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPackageBodyDeclarativeItem = PosnWrapper PackageBodyDeclarativeItem

-- |Package body declarative item
-- @
--    package_body_declarative_item ::=
--       subprogram_declaration
--       | subprogram_body
--       | type_declaration
--       | subtype_declaration
--       | constant_declaration
--       | file_declaration
--       | alias_declaration
--       | use_clause
-- @
data PackageBodyDeclarativeItem =
   -- |Subprogram declaration
   -- > package_body_declarative_item ::= subprogram_declaration
   PackageBodyDeclarativeItem_SubprogramDeclaration   SubprogramDeclaration
   -- |Subprogram body
   -- > package_body_declarative_item ::= subprogram_body
   | PackageBodyDeclarativeItem_SubprogramBody        SubprogramBody
   -- |Type declaration
   -- > package_body_declarative_item ::= type_declaration
   | PackageBodyDeclarativeItem_TypeDeclaration       TypeDeclaration
   -- |Subtype declaration
   -- > package_body_declarative_item ::= subtype_declaration
   | PackageBodyDeclarativeItem_SubtypeDeclaration    SubtypeDeclaration
   -- |Constant declaration
   -- > package_body_declarative_item ::= constant_declaration
   | PackageBodyDeclarativeItem_ConstantDeclaration   ConstantDeclaration
   -- |File declaration
   -- > package_body_declarative_item ::= file_declaration
   | PackageBodyDeclarativeItem_FileDeclaration       FileDeclaration
   -- |Alias declaration
   -- > package_body_declarative_item ::= alias_declaration
   | PackageBodyDeclarativeItem_AliasDeclaration      AliasDeclaration
   -- |Use clause
   -- > package_body_declarative_item ::= use_clause
   | PackageBodyDeclarativeItem_UseClause             UseClause
   deriving(Show)

------------------------------------------
-- Names and Expressions
------------------------------------------

-- |Wrapped name
-- Contains 'Name' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedName = PosnWrapper Name

-- |Name type
-- @
--    name ::=
--       simple_name
--       | operator_symbol
--       | selected_name
--       | indexed_name
--       | slice_name
--       | attribute_name
-- @
data Name =
   -- |Simple name ('String')
   Name_Simple SimpleName
   -- |Operator type ('StringLiteral')
   | Name_Operator OperatorSymbol
   -- |Selected name ('SelectedName')
   | Name_Selected SelectedName
   -- |Indexed name ('IndexedName')
   | Name_Indexed IndexedName
   -- |Slice name ('SliceName')
   | Name_Slice SliceName
   -- |Attribute name ('AttributeName')
   | Name_Attribute AttributeName
   deriving(Show)

-- |Wrapped simple name
-- Contains 'SimpleName' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSimpleName = PosnWrapper SimpleName

-- |Simple name type
-- @
--    simple_name ::= identifier
-- @
-- Just a 'String'
type SimpleName = String

-- |Operator symbol type
-- @
--    operator_symbol ::= string_literal
-- @
type OperatorSymbol  = StringLiteral

-- |Wrapped selected name
-- Contains 'SelectedName' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSelectedName = PosnWrapper SelectedName

-- |Selected name typ
-- @
--    selected_name ::= prefix . suffix
-- @
data SelectedName = SelectedName WrappedPrefix WrappedSuffix
                  deriving (Show)

-- |Indexed name type
-- @
--    indexed_name ::= prefix ( expression { , expression } )
-- @
-- Implemented as:
-- @
--    indexed_name ::= prefix ( expression_list )
-- @
data IndexedName = IndexedName WrappedPrefix [WrappedExpression]
                 deriving (Show)

-- |Slice name type
-- @
--    slice_name ::= prefix ( discrete_range )
-- @
data SliceName = SliceName WrappedPrefix WrappedDiscreteRange
               deriving (Show)

-- |Wrapped attribute name
-- Contains 'AttributeName' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAttributeName = PosnWrapper AttributeName

-- |Attribute name type
-- @
--    attribute_name ::=
--       prefix ' attribute_designator [ ( /static/_expression ) ]
--    attribute_designator ::= /attribute/_simple_name
-- @
data AttributeName = AttributeName WrappedPrefix WrappedSimpleName (Maybe WrappedExpression)
                   deriving (Show)

-- |Wrapped prefix
-- Contains 'Prefix' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPrefix = PosnWrapper Prefix

-- |Prefix
-- @
--    prefix ::=
--       name
--       | function_call
-- @
-- = NOTE
-- 'Prefix' does not exactly match BNF
-- * 'Prefix_Function' covers the exact case
--     > /function/_name ( actual_parameter_part )
-- * 'Prefix_Name' covers all other cases
data Prefix =
   -- |
   -- @
   --    prefix ::=
   --       name
   --       | /function/_name
   -- @
   Prefix_Name Name
   -- |
   -- @
   --    prefix ::=
   --       /function/_name ( actual_parameter_part )
   -- @
   | Prefix_Function FunctionCall
   deriving(Show)

-- |Wrapped suffix
-- Contains 'Suffix' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSuffix = PosnWrapper Suffix

-- |Suffix
-- @
--    suffix ::=
--       simple_name
--       | character_literal
--       | operator_symbol
--       | all
-- @
data Suffix =
   -- |Simple name
   Suffix_Name SimpleName
   -- |Character literal
   | Suffix_Char Char
   -- |Operator symbol
   | Suffix_Operator OperatorSymbol
   -- |All keyword
   | Suffix_All
   deriving(Show)

-- |Wrapped function call
-- Contains 'FunctionCall' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedFunctionCall = PosnWrapper FunctionCall

-- |Function Call
-- @
--    function_call ::=
--       /function/_name [ ( actual_parameter_part ) ]
-- @
-- = NOTE
-- Only matches full case, other case is caught by 'Name'
-- > /function/_name ( actual_parameter_part )
data FunctionCall = FunctionCall WrappedName ActualParameterPart
                  deriving (Show)

-- |Actual parameter part
-- @
--    actual_parameter_part ::= /parameter/_association_list
-- @
type ActualParameterPart = AssociationList

-- |Wrapped expression
-- Contains 'Expression' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedExpression = PosnWrapper Expression

-- |Expression
-- @
--    expression ::=
--         relation { __and__ relation }
--       | relation { __or__ relation }
--       | relation { __xor__ relation }
--       | relation [ __nand__ relation ]
--       | relation [ __nor__ relation ]
-- @
data Expression =
   -- |And Expression
   -- > expression ::= relation __and__ relation { __and__ relation }
   Expression_And [WrappedRelation]
   -- |Or Expression
   -- > expression ::= relation __or__ relation { __or__ relation }
   | Expression_Or [WrappedRelation]
   -- |Xor Expression
   -- > expression ::= relation __xor__ relation { __xor__ relation }
   | Expression_Xor [WrappedRelation]
   -- |Nand Expression
   -- > expression ::= relation __nand__ relation
   | Expression_Nand WrappedRelation WrappedRelation
   -- |Nor Expression
   -- > expression ::= relation __nor__ relation
   | Expression_Nor WrappedRelation WrappedRelation
   -- |Empty Expression
   -- > expression ::= relation
   | Expression_Relation Relation
   deriving(Show)

-- |Wrapped relation
-- Contains 'Relation' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedRelation = PosnWrapper Relation

-- |Relation
-- @
--    relation ::=
--       simple_expression [ relational_operator simple_expression ]
-- @
data Relation =
   -- |Relation of two terms
   -- > relation ::= simple_expression relational_operator simple_expression
   Relation_Compare WrappedSimpleExpression WrappedRelationalOperator WrappedSimpleExpression
   -- |Relation with a simple term
   -- > relation ::= simple_expression
   | Relation_Term SimpleExpression
   deriving(Show)

-- |Wrapped simple expression
-- Contains 'SimpleExpression' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSimpleExpression = PosnWrapper SimpleExpression

-- |Simple expression
-- @
--    simple_expression ::=
--       [ sign ] term { adding_operator term }
-- @
-- = NOTE
-- Implemented as:
-- @
--    simple_expression ::=
--       [ sign ] term { adding_operation }
-- @
data SimpleExpression = SimpleExpression (Maybe WrappedSign) WrappedTerm [WrappedAddingOperation]
                      deriving (Show)

-- |Wrapped relational operator
-- Contains 'RelationalOperator' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedRelationalOperator = PosnWrapper RelationalOperator

-- |Relational operator
-- > relational_operator ::= = | /= | < | <= | > | >=
data RelationalOperator =
   -- |Relation = relation
   -- > relational_operator ::= =
   Relation_Equals
   -- |Relation /= relation
   -- > relational_operator ::= /=
   | Relation_NotEquals
   -- |Relation < relation
   -- > relational_operator ::= <
   | Relation_LessThan
   -- |Relation <= relation
   -- > relational_operator ::= <=
   | Relation_LessThanOrEqual
   -- |Relation > relation
   -- > relational_operator ::= >
   | Relation_GreaterThan
   -- |Relation >= relation
   -- > relational_operator ::= >=
   | Relation_GreaterThanOrEqual
   deriving(Show)

-- |Wrapped sign
-- Contains 'Sign' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSign = PosnWrapper Sign

-- |Sign
-- > sign ::= + | -
data Sign =
   -- |Positive sign
   -- > sign ::= +
   Positive
   -- |Negative sign
   -- > sign ::= -
   | Negative
   deriving(Show)

-- |Wrapped term
-- Contains 'Term' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedTerm = PosnWrapper Term

-- |Term
-- @
--    term ::=
--       factor { multiplying_operator factor }
-- @
-- = NOTE
-- Implemented as:
-- @
--    term ::=
--       factor { multiplying_operation }
-- @
data Term = Term WrappedFactor [WrappedMultiplyingOperation]
          deriving (Show)

-- |Wrapped adding operation
-- Contains 'AddingOperation' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAddingOperation = PosnWrapper AddingOperation

-- |Adding operation
-- > adding_operation ::= adding_operator term
data AddingOperation = AddingOperation WrappedAddingOperator WrappedTerm
                     deriving (Show)

-- |Wrapped adding operator
-- Contains 'AddingOperator' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAddingOperator = PosnWrapper AddingOperator

-- |Adding operator
-- > adding_operator ::= + | - | &
data AddingOperator =
   -- |Add operator
   -- > adding_operator ::= +
   Add
   -- |Subtract operator
   -- > adding_operator ::= -
   | Minus
   -- |Concatenation operator
   -- > adding_operator ::= &
   | Concat
   deriving(Show)

-- |Wrapped multiplying operation
-- Contains 'MultiplyingOperation' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedMultiplyingOperation = PosnWrapper MultiplyingOperation

-- |Multipying operation
-- > multiplying_operation ::= multiplying_operator factor
data MultiplyingOperation = MultiplyingOperation WrappedMultiplyingOperator WrappedFactor
                          deriving (Show)

-- |Wrapped multiplying operator
-- Contains 'MultiplyingOperator' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedMultiplyingOperator = PosnWrapper MultiplyingOperator

-- |Multiplying operator
-- > multiplying_operator ::= * | / | __mod__ | __rem__
data MultiplyingOperator =
   -- |Multiply operator
   -- > multiplying_operator ::= *
   Multiply
   -- |Divide operator
   -- > multiplying_operator ::= /
   | Divide
   -- |Modulus operator
   -- > multiplying_operator ::= __mod__
   | Mod
   -- |Remainder operator
   -- > multiplying_operator ::= __rem__
   | Rem
   deriving(Show)

-- |Wrapped Factor
-- Contains 'Factor' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedFactor = PosnWrapper Factor

-- |Factor
-- @
-- factor ::=
--    primary [ ** primary ]
--    | __abs__ primary
--    | __not__ primary
-- @
data Factor =
   -- |Factor single value
   -- > factor ::= primary
   Factor_Value Primary
   -- |Factor power factor
   -- > factor ::= primary ** primary
   | Factor_Pow WrappedPrimary WrappedPrimary
   -- |Factor absolute
   -- > factor ::= __abs__ primary
   | Factor_Abs WrappedPrimary
   -- |Factor not
   -- > factor ::= __not__ primary
   | Factor_Not WrappedPrimary
   deriving(Show)

-- |Wrapped Primary
-- Contains 'Primary' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPrimary = PosnWrapper Primary

-- |Primary
-- @
--    primary ::=
--       name
--       | literal
--       | aggregate
--       | function_call
--       | qualified_expression
--       | type_conversion
--       | allocator
--       | ( expression )
-- @
-- = NOTE
-- 'Primary' does not exactly match BNF
-- * 'Primary_FunctionCall' covers the exact case function_call
--     > /function/_name ( actual_parameter_part )
-- * 'Primary_Name' covers other case of function_call
data Primary =
   -- |Primary name
   -- @
   --    primary ::=
   --       name
   --       | /function/_name
   -- @
   Primary_Name Name
   -- |Primary literal
   -- > primary ::= literal
   | Primary_Literal Literal
   -- |Primary aggregate
   -- > primary ::= aggregate
   | Primary_Aggregate Aggregate
   -- |Primary function_call
   -- @
   --    primary ::=
   --       /function/_name ( actual_parameter_part )
   -- @
   | Primary_FunctionCall FunctionCall
   -- |Primary qualified_expression
   -- > primary ::= qualified_expression
   | Primary_QualifiedExpression QualifiedExpression
   -- |Primary type_conversion
   -- > primary ::= type_conversion
   | Primary_TypeConversion TypeConversion
   -- |Primary allocator
   -- > primary ::= allocator
   | Primary_Allocator Allocator
   -- |Primary expression
   -- > primary ::= ( expression )
   | Primary_Expression WrappedExpression
   deriving(Show)

-- |Aggregate
-- @
--    aggregate ::=
--       ( element_assocation { , element_assocation } )
-- @
type Aggregate = [WrappedElementAssociation]

-- |Wrapped qualified expression
-- Contains 'QualifiedExpression' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedQualifiedExpression = PosnWrapper QualifiedExpression

-- |Qualified expression
-- @
--    qualified_expression ::=
--       type_mark ' ( expresssion )
--       | type_mark ' aggregate
--    type_mark ::=
--       /type/_name
--       | /subtype/_name
-- @
data QualifiedExpression = QualifiedExpression_Expression WrappedName WrappedExpression
                         | QualifiedExpression_Aggregate WrappedName Aggregate
                         deriving(Show)

-- |Type conversion
-- @
--    type_conversion ::= type_mark ( expression )
--    type_mark ::=
--       /type/_name
--       | /subtype/_name
-- @
data TypeConversion = TypeConversion WrappedName WrappedExpression
                    deriving (Show)

-- |Wrapped allocator
-- Contains 'Allocator' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAllocator = PosnWrapper Allocator

-- |Allocator
-- @
--    allocator ::=
--       __new__ subtype_indication
--       | __new__ qualified_expression
-- @
data Allocator =
   -- |Allocator subtype
   -- > allocator ::= __new__ subtype_indication
   Allocator_Subtype WrappedSubtypeIndication
   -- |Allocator expression
   -- > allocator ::= __new__ qualified_expression
   | Allocator_Expression WrappedQualifiedExpression
   deriving(Show)

-- |Index Constraint
-- > index_constraint ::= ( discrete_range { , discrete_range } )
type IndexConstraint = [WrappedDiscreteRange]

-- |Wrapped range constraint
-- Contains 'RangeConstraint' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedRangeConstraint = PosnWrapper RangeConstraint

-- |Range Constraint
-- > range_constraint ::= __range__ range_definition
newtype RangeConstraint = RangeConstraint WrappedRange
                        deriving (Show)

-- |Wrapped range definition
-- Contains 'Range' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedRange = PosnWrapper Range

-- |Range definition
-- @
--    range_definition ::=
--       /range/_attribute_name
--       | simple_expression direction simple_expression
-- @
data Range =
   -- |Range attribute name
   -- > range_definition ::= /range/_attribute_name
   RangeAttributeName AttributeName
   -- |Range expression
   -- > range_definition ::= simple_expression direction simple_expression
   | RangeExpression WrappedSimpleExpression WrappedDirection WrappedSimpleExpression
   deriving(Show)

-- |Wrapped direction
-- Contains 'Direction' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedDirection = PosnWrapper Direction

-- |Direction
-- > direction ::= __to__ | __downto__
data Direction =
   -- |To
   -- > direction ::= __to__
   To
   -- |Downto
   -- > direction ::= __downto__
   | Downto
   deriving(Show)

-- |Wrapped discrete range
-- Contains 'DiscreteRange' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedDiscreteRange = PosnWrapper DiscreteRange

-- |Discrete range
-- @
--    discrete_range ::=
--       /discrete/_subtype_indication
--       | range_definition
-- @
data DiscreteRange = DiscreteRange_SubtypeIndication SubtypeIndication
                   | DiscreteRange_Range Range
                   deriving(Show)

-- |Wrapped element association
-- Contains 'ElementAssociation' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedElementAssociation = PosnWrapper ElementAssociation

-- |Element association
-- @
--    element_association ::=
--       [ choices => ] expression
-- @
data ElementAssociation = ElementAssociation (Maybe Choices) WrappedExpression
                        deriving (Show)

-- |Choices
-- > choices ::= choice { | choice }
type Choices = [WrappedChoice]

-- |Wrapped choice
-- Contains 'Choice' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedChoice = PosnWrapper Choice

-- |Choice
-- @
--    choice ::=
--       simple_expression
--       | discrete_range
--       | /element/_simple_name
--       | __others__
-- @
data Choice =
   Choice_Expression SimpleExpression
   | Choice_DiscreteRange DiscreteRange
   | Choice_ElementName SimpleName
   | Choice_Others
   deriving(Show)

------------------------------------------
-- Declarations
------------------------------------------

-- |Wrapped type declaration
-- Contains 'TypeDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedTypeDeclaration = PosnWrapper TypeDeclaration

-- |Type declaration
-- @
--    type_declaration ::=
--       full_type_declaration
--       | incomplete_type_declaration
--    full_type_declaration ::=
--       __type__ identifier __is__ type_definition ;
--    incomplete_type_declaration ::= __type__ identifier ;
-- @
data TypeDeclaration =
   -- |Full type declaration
   -- @
   --    type_declaration ::=
   --       full_type_declaration
   --    full_type_declaration ::=
   --       __type__ identifier __is__ type_definition ;
   -- @
   FullTypeDeclaration WrappedSimpleName WrappedTypeDefinition
   -- |Incomplete type definition
   -- @
   --    type_declaration ::=
   --       incomplete_type_declaration
   --    incomplete_type_declaration ::= __type__ identifier ;
   -- @
   | IncompleteTypeDefinition WrappedSimpleName
   deriving(Show)

-- |Wrapped subtype declaration
-- Contains 'SubtypeDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSubtypeDeclaration = PosnWrapper SubtypeDeclaration

-- |Subtype declaration
-- @
--    subtype_declaration ::=
--       __subtype__ identifier __is__ subtype_indication ;
-- @
data SubtypeDeclaration = SubtypeDeclaration WrappedSimpleName WrappedSubtypeIndication
                        deriving (Show)

-- |Wrapped subtype indication
-- Contains 'SubtypeIndication' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSubtypeIndication = PosnWrapper SubtypeIndication

-- |Subtype Indication
-- @
-- subtype_indication ::=
--    [ /resolution_function/_name ] type_mark [ constraint ]
-- type_mark ::=
--    /type/_name
--    | /subtype/_name
-- @
data SubtypeIndication = SubtypeIndication (Maybe WrappedName) WrappedName (Maybe WrappedConstraint)
                       deriving (Show)

-- |Wrapped constraint
-- Contains 'Constraint' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConstraint = PosnWrapper Constraint

-- |Constraint
-- @
--    constraint ::=
--       range_constraint
--       | index_constraint
-- @
data Constraint =
   -- |Range constraint
   -- > constraint ::= range_constraint
   Constraint_Range RangeConstraint
   -- |Index constraint
   -- > constraint ::= index_constraint
   | Constraint_Index IndexConstraint
   deriving(Show)

-- |Wrapped constant declaration
-- Contains 'ConstantDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConstantDeclaration = PosnWrapper ConstantDeclaration

-- |Constant declaration
-- @
--    constant_declaration ::=
--       __constant__ identifier_list : subtype_indication [ := expression ] ;
-- @
data ConstantDeclaration = ConstantDeclaration [WrappedSimpleName] WrappedSubtypeIndication (Maybe WrappedExpression)
                         deriving (Show)

-- |Wrapped signal declaration
-- Contains 'SignalDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSignalDeclaration = PosnWrapper SignalDeclaration

-- |Signal declaration
-- @
--    signal_declaration ::=
--       __signal__ identifier_list : subtype_indication [ signal_kind ] [ := expression ] ;
-- @
data SignalDeclaration = SignalDeclaration [WrappedSimpleName] WrappedSubtypeIndication (Maybe WrappedSignalKind) (Maybe WrappedExpression)
                       deriving (Show)

-- |Wrapped signal kind
-- Contains 'SignalKind' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSignalKind = PosnWrapper SignalKind

-- |Signal kind
-- > signal_kind ::= __register__ | __bus__
data SignalKind =
   -- |Register signal
   -- > signal_kind ::= __register__
   Register
   -- |Bus signal
   -- > signal_kind ::= __bus__
   | Bus
   deriving(Show)

-- |Wrapped variable declaration
-- Contains 'VariableDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedVariableDeclaration = PosnWrapper VariableDeclaration

-- |Variable declaration
-- @
--    variable_declaration ::=
--       __variable__ identifier_list : subtype_indication [ := expression ] ;
-- @
data VariableDeclaration = VariableDeclaration [WrappedSimpleName] WrappedSubtypeIndication (Maybe WrappedExpression)
                         deriving (Show)

-- |Wrapped file declaration
-- Contains 'FileDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedFileDeclaration = PosnWrapper FileDeclaration

-- |File declaration
-- @
--    file_declaration ::=
--       __file__ identifier : subtype_indication __is__ [ mode ] file_logical_name ;
--    file_logical_name ::= /string/_expression
-- @
data FileDeclaration = FileDeclaration WrappedSimpleName WrappedSubtypeIndication (Maybe WrappedMode) WrappedExpression
                     deriving (Show)

-- |Wrapped interface declaration
-- Contains 'InterfaceDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedInterfaceDeclaration = PosnWrapper InterfaceDeclaration

-- |Interface declaration
-- @
--    interface_declaration ::=
--       interface_constant_declaration
--       | interface_signal_declaration
--       | interface_variable_declaration
--    interface_constant_declaration ::=
--       [ __constant__ ] identifier_list : [ __in__ ] subtype_indication [ := /static/_expression ]
--    interface_signal_declaration ::=
--       [ __signal__ ] identifier_list : [ mode ] subtype_indication [ __bus__ ] [ := /static/_expression ]
--    interface_variable_declaration ::=
--       [ __variable__ ] identifier_list : [ mode ] subtype_indication [ := /static/_expression ]
-- @
data InterfaceDeclaration = InterfaceDeclaration (Maybe WrappedInterfaceType) [WrappedSimpleName] (Maybe WrappedMode) WrappedSubtypeIndication (Maybe WrappedExpression)
                          deriving (Show)

-- |Wrapped interface type
-- Contains 'InterfaceType' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedInterfaceType = PosnWrapper InterfaceType

-- |Interface types
data InterfaceType =
   Constant
   | Signal
   | GuardedSignal
   | Variable
   deriving(Show)

-- |Wrapped mode
-- Contains 'Mode' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedMode = PosnWrapper Mode

-- |Mode
-- > mode ::= __in__ | __out__ | __inout__ | __buffer__ | __linkage__
data Mode =
   -- |In
   -- > mode ::= __in__
   In
   -- |Out
   -- > mode ::= __out__
   | Out
   -- |Inout
   -- > mode ::= __inout__
   | Inout
   -- |Buffer
   -- > mode ::= __buffer__
   | Buffer
   -- |Linkage
   -- > mode ::= __linkage__
   | Linkage
   deriving(Show)

-- |Interface list
-- @
--    interface_list ::=
--       interface_element { ; interface_element }
--    interface_element ::= interface_declaration
-- @
type InterfaceList = [WrappedInterfaceDeclaration]

-- |Association list
-- @
--    assocation_list ::= association_element { , association_element }
-- @
type AssociationList = [WrappedAssociationElement]

-- |Wrapped association element
-- Contains 'AssociationElement' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAssociationElement = PosnWrapper AssociationElement

-- |Association element
-- @
--    association_element ::= [ formal_part => ] actual_part
-- @
data AssociationElement = AssociationElement (Maybe WrappedFormalPart) WrappedActualPart
                        deriving (Show)

-- |Wrapped formal part
-- Contains 'FormalPart' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedFormalPart = PosnWrapper FormalPart

-- |Formal part
-- @
--    formal_part ::=
--       formal_designator
--       | /function/_name ( formal_designator )
--    formal_designator ::=
--       /generic/_name
--       | /port/_name
--       | /parameter/_name
-- @
data FormalPart =
   -- |Formal part designator
   -- @
   --    formal_part ::= formal_designator
   -- @
   FormalPart_Designator WrappedName
   -- |Formal part function
   -- @
   --    formal_part ::= /function/_name ( formal_designator )
   -- @
   | FormalPart_Function WrappedName WrappedName
   deriving(Show)

-- |Wrapped actual part
-- Contains 'ActualPart' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedActualPart = PosnWrapper ActualPart

-- |Actual part
-- @
--    actual_part ::=
--       actual_designator
--       | /function/_name ( actual_designator )
-- @
data ActualPart =
   -- |Actual part designator
   -- @
   --    actual_part ::= actual_designator
   -- @
   ActualPart_Designator ActualDesignator
   -- |Actual part function
   -- @
   --    actual_part ::= /function/_name ( actual_designator )
   -- @
   | ActualPart_Function WrappedName WrappedActualDesignator
   deriving(Show)

-- |Wrapped designator
-- Contains 'ActualDesignator' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedActualDesignator = PosnWrapper ActualDesignator

-- |Actual designator
-- @
--    actual_designator ::=
--       expression
--       | /signal/_name
--       | /variable/_name
--       | __open__
-- @
data ActualDesignator =
   -- |Actual designator expression
   ActualDesignator_Expression Expression
   -- |Actual designator name
   -- @
   --    actual_designator ::=
   --       /signal/_name
   --       | /variable/_name
   --       | __open__
   -- @
   | ActualDesignator_Name Name
   -- |Actual designator open
   | ActualDesignator_Open
   deriving(Show)

-- |Wrapped alias declaration
-- Contains 'AliasDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAliasDeclaration = PosnWrapper AliasDeclaration

-- |Alias declaration
-- @
--    alias_declaration ::=
--       __alias__ identifier : subtype_indication __is__ name ;
-- @
data AliasDeclaration = AliasDeclaration WrappedSimpleName WrappedSubtypeIndication WrappedName
                      deriving (Show)

-- |Wrapped attribute declaration
-- Contains 'AttributeDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAttributeDeclaration = PosnWrapper AttributeDeclaration

-- |Attribute declaration
-- @
--    attribute_declaration ::=
--       __attribute__ identifier : type_mark ;
-- @
data AttributeDeclaration = AttributeDeclaration WrappedSimpleName WrappedName
                          deriving (Show)

-- |Wrapped component declaration
-- Contains 'ComponentDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedComponentDeclaration = PosnWrapper ComponentDeclaration

-- |Component declaration
-- @
--    component_declaration ::=
--       __component__ identifier
--          [ /local/_generic_clause ]
--          [ /local/_port_clause ]
--       __end__ __component__ ;
-- @
data ComponentDeclaration = ComponentDeclaration WrappedSimpleName (Maybe WrappedGenericClause) (Maybe WrappedPortClause)
                          deriving (Show)

------------------------------------------
-- Specifications
------------------------------------------

-- |Wrapped attribute specification
-- Contains 'AttributeSpecification' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAttributeSpecification = PosnWrapper AttributeSpecification

-- |Attribute specification
-- @
--    attribute_specification ::=
--       __attribute__ attribute_designator __of__ entity_specification __is__ expression ;
--    entity_specification ::=
--       entity_name_list : entity_class
--    attribute_designator ::= /attribute/_simple_name
-- @
data AttributeSpecification = AttributeSpecification WrappedSimpleName WrappedEntityNameList WrappedEntityClass WrappedExpression
                            deriving (Show)

-- |Wrapped entity name list
-- Contains 'EntityNameList' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedEntityNameList = PosnWrapper EntityNameList

-- |Entity name list
-- @
--    entity_name_list ::=
--       entity_designator { , entity_designator }
--       | __others__
--       | __all__
-- @
data EntityNameList =
   -- |List
   -- @
   --    entity_name_list ::=
   --       entity_designator { , entity_designator }
   -- @
   EntityNameList_List [WrappedEntityDesignator]
   -- |Others
   -- @
   --    entity_name_list ::=
   --       __others__
   -- @
   | EntityNameList_Others
   -- |All
   -- @
   --    entity_name_list ::=
   --       __all__
   -- @
   | EntityNameList_All
   deriving(Show)

-- |Wrapped entity class
-- Contains 'EntityClass' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedEntityClass = PosnWrapper EntityClass

-- |Entity class
-- @
--    entity_class ::=
--       __entity__
--       | __architecture__
--       | __configuration__
--       | __procedure__
--       | __function__
--       | __package__
--       | __type__
--       | __subtype__
--       | __constant__
--       | __signal__
--       | __variable__
--       | __component__
--       | __label__
-- @
data EntityClass =
   -- |Entity class
   -- > entity_class ::= __entity__
   EntityClass_Entity
   -- |Architecture class
   -- > entity_class ::= __architecture__
   | EntityClass_Architecture
   -- |Configuration class
   -- > entity_class ::= __configuration__
   | EntityClass_Configuration
   -- |Procedure class
   -- > entity_class ::= __procedure__
   | EntityClass_Procedure
   -- |Function class
   -- > entity_class ::= __function__
   | EntityClass_Function
   -- |Package class
   -- > entity_class ::= __package__
   | EntityClass_Package
   -- |Type class
   -- > entity_class ::= __type__
   | EntityClass_Type
   -- |Subtype class
   -- > entity_class ::= __subtype__
   | EntityClass_Subtype
   -- |Constant class
   -- > entity_class ::= __constant__
   | EntityClass_Constant
   -- |Signal class
   -- > entity_class ::= __signal__
   | EntityClass_Signal
   -- |Variable class
   -- > entity_class ::= __variable__
   | EntityClass_Variable
   -- |Component class
   -- > entity_class ::= __component__
   | EntityClass_Component
   -- |Label class
   -- > entity_class ::= __label__
   | EntityClass_Label
   deriving(Show)

-- |Wrapped entity designator
-- Contains 'EntityDesignator' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedEntityDesignator = PosnWrapper EntityDesignator

-- |Entity designator
-- > entity_designator ::= simple_name | operator_symbol
data EntityDesignator =
   EntityDesignator_Name String
   | EntityDesignator_Operator String
   deriving(Show)

-- |Wrapped configuration specification
-- Contains 'ConfigurationSpecification' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedConfigurationSpecification = PosnWrapper ConfigurationSpecification

-- |Configuration specification
-- @
--    configuration_specification ::=
--       __for__ component_specification __use__ binding_indication ;
-- @
data ConfigurationSpecification = ConfigurationSpecification WrappedComponentSpecification WrappedBindingIndication
                                deriving (Show)

-- |Wrapped component specification
-- Contains 'ComponentSpecification' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedComponentSpecification = PosnWrapper ComponentSpecification

-- |Component specification
-- @
--    component_specification ::=
--       instantiation_list : /component/_name
-- @
data ComponentSpecification = ComponentSpecification WrappedInstantiationList WrappedName
                            deriving (Show)

-- |Wrapped instantiation list
-- Contains 'InstantiationList' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedInstantiationList = PosnWrapper InstantiationList

-- |Instantiation list
-- @
--    instantiation_list ::=
--       /instantiation/_label { , /instantiation/_label }
--       | __others__
--       | __all__
-- @
data InstantiationList =
   -- |Instantiation label list
   -- > instantiation_list ::= /instantiation/_label { , /instantiation/_label }
   InstantiationList_Label [WrappedSimpleName]
   -- |Others
   -- > instantiation_list ::= __others__
   | InstantiationList_Others
   -- |All
   -- > instantiation_list ::= __all__
   | InstantiationList_All
   deriving(Show)

-- |Wrapped binding indication
-- Contains 'BindingIndication' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedBindingIndication = PosnWrapper BindingIndication

-- |Binding indication
-- @
--    binding_indication ::=
--       entity_aspect
--       [ generic_map_aspect ]
--       [ port_map_aspect ]
-- @
data BindingIndication = BindingIndication WrappedEntityAspect (Maybe AssociationList) (Maybe AssociationList)
                       deriving (Show)

-- |Wrapped entity aspect
-- Contains 'EntityAspect' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedEntityAspect = PosnWrapper EntityAspect

-- |Entity aspect
-- @
--    entity_aspect ::=
--       __entity__ /entity/_name [ ( /architecture/_identifier ) ]
--       | __configuration__ /configuration/_name
--       | __open__
-- @
data EntityAspect =
   -- |Entity
   -- > entity_aspect ::= __entity__ /entity/_name [ ( /architecture/_identifier ) ]
   EntityAspect_Entity WrappedName (Maybe WrappedSimpleName)
   -- |Configuration
   -- > entity_aspect ::= __configuration__ /configuration/_name
   | EntityAspect_Configuration WrappedName
   -- |Open
   -- > entity_aspect ::= __open__
   | EntityAspect_Open
   deriving(Show)

-- |Wrapped generic map aspect
-- Contains 'GenericMapAspect' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedGenericMapAspect = PosnWrapper GenericMapAspect

-- |Generic map aspect
-- @
--    generic_map_aspect ::=
--       __generic__ __map__ ( /generic/_association_list )
-- @
type GenericMapAspect = AssociationList

-- |Wrapped port map aspect
-- Contains 'PortMapAspect' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPortMapAspect = PosnWrapper PortMapAspect

-- |Port map aspect
-- @
--    port_map_aspect ::=
--       __port__ __map__ ( /port/_association_list )
-- @
type PortMapAspect = AssociationList

-- |Wrapped disconnection specification
-- Contains 'DisconnectionSpecification' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedDisconnectionSpecification = PosnWrapper DisconnectionSpecification

-- |Disconnection specification
-- @
--    disconnection_specification ::=
--       __disconnect__ guarded_signal_specification __after__ /time/_expression ;
--    guarded_signal_specification ::=
--       /guarded/_signal_list : type_mark
-- @
data DisconnectionSpecification = DisconnectionSpecification WrappedGuardedSignalList WrappedName WrappedExpression
                                deriving (Show)

-- |Wrapped guarded signal list
-- Contains 'GuardedSignalList' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedGuardedSignalList = PosnWrapper GuardedSignalList

-- |Guarded signal list
-- @
--    signal_list ::=
--       /signal/_name { , /signal/_name }
--       | __others__
--       | __all__
-- @
data GuardedSignalList = GuardedSignal_List [WrappedName]
                       | GuardedSignal_Others
                       | GuardedSignal_All
                       deriving(Show)

------------------------------------------
-- Types
------------------------------------------

-- |Wrapped type definition
-- Contains 'TypeDefinition' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedTypeDefinition = PosnWrapper TypeDefinition

-- |Type definition
-- @
--    type_definition ::=
--       scalar_type_definition
--       | composite_type_definition
--       | access_type_definition
--       | file_type_definition
--    scalar_type_definition ::=
--       enumeration_type_definition
--       | integer_type_definition
--       | floating_type_definition
--       | physical_type_definition
--    enumeration_type_definition ::=
--       ( enumeration_literal { , enumeration_literal } )
--    integer_type_definition ::= range_constraint
--    floating_type_definition ::= range_constraint
--    physical_type_definition ::=
--       range_constraint
--          __units__
--             base_unit_declaration
--             { secondary_unit_declaration }
--          __end__ __units__
--    range_constraint ::= __range__ range_definition
--    base_unit_declaration ::= identifier ;
--    composite_type_definition ::=
--       array_type_definition
--       | record_type_definition
--    array_type_definition ::=
--       unconstrained_array_definition | constrained_array_definition
--    unconstrained_array_definition ::=
--       __array__ ( index_subtype_definition { , index_subtype_definition } )
--          __of__ /element/_subtype_indication
--    constrained_array_definition ::=
--       __array__ index_constraint __of__ /element/_subtype_indication
--    index_subtype_definition ::= type_mark __range__ <>
--    type_mark ::=
--       /type/_name
--       | /subtype/_name
--    record_type_definition ::=
--       __record__
--          element_declaration
--          { element_declaration }
--       __end__ __record__
--    access_type_definition ::= __access__ subtype_indication
--    file_type_definition ::= __file__ __of__ type_mark
-- @
data TypeDefinition =
   -- |Enumeration type definition
   -- @
   --    type_definition ::=
   --       scalar_type_definition
   --    scalar_type_definition ::=
   --       enumeration_type_definition
   --    enumeration_type_definition ::=
   --       ( enumeration_literal { , enumeration_literal } )
   -- @
   EnumerationTypeDefinition [WrappedEnumerationLiteral]
   -- |Universal type definition
   -- @
   --    type_definition ::=
   --       scalar_type_definition
   --    scalar_type_definition ::=
   --       | integer_type_definition
   --       | floating_type_definition
   --    integer_type_definition ::= range_constraint
   --    floating_type_definition ::= range_constraint
   --    range_constraint ::= __range__ range_definition
   -- @
   -- = NOTE
   -- Covers both integer and floating type since these are lexed to universal types
   -- Types will be checked and resolved in the next stage
   | UniversalTypeDefinition WrappedRange
   -- |Physical type definition
   -- @
   --    type_definition ::=
   --       scalar_type_definition
   --    scalar_type_definition ::=
   --       | physical_type_definition
   --    physical_type_definition ::=
   --       range_constraint
   --          __units__
   --             base_unit_declaration
   --             { secondary_unit_declaration }
   --          __end__ __units__
   --    range_constraint ::= __range__ range_definition
   --    base_unit_declaration ::= identifier ;
   -- @
   | PhysicalTypeDefinition WrappedRange WrappedSimpleName [WrappedSecondaryUnitDeclaration]
   -- |Unconstrained array type definition
   -- @
   --    type_definition ::=
   --       composite_type_definition
   --    composite_type_definition ::=
   --       array_type_definition
   --    array_type_definition ::=
   --       unconstrained_array_definition
   --    unconstrained_array_definition ::=
   --       __array__ ( index_subtype_definition { , index_subtype_definition } )
   --          __of__ /element/_subtype_indication
   --    index_subtype_definition ::= type_mark __range__ <>
   --    type_mark ::=
   --       /type/_name
   --       | /subtype/_name
   -- @
   | UnconstrainedArrayTypeDefinition [WrappedName] WrappedSubtypeIndication
   -- |Constrained array type definition
   -- @
   --    type_definition ::=
   --       composite_type_definition
   --    composite_type_definition ::=
   --       array_type_definition
   --    array_type_definition ::=
   --       constrained_array_definition
   --    constrained_array_definition ::=
   --       __array__ index_constraint __of__ /element/_subtype_indication
   -- @
   | ConstrainedArrayTypeDefinition IndexConstraint WrappedSubtypeIndication
   -- |Record type definition
   -- @
   --    type_definition ::=
   --       composite_type_definition
   --    composite_type_definition ::=
   --       record_type_definition
   --    record_type_definition ::=
   --       __record__
   --          element_declaration
   --          { element_declaration }
   --       __end__ __record__
   -- @
   | RecordTypeDefinition [WrappedElementDeclaration]
   -- |Access type definition
   -- @
   --    type_definition ::=
   --       access_type_definition
   --    access_type_definition ::= __access__ subtype_indication
   -- @
   | AccessTypeDefinition WrappedSubtypeIndication
   -- |File type definition
   -- @
   --    type_definition ::=
   --       file_type_definition
   --    type_mark ::=
   --       /type/_name
   --       | /subtype/_name
   --    file_type_definition ::= __file__ __of__ type_mark
   -- @
   | FileTypeDefinition WrappedName
   deriving(Show)

-- |Wrapped secondary unit declaration
-- Contains 'SecondaryUnitDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedSecondaryUnitDeclaration = PosnWrapper SecondaryUnitDeclaration

-- |Secondary unit declaration
-- > secondary_unit_declaration ::= identifier = physical_literal ;
data SecondaryUnitDeclaration = SecondaryUnitDeclaration WrappedSimpleName WrappedPhysicalLiteral
                              deriving (Show)

-- |Wrapped element declaration
-- Contains 'ElementDeclaration' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedElementDeclaration = PosnWrapper ElementDeclaration

-- |Element declaration
-- @
--    element_declaration ::=
--       identifier_list : element_subtype_definition ;
--    identifier_list ::= identifier { , identifier }
--    element_subtype_definition ::= subtype_indication
-- @
data ElementDeclaration = ElementDeclaration [WrappedSimpleName] WrappedSubtypeIndication
                        deriving (Show)

------------------------------------------
-- Literals
------------------------------------------

-- |Wrapped literal type
-- Contains 'Literal' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedLiteral = PosnWrapper Literal

-- |Literal type
-- @
--    literal ::=
--       numeric_literal
--       | enumeration_literal
--       | string_literal
--       | bit_string_literal
--       | __null__
-- @
data Literal =
   -- |Numeric literal type
   Literal_Numeric NumericLiteral
   -- |Enumeration literal type
   | Literal_Enumeration EnumerationLiteral
   -- |String literal type
   | Literal_String StringLiteral
   -- |Bit string literal type
   | Literal_BitStr BitStrLiteral
   -- |Represents *null* keyword
   | Literal_Null
   deriving(Show)

-- |Numeric literal type
-- @
--    numeric_literal ::=
--       abstract_literal
--       | physical_literal
-- @
data NumericLiteral =
   -- |Abstract literal type
   NumericLiteral_Abstract AbstractLiteral
   -- |Physical literal type and position
   | NumericLiteral_Physical PhysicalLiteral
   deriving(Show)

-- |Wrapped enumeration literal type
-- Contains 'EnumerationLiteral' (enumeration literal) and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedEnumerationLiteral = PosnWrapper EnumerationLiteral

-- |Enumeration literal type
-- @
--    enumeration_literal ::= identifier | character_literal
-- @
data EnumerationLiteral =
   -- |Enumerated identifier
   EnumerationLiteral_Identifier String
   -- |Enumerated character
   | EnumerationLiteral_Char Char
   deriving(Eq,Ord,Show)

-- |String literal type
-- Contains 'String' (with no quotation marks)
type StringLiteral = String

-- |Bit string literal type
-- Contains:
-- * Literal base 'TokenTypes.LiteralBase'
-- * Literal string 'ByteString'
data BitStrLiteral = BitStrLiteral TokenTypes.LiteralBase ByteString
                   deriving (Show)

-- |Wrapped universal type literal
-- Contains 'AbstractLiteral' (universal type) and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedAbstractLiteral = PosnWrapper AbstractLiteral

-- |Literals that contain abstract values
-- @
--    abstract_literal ::= decimal_literal | based_literal
-- @
-- = NOTE
-- Lexed output does not match tree specification
-- There are two acceptable types of this literal
-- * Decimal Literals
-- * Based Literals
-- The lexer converts both of these to their representative value:
-- * Integer value
-- * Real - Floating point value
data AbstractLiteral =
   -- |Universal integer type value
   -- Implemented with Haskell 'Data.Int.Int64' type
   UniversalInteger Int64
   -- |Universal real type value
   -- Implemented with Haskell 'Double' type
   | UniversalReal Double
   deriving(Show)

-- |Wrapped physical type literal
-- Contains 'PhysicalLiteral' and 'Parser.Alex.BaseTypes.AlexPosn' (position)
type WrappedPhysicalLiteral = PosnWrapper PhysicalLiteral

-- |Physical literal type
-- @
--    physical_literal ::= [ abstract_literal ] /unit/_name
-- @
-- Contains:
-- * Abstract literal type and position: 'WrappedAbstractLiteral'
-- * Unit name and position: 'WrappedName'
data PhysicalLiteral = PhysicalLiteral (Maybe WrappedAbstractLiteral) WrappedName
                     deriving (Show)
