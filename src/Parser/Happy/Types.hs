module Parser.Happy.Types where

import Data.Int (Int64)
import Data.ByteString.Char8 (ByteString)

import qualified Parser.TokenTypes as TokenTypes

-- |Entity Declaration
-- @
--    entity_declaration ::=
--       entity identifier is
--          entity_header
--          entity_declarative_part
--     [ begin
--          entity_statement_part ]
--       end [ <entity>_simple_name ] ;
-- @
-- <entity>_simple_name must repeat identifier
data EntityDeclaration = EntityDeclaration String EntityHeader EntityDeclarativePart (Maybe EntityStatementPart) (Maybe String)
                       deriving (Show)

-- |Entity Header
-- @
--    entity_header ::=
--       [ <formal>_generic_clause ]
--       [ <formal>_port_clause ]
-- @
data EntityHeader = EntityHeader (Maybe GenericClause) (Maybe PortClause)
                  deriving (Show)

-- |Generic Clause
-- @
--    generic_clause ::=
--       generic ( generic_list ) ;
--    generic_list ::= <generic>_interface_list
-- @
type GenericClause = InterfaceList

-- |Port Clause
-- @
--    port_clause ::=
--       port ( port_list ) ;
--    port_list ::= <port>_interface_list
-- @
type PortClause = InterfaceList

type EntityDeclarativePart = [EntityDeclarativeItem]

data EntityDeclarativeItem = EntityDeclaration_SubprogramDeclaration       SubprogramDeclaration
                           | EntityDeclaration_SubprogramBody              SubprogramBody
                           | EntityDeclaration_TypeDeclaration             TypeDeclaration
                           | EntityDeclaration_SubtypeDeclaration          SubtypeDeclaration
                           | EntityDeclaration_ConstantDeclaration         ConstantDeclaration
                           | EntityDeclaration_SignalDeclaration           SignalDeclaration
                           | EntityDeclaration_FileDeclaration             FileDeclaration
                           | EntityDeclaration_AliasDeclaration            AliasDeclaration
                           | EntityDeclaration_AttributeDeclaration        AttributeDeclaration
                           | EntityDeclaration_AttributeSpecification      AttributeSpecification
                           | EntityDeclaration_DisconnectionSpecification  DisconnectionSpecification
                           | EntityDeclaration_UseClause                   UseClause
                           deriving(Show)

type EntityStatementPart = [EntityStatement]

data EntityStatement = EntityStatement_ConcurrentAssertionStatement  ConcurrentAssertionStatement
                     | EntityStatement_ConcurrentProcedureCall       ConcurrentProcedureCall
                     | EntityStatement_ProcessStatement              ProcessStatement
                     deriving(Show)

data ArchitectureBody = ArchitectureBody String Name ArchitectureDeclarativePart ArchitectureStatementPart (Maybe String)
                      deriving (Show)

type ArchitectureDeclarativePart = [BlockDeclarativeItem]

data BlockDeclarativeItem = BlockDeclarativeItem_SubprogramDeclaration        SubprogramDeclaration
                          | BlockDeclarativeItem_SubprogramBody               SubprogramBody
                          | BlockDeclarativeItem_TypeDeclaration              TypeDeclaration
                          | BlockDeclarativeItem_SubtypeDeclaration           SubtypeDeclaration
                          | BlockDeclarativeItem_ConstantDeclaration          ConstantDeclaration
                          | BlockDeclarativeItem_SignalDeclaration            SignalDeclaration
                          | BlockDeclarativeItem_FileDeclaration              FileDeclaration
                          | BlockDeclarativeItem_AliasDeclaration             AliasDeclaration
                          | BlockDeclarativeItem_ComponentDeclaration         ComponentDeclaration
                          | BlockDeclarativeItem_AttributeDeclaration         AttributeDeclaration
                          | BlockDeclarativeItem_AttributeSpecification       AttributeSpecification
                          | BlockDeclarativeItem_ConfigurationSpecification   ConfigurationSpecification
                          | BlockDeclarativeItem_DisconnectionSpecification   DisconnectionSpecification
                          | BlockDeclarativeItem_UseClause                    UseClause
                          deriving(Show)

type ArchitectureStatementPart = [ConcurrentStatement]

data ConfigurationDeclaration = ConfigurationDeclaration String String ConfigurationDeclarativePart BlockConfiguration (Maybe String)
                              deriving (Show)

type ConfigurationDeclarativePart = [ConfigurationDeclarativeItem]

data ConfigurationDeclarativeItem = ConfigurationDeclarativeItem_UseClause                UseClause
                                  | ConfigurationDeclarativeItem_AttributeSpecification   AttributeSpecification
                                  deriving(Show)

-- |Block Configuration
-- @
--    block_configuration ::=
--       for block_specification
--          { use_clause }
--          { configuration_item }
--       end for ;
-- @
data BlockConfiguration = BlockConfiguration BlockSpecification [UseClause] [ConfigurationItem]
                        deriving (Show)

-- |Block Specification
--    block_specification ::=
--       <architecture>_name
--       | <block_statement>_label
--       | <generate_statement>_label [ ( index_specification ) ]
-- @
-- = NOTE
-- 'BlockSpecification' does not exactly match BNF
-- * 'BlockSpecification_Generate' covers the exact case
--     > <generate_statement>_label ( index_specification )
-- * 'BlockSpecification_Name' covers all other cases
data BlockSpecification =
   -- |
   -- @
   --    BlockSpecification_Name ::=
   --       <architecture>_name
   --       | <block_statement>_label
   --       | <generate_statement>_label
   -- @
   BlockSpecification_Name Name
   -- |
   -- @
   --    BlockSpecification_Generate ::=
   --       <generate_statement>_label ( index_specification )
   -- @
   | BlockSpecification_Generate String IndexSpecification
   deriving(Show)

data IndexSpecification = IndexSpecification_DiscreteRange  DiscreteRange
                        | IndexSpecification_Expression     Expression -- static_expression
                        deriving(Show)

data ConfigurationItem = ConfigurationItem_BlockConfiguration BlockConfiguration
                       | ConfigurationItem_ComponentConfiguration ComponentConfiguration
                       deriving(Show)

data ComponentConfiguration = ComponentConfiguration ComponentSpecification (Maybe BindingIndication) (Maybe BlockConfiguration)
                            deriving (Show)

data ComponentSpecification = ComponentSpecification InstantiationList Name
                            deriving (Show)

newtype SubprogramDeclaration = SubprogramDeclaration SubprogramSpecification
                              deriving (Show)

data SubprogramSpecification = ProcedureDeclaration Designator (Maybe FormalParameterList)
                             | FunctionDeclaration Designator (Maybe FormalParameterList) Name
                             deriving(Show)

type FormalParameterList = InterfaceList

data Designator = Designator_Identifier String
                | Designator_Operator TokenTypes.OperatorType
                | Designator_Keyword TokenTypes.ReservedWord
                deriving(Show)

data SubprogramBody = SubprogramBody SubprogramSpecification SubprogramDeclarativePart SubprogramStatementPart (Maybe Designator)
                    deriving (Show)

type SubprogramDeclarativePart = [SubprogramDeclarativeItem]

data SubprogramDeclarativeItem = SubprogramDeclarativeItem_SubprogramDeclaration SubprogramDeclaration
                               | SubprogramDeclarativeItem_SubprogramBody SubprogramBody
                               | SubprogramDeclarativeItem_TypeDeclaration TypeDeclaration
                               | SubprogramDeclarativeItem_SubtypeDeclaration SubtypeDeclaration
                               | SubprogramDeclarativeItem_ConstantDeclaration ConstantDeclaration
                               | SubprogramDeclarativeItem_VariableDeclaration VariableDeclaration
                               | SubprogramDeclarativeItem_FileDeclaration FileDeclaration
                               | SubprogramDeclarativeItem_AliasDeclaration AliasDeclaration
                               | SubprogramDeclarativeItem_AttributeDeclaration AttributeDeclaration
                               | SubprogramDeclarativeItem_AttributeSpecification AttributeSpecification
                               | SubprogramDeclarativeItem_UseClause UseClause
                               deriving(Show)

type SubprogramStatementPart = [SequentialStatement]

data PackageDeclaration = PackageDeclaration String PackageDeclarativePart (Maybe String)
                        deriving (Show)

type PackageDeclarativePart = [PackageDeclarativeItem]

data PackageDeclarativeItem = PackageDeclarativeItem_SubprogramDeclaration       SubprogramDeclaration
                            | PackageDeclarativeItem_TypeDeclaration             TypeDeclaration
                            | PackageDeclarativeItem_SubtypeDeclaration          SubtypeDeclaration
                            | PackageDeclarativeItem_ConstantDeclaration         ConstantDeclaration
                            | PackageDeclarativeItem_SignalDeclaration           SignalDeclaration
                            | PackageDeclarativeItem_FileDeclaration             FileDeclaration
                            | PackageDeclarativeItem_AliasDeclaration            AliasDeclaration
                            | PackageDeclarativeItem_ComponentDeclaration        ComponentDeclaration
                            | PackageDeclarativeItem_AttributeDeclaration        AttributeDeclaration
                            | PackageDeclarativeItem_AttributeSpecification      AttributeSpecification
                            | PackageDeclarativeItem_DisconnectionSpecification  DisconnectionSpecification
                            | PackageDeclarativeItem_UseClause                   UseClause
                            deriving(Show)

data PackageBody = PackageBody String PackageBodyDeclarativePart (Maybe String)
                 deriving (Show)

type PackageBodyDeclarativePart = [PackageBodyDeclarativeItem]

data PackageBodyDeclarativeItem = PackageBodyDeclarativeItem_SubprogramDeclaration SubprogramDeclaration
                                | PackageBodyDeclarativeItem_SubprogramBody        SubprogramBody
                                | PackageBodyDeclarativeItem_TypeDeclaration       TypeDeclaration
                                | PackageBodyDeclarativeItem_SubtypeDeclaration    SubtypeDeclaration
                                | PackageBodyDeclarativeItem_ConstantDeclaration   ConstantDeclaration
                                | PackageBodyDeclarativeItem_FileDeclaration       FileDeclaration
                                | PackageBodyDeclarativeItem_AliasDeclaration      AliasDeclaration
                                | PackageBodyDeclarativeItem_UseClause             UseClause
                                deriving(Show)

data ScalarTypeDefinition = EnumerationTypeDefinition [EnumerationLiteral]
                          | UniversalTypeDefinition Range
                          | PhysicalTypeDefinition Range String [SecondaryUnitDeclaration]
                          deriving(Show)

data EnumerationLiteral = EnumerationLiteral_Identifier String
                        | EnumerationLiteral_Char Char
                        deriving(Show)

data Range = RangeAttributeName AttributeName
           | RangeExpression SimpleExpression Direction SimpleExpression
           deriving(Show)

data Direction = To
               | Downto
               deriving(Show)

data SecondaryUnitDeclaration = SecondaryUnitDeclaration String PhysicalLiteral
                              deriving (Show)

data PhysicalLiteral = PhysicalLiteral (Maybe AbstractLiteral) String
                     deriving (Show)

data CompositeTypeDefinition = Composite_ArrayTypeDefinition ArrayTypeDefinition
                             | RecordTypeDefinition [ElementDeclaration]
                             deriving(Show)

data ArrayTypeDefinition = UnconstrainedArrayTypeDefinition [String] SubtypeIndication
                         | ConstrainedArrayTypeDefinition IndexConstraint SubtypeIndication
                         deriving(Show)

type IndexConstraint = [DiscreteRange]

data DiscreteRange = DiscreteRange_SubtypeIndication SubtypeIndication
                   | DiscreteRange_Range Range
                   deriving(Show)

data ElementDeclaration = ElementDeclaration [String] SubtypeIndication
                        deriving (Show)

data Declaration = Declaration_Type TypeDeclaration
                 | Declaration_Subtype SubtypeDeclaration
                 | Declaration_Object ObjectDeclaration
                 | Declaration_File FileDeclaration
                 | Declaration_Interface InterfaceDeclaration
                 | Declaration_Alias AliasDeclaration
                 | Declaration_Attribute AttributeDeclaration
                 | Declaration_Component ComponentDeclaration
                 | Declaration_Entity EntityDeclaration
                 | Declaration_Configuration ConfigurationDeclaration
                 | Declaration_Subprogram SubprogramDeclaration
                 | Declaration_Package PackageDeclaration
                 deriving(Show)

data TypeDeclaration = FullTypeDeclaration String TypeDefinition
                     | IncompleteTypeDefinition String
                     deriving(Show)

data TypeDefinition = TypeDefinition_Scalar ScalarTypeDefinition
                    | TypeDefinition_Composite CompositeTypeDefinition
                    | TypeDefinition_Access SubtypeIndication
                    | TypeDefinition_File Name
                    deriving(Show)

data SubtypeDeclaration = SubtypeDeclaration String SubtypeIndication
                        deriving (Show)

data SubtypeIndication = SubtypeIndication (Maybe Name) Name (Maybe Constraint)
                       deriving (Show)

data Constraint = Constraint_Range Range
                | Constraint_Index IndexConstraint
                deriving(Show)

data ObjectDeclaration = ObjectDeclaration_Constant ConstantDeclaration
                       | ObjectDeclaration_Signal SignalDeclaration
                       | ObjectDeclaration_Variable VariableDeclaration
                       deriving(Show)

data ConstantDeclaration = ConstantDeclaration [String] SubtypeIndication (Maybe Expression)
                         deriving (Show)

data SignalDeclaration = SignalDeclaration [String] SubtypeIndication (Maybe SignalKind) (Maybe Expression)
                       deriving (Show)

data SignalKind = Register
                | Bus
                deriving(Show)

data VariableDeclaration = VariableDeclaration [String] SubtypeIndication (Maybe Expression)
                         deriving (Show)

data FileDeclaration = FileDeclaration String SubtypeIndication (Maybe Mode) Expression
                     deriving (Show)

data InterfaceDeclaration = InterfaceDeclaration (Maybe InterfaceType) [String] (Maybe Mode) SubtypeIndication (Maybe Expression)
                          deriving (Show)

data InterfaceType = Constant
                   | Signal
                   | GuardedSignal
                   | Variable
                   deriving(Show)

data Mode = In
          | Out
          | Inout
          | Buffer
          | Linkage
          deriving(Show)

type InterfaceList = [InterfaceDeclaration]

type AssociationList = [AssociationElement]

data AssociationElement = AssociationElement (Maybe FormalPart) ActualPart
                        deriving (Show)

data FormalPart = FormalPart_Designator Name
                | FormalPart_Function Name Name
                deriving(Show)

data ActualPart = ActualPart_Designator ActualDesignator
                | ActualPart_Function Name ActualDesignator
                deriving(Show)

data ActualDesignator = ActualDesignator_Expression Expression
                      | ActualDesignator_Name Name
                      | ActualDesignator_Open
                      deriving(Show)

data AliasDeclaration = AliasDeclaration String SubtypeIndication Name
                      deriving (Show)

data AttributeDeclaration = AttributeDeclaration String Name
                          deriving (Show)

data ComponentDeclaration = ComponentDeclaration String (Maybe GenericClause) (Maybe PortClause)
                          deriving (Show)

data AttributeSpecification = AttributeSpecification String AttributeSpecificationEntityNameList EntityClass Expression
                            deriving (Show)

data EntityClass = EntityClass_Entity
                 | EntityClass_Architecture
                 | EntityClass_Configuration
                 | EntityClass_Procedure
                 | EntityClass_Function
                 | EntityClass_Package
                 | EntityClass_Type
                 | EntityClass_Subtype
                 | EntityClass_Constant
                 | EntityClass_Signal
                 | EntityClass_Variable
                 | EntityClass_Component
                 | EntityClass_Label
                 deriving(Show)

data AttributeSpecificationEntityNameList = AttributeSpecificationEntityName_List [EntityDesignator]
                                          | AttributeSpecificationEntityName_Others
                                          | AttributeSpecificationEntityName_All
                                          deriving(Show)

data EntityDesignator = EntityDesignator_Name String
                      | EntityDesignator_Operator TokenTypes.OperatorType
                      deriving(Show)

data ConfigurationSpecification = ConfigurationSpecification ComponentSpecification BindingIndication
                                deriving (Show)

data InstantiationList = InstantiationList_Label [String]
                       | InstantiationList_Others
                       | InstantiationList_All
                       deriving(Show)

data BindingIndication = BindingIndication EntityAspect (Maybe AssociationList) (Maybe AssociationList)
                       deriving (Show)

data EntityAspect = EntityAspect_Entity Name (Maybe String)
                  | EntityAspect_Configuration Name
                  | EntityAspect_Open
                  deriving(Show)

data DisconnectionSpecification = DisconnectionSpecification GuardedSignalList Name Expression
                                deriving (Show)

data GuardedSignalList = GuardedSignal_List [Name]
                       | GuardedSignal_Others
                       | GuardedSignal_All
                       deriving(Show)

data Name = Name_Simple SimpleName
          | Name_Operator TokenTypes.OperatorType
          | Name_Selected SelectedName
          | Name_Indexed IndexedName
          | Name_Slice SliceName
          | Name_Attribute AttributeName
          deriving(Show)

type SimpleName = String

data SelectedName = SelectedName Prefix Suffix
                  deriving (Show)

data IndexedName = IndexedName Prefix [Expression]
                 deriving (Show)

data SliceName = SliceName Prefix DiscreteRange
               deriving (Show)

-- |Prefix
--    prefix ::=
--       name
--       | function_call
-- @
-- = NOTE
-- 'Prefix' does not exactly match BNF
-- * 'Prefix_Function' covers the exact case
--     > <function>_name ( actual_parameter_part )
-- * 'Prefix_Name' covers all other cases
data Prefix =
   -- |
   -- @
   --    prefix ::=
   --       name
   --       | <function>_name
   -- @
   Prefix_Name Name
   -- |
   -- @
   --    prefix ::=
   --       <function>_name ( actual_parameter_part )
   -- @
   | Prefix_Function FunctionCall
   deriving(Show)

data Suffix = Suffix_Name String
            | Suffix_Char Char
            | Suffix_Operator TokenTypes.OperatorType
            | Suffix_All
            deriving(Show)

data AttributeName = AttributeName Prefix String (Maybe Expression)
                   deriving (Show)

data Expression = Expression_And AndExpression
                | Expression_Or OrExpression
                | Expression_Xor XorExpression
                | Expression_Nand NandExpression
                | Expression_Nor NorExpression
                | Expression_Relation Relation
                deriving(Show)

data AndExpression = AndExpression Relation AndExpression
                   | AndRelation Relation Relation
                   deriving(Show)

data OrExpression = OrExpression Relation OrExpression
                   | OrRelation Relation Relation
                   deriving(Show)

data XorExpression = XorExpression Relation XorExpression
                   | XorRelation Relation Relation
                   deriving(Show)

data NandExpression = NandExpression Relation Relation
                    deriving (Show)

data NorExpression = NorExpression Relation Relation
                   deriving (Show)

data Relation = Relation_Compare SimpleExpression RelationalOperator SimpleExpression
              | Relation_Term SimpleExpression
              deriving(Show)

data SimpleExpression = SimpleExpression Sign Term [AddingOperation]
                      deriving (Show)

data Sign = Positive
          | Negative
          deriving(Show)

data AddingOperation = AddingOperation AddingOperator Term
                     deriving (Show)

data Term = Term Factor [MultiplyingOperation]
          deriving (Show)

data MultiplyingOperation = MultiplyingOperation MultiplyingOperator Factor
                          deriving (Show)

data Factor = Factor_Value Primary
            | Factor_Pow Primary Primary
            | Factor_Abs Primary
            | Factor_Not Primary
            deriving(Show)

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
--     > <function>_name ( actual_parameter_part )
-- * 'Primary_Name' covers other case of function_call
data Primary = Primary_Name Name
             | Primary_Literal Literal
             | Primary_Aggregate Aggregate
             | Primary_FunctionCall FunctionCall
             | Primary_QualifiedExpression QualifiedExpression
             | Primary_TypeConversion TypeConversion
             | Primary_Allocator Allocator
             | Primary_Expression Expression
             deriving(Show)

data TypeConversion = TypeConversion Name Expression
                    deriving (Show)

data RelationalOperator = Relation_Equals
                        | Relation_NotEquals
                        | Relation_LessThan
                        | Relation_LessThanOrEqual
                        | Relation_GreaterThan
                        | Relation_GreaterThanOrEqual
                        deriving(Show)

data AddingOperator = Add
                    | Minus
                    | Concat
                    deriving(Show)

data MultiplyingOperator = Multiply
                         | Divide
                         | Mod
                         | Rem
                         deriving(Show)

data Literal = Literal_Numeric NumericLiteral
             | Literal_Enumeration EnumerationLiteral
             | Literal_String StringLiteral
             | Literal_BitStr BitStrLiteral
             | Literal_Null
             deriving(Show)

data NumericLiteral = NumericLiteral_Abstract AbstractLiteral
                    | NumericLiteral_Physical PhysicalLiteral
                    deriving(Show)

data AbstractLiteral = UniversalInteger Int64
                     | UniversalReal Double
                     deriving(Show)

type StringLiteral = String

data BitStrLiteral = BitStrLiteral TokenTypes.LiteralBase ByteString
                   deriving (Show)

data ElementAssociation = ElementAssociation (Maybe [Choice]) Expression
                        deriving (Show)

type Aggregate = [ElementAssociation]

data Choice = Choice_Expression SimpleExpression
            | Choice_DiscreteRange DiscreteRange
            | Choice_ElementName String
            | Choice_Others
            deriving(Show)

-- |Function Call
-- @
--    function_call ::=
--       <function>_name [ ( actual_parameter_part ) ]
-- @
-- = NOTE
-- Only matches full case, other case is caught by 'Name'
-- > <function>_name ( actual_parameter_part )
data FunctionCall = FunctionCall Name ActualParameterPart
                  deriving (Show)

data QualifiedExpression = QualifiedExpression_Expression Name Expression
                         | QualifiedExpression_Aggregate Name Aggregate
                         deriving(Show)

data Allocator = Allocator_Subtype SubtypeIndication
               | Allocator_Expression QualifiedExpression
               deriving(Show)

data SequentialStatement = SequentialStatement_Wait WaitStatement
                         | SequentialStatement_Assertion AssertionStatement
                         | SequentialStatement_SignalAssignment SignalAssignmentStatement
                         | SequentialStatement_VariableAssignment VariableAssignmentStatement
                         | SequentialStatement_ProcedureCall ProcedureCallStatement
                         | SequentialStatement_If IfStatement
                         | SequentialStatement_Case CaseStatement
                         | SequentialStatement_Loop LoopStatement
                         | SequentialStatement_Next NextStatement
                         | SequentialStatement_Exit ExitStatement
                         | SequentialStatement_Return ReturnStatement
                         | SequentialStatement_Null
                         deriving(Show)

data WaitStatement = WaitStatement (Maybe [Name]) (Maybe Expression) (Maybe Expression)
                   deriving (Show)

data AssertionStatement = AssertionStatement Expression (Maybe Expression) (Maybe Expression)
                        deriving (Show)

data SignalAssignmentStatement = SignalAssignmentStatement Target SignalAssignmentType Waveform
                               deriving (Show)

data VariableAssignmentStatement = VariableAssignmentStatement Target Expression
                                 deriving (Show)

type Waveform = [WaveformElement]

data Target = Target_Name Name
            | Target_Aggregate Aggregate
            deriving(Show)

data SignalAssignmentType = SignalAssignmentNormal
                          | SignalAssignmentTransport
                          deriving(Show)

data WaveformElement = Waveform_Expression Expression (Maybe Expression)
                     | Waveform_Null (Maybe Expression)
                     deriving(Show)

data ProcedureCallStatement = ProcedureCallStatement Name (Maybe ActualParameterPart)
                            deriving (Show)

type ActualParameterPart = AssociationList

data IfStatement = IfStatement Expression [SequentialStatement] [ElsifStatement] (Maybe [SequentialStatement])
                 deriving (Show)

data ElsifStatement = ElsifStatement Expression [SequentialStatement]
                    deriving (Show)

data CaseStatement = CaseStatement Expression [CaseStatementAlternative]
                   deriving (Show)

data CaseStatementAlternative = CaseStatementAlternative [Choice] [SequentialStatement]
                              deriving (Show)

data LoopStatement = LoopStatement (Maybe (String,String)) (Maybe IterationScheme) [SequentialStatement]
                   deriving (Show)

data IterationScheme = IterationScheme_While Expression
                     | IterationScheme_For String DiscreteRange
                     deriving(Show)

data NextStatement = NextStatement (Maybe String) (Maybe Expression)
                   deriving (Show)

data ExitStatement = ExitStatement (Maybe String) (Maybe Expression)
                   deriving (Show)

newtype ReturnStatement = ReturnStatement (Maybe Expression)
                        deriving (Show)

data ConcurrentStatement = Concurrent_BlockStatement BlockStatement
                         | Concurrent_ProcessStatement ProcessStatement
                         | Concurrent_ProcedureCall ConcurrentProcedureCall
                         | Concurrent_AssertionStatement ConcurrentAssertionStatement
                         | Concurrent_SignalAssignmentStatement ConcurrentSignalAssignmentStatement
                         | Concurrent_ComponentInstantiationStatement ComponentInstantiationStatement
                         | Concurrent_GenerateStatement GenerateStatement
                         deriving(Show)

data BlockStatement = BlockStatement String (Maybe Expression) BlockHeader BlockDeclarativePart BlockStatementPart (Maybe String)
                    deriving (Show)

data BlockHeader = BlockHeader (Maybe BlockHeader_Generic) (Maybe BlockHeader_Port)
                 deriving (Show)

data BlockHeader_Generic = BlockHeader_Generic GenericClause (Maybe GenericMapAspect)
                         deriving (Show)

type GenericMapAspect = AssociationList

data BlockHeader_Port = BlockHeader_Port PortClause (Maybe PortMapAspect)
                      deriving (Show)

type PortMapAspect = AssociationList

type BlockDeclarativePart = [BlockDeclarativeItem]

type BlockStatementPart = [ConcurrentStatement]

data ProcessStatement = ProcessStatement (Maybe (String,String)) (Maybe SensitivityList) ProcessDeclarativePart ProcessStatementPart
                      deriving (Show)

type ProcessDeclarativePart = [ProcessDeclarativeItem]

type ProcessStatementPart = [SequentialStatement]

type SensitivityList = [Name]

data ComponentInstantiationStatement = ComponentInstantiationStatement String Name (Maybe AssociationList) (Maybe AssociationList)
                                     deriving (Show)

data GenerateStatement = GenerateStatement String GenerationScheme [ConcurrentStatement] (Maybe String)
                       deriving (Show)

data ProcessDeclarativeItem = ProcessDeclarative_SubprogramDeclaration     SubprogramDeclaration
                            | ProcessDeclarative_SubprogramBody            SubprogramBody
                            | ProcessDeclarative_TypeDeclaration           TypeDeclaration
                            | ProcessDeclarative_SubtypeDeclaration        SubtypeDeclaration
                            | ProcessDeclarative_ConstantDeclaration       ConstantDeclaration
                            | ProcessDeclarative_VariableDeclaration       VariableDeclaration
                            | ProcessDeclarative_FileDeclaration           FileDeclaration
                            | ProcessDeclarative_AliasDeclaration          AliasDeclaration
                            | ProcessDeclarative_AttributeDeclaration      AttributeDeclaration
                            | ProcessDeclarative_AttributeSpecification    AttributeSpecification
                            | ProcessDeclarative_UseClause                 UseClause
                            deriving(Show)

data ConcurrentProcedureCall = ConcurrentProcedureCall (Maybe String) ProcedureCallStatement
                             deriving (Show)

data ConcurrentAssertionStatement = ConcurrentAssertionStatement (Maybe String) AssertionStatement
                                  deriving (Show)

data ConcurrentSignalAssignmentStatement = ConditionalSignalAssignment (Maybe String) Target SignalAssignmentOptions ConditionalWaveforms
                                         | SelectedSignalAssignment (Maybe String) Expression Target SignalAssignmentOptions [SelectedWaveformPair]
                                         deriving(Show)

data SignalAssignmentOptions = SignalAssignmentOptions SignalAssignment_Guarded SignalAssignment_Transport
                             deriving (Show)

data SignalAssignment_Guarded = SignalAssignment_Guarded | SignalAssignment_NonGuarded
                              deriving (Show)

data SignalAssignment_Transport = SignalAssignment_Transport | SignalAssignment_NonTransport
                                deriving (Show)

data ConditionalWaveforms = ConditionalWaveforms [ConditionalWaveformPair] Waveform
                          deriving (Show)

data ConditionalWaveformPair = ConditionalWaveformPair Waveform Expression
                             deriving (Show)

data SelectedWaveformPair = SelectedWaveformPair Waveform [Choice]
                          deriving (Show)

data GenerationScheme = GenerationScheme_For String DiscreteRange
                      | GenerationScheme_If Expression
                      deriving(Show)

newtype UseClause = UseClause [SelectedName]
                  deriving (Show)

newtype DesignFile = DesignFile [DesignUnit]
                   deriving (Show)

data DesignUnit = DesignUnit ContextClause LibraryUnit
                deriving (Show)

data LibraryUnit = Library_PrimaryUnit PrimaryUnit
                 | Library_SecondaryUnit SecondaryUnit
                 deriving(Show)

data PrimaryUnit = PrimaryUnit_EntityDeclaration EntityDeclaration
                 | PrimaryUnit_ConfigurationDeclaration ConfigurationDeclaration
                 | PrimaryUnit_PackageDeclaration PackageDeclaration
                 deriving(Show)

data SecondaryUnit = Secondary_ArchitectureBody ArchitectureBody
                   | Secondary_PackageBody PackageBody
                   deriving(Show)

type LibraryClause = [String]

type ContextClause = [ContextItem]

data ContextItem = Context_LibraryClause LibraryClause
                 | Context_UseClause UseClause
                 deriving(Show)
