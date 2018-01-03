module Parser.Happy.Types where

newtype EntityDeclaration = EntityDeclaration String EntityHeader EntityDeclarativePart EntityStatementPart (Maybe String)

newtype EntityHeader = EntityHeader (Maybe GenericClause) (Maybe PortClause)

newtype EntityDeclarativePart = [EntityDeclarativeItem]

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

newtype EntityStatementPart = [EntityStatement]

data EntityStatement = EntityStatement_ConcurrentAssertionStatement  ConcurrentAssertionStatement
                     | EntityStatement_ConcurrentProcedureCall       ConcurrentProcedureCall
                     | EntityStatement_ProcessStatement              ProcessStatement

newtype ArchitectureBody = ArchitectureBody String String ArchitectureDeclarativePart ArchitectureStatementPart (Maybe String)

newtype ArchitectureDeclarativePart = [BlockDeclarativeItem]

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

newtype ArchitectureStatementPart = [ConcurrentStatement]

newtype ConfigurationDeclaration = ConfigurationDeclaration String String ConfigurationDeclarativePart BlockConfiguration (Maybe String)

newtype ConfigurationDeclarativePart = [ConfigurationDeclarativeItem]

data ConfigurationDeclarativeItem = ConfigurationDeclarativeItem_UseClause                UseClause
                                  | ConfigurationDeclarativeItem_AttributeSpecification   AttributeSpecification

newtype BlockConfiguration BlockSpecification [UseClause] [ConfigurationItem]

data BlockSpecification = BlockSpecificationIdentifier String -- architecture_name or block_statement_label or generate_statement_label
                        | BlockSpecificationGenerate String IndexSpecification -- generate_statement_label ( index_specification )

data IndexSpecification = IndexSpecification_DiscreteRange  DiscreteRange
                        | IndexSpecification_Expression     Expression -- static_expression

data ConfigurationItem = ConfigurationItem_BlockConfiguration BlockConfiguration
                       | ConfigurationItem_ComponentConfiguration ComponentConfiguration

newtype ComponentConfiguration = ComponentConfiguration ComponentSpecification (Maybe BindingIndication) (Maybe BlockConfiguration)

newtype SubprogramDeclaration = SubprogramDeclaration SubprogramSpecification

data SubprogramSpecification = ProcedureDeclaration Designator (Maybe FormalParameterList)
                             | FunctionDeclaration Designator (Maybe FormalParameterList) String

data Designator = Designator_Identifier String
                | Designator_Operator OperatorType
                | Designator_Keyword ReservedWord

newtype SubprogramBody = SubprogramBody SubprogramSpecification SubprogramDeclarativePart SubprogramStatementPart (Maybe Designator)

newtype SubprogramDeclarativePart = [SubprogramDeclarativeItem]

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

newtype SubprogramStatementPart = [SequentialStatement]

newtype PackageDeclaration = PackageDeclaration String PackageDeclarativePart (Maybe String)

newtype PackageDeclarativePart = [PackageDeclarativeItem]

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

newtype PackageBody = PackageBody String PackageBodyDeclarativePart (Maybe String)

newtype PackageBodyDeclarativePart = [PackageBodyDeclarativeItem]

data PackageBodyDeclarativeItem = PackageBodyDeclarativeItem_SubprogramDeclaration SubprogramDeclaration
                                | PackageBodyDeclarativeItem_SubprogramBody        SubprogramBody
                                | PackageBodyDeclarativeItem_TypeDeclaration       TypeDeclaration
                                | PackageBodyDeclarativeItem_SubtypeDeclaration    SubtypeDeclaration
                                | PackageBodyDeclarativeItem_ConstantDeclaration   ConstantDeclaration
                                | PackageBodyDeclarativeItem_FileDeclaration       FileDeclaration
                                | PackageBodyDeclarativeItem_AliasDeclaration      AliasDeclaration
                                | PackageBodyDeclarativeItem_UseClause             UseClause

data ScalarTypeDefinition = EnumerationTypeDefinition [EnumerationLiteral]
                          | IntegerTypeDefinition Range
                          | FloatingTypeDefinition Range
                          | PhysicalTypeDefinition Range String [SecondaryUnitDeclaration]

data EnumerationLiteral = EnumerationLiteral_Identifier String
                        | EnumerationLiteral_Char Char

data Range = RangeAttributeName AttributeName 
           | RangeExpression SimpleExpression Direction SimpleExpression

data Direction = To
               | Downto

newtype SecondaryUnitDeclaration = SecondaryUnitDeclaration String PhysicalLiteral

newtype PhysicalLiteral = PhysicalLiteral (Maybe AbstractLiteral) String

data CompositeTypeDefinition = Composite_ArrayTypeDefinition ArrayTypeDefinition
                             | RecordTypeDefinition [ElementDeclaration]

data ArrayTypeDefinition = UnconstrainedArrayTypeDefinition [String] SubtypeIndication
                         | ConstrainedArrayTypeDefinition IndexConstraint SubtypeIndication

newtype IndexConstraint = [DiscreteRange]

data DiscreteRange = DiscreteRange_SubtypeIndication SubtypeIndication
                   | DescreteRange_Range Range

newtype ElementDeclaration = ElementDeclaration [String] SubtypeIndication

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

data TypeDeclaration = FullTypeDeclaration String TypeDefinition
                     | IncompleteTypeDefinition String

data TypeDefinition = TypeDefinition_Scalar ScalarTypeDefinition
                    | TypeDefinition_Composite CompositeTypeDefinition
                    | TypeDefinition_Access SubtypeIndication
                    | TypeDefinition_File String

newtype SubtypeDeclaration = SubtypeDeclaration String SubtypeIndication

newtype SubtypeIndication = SubtypeIndication (Maybe String) String (Maybe Constraint)

data Constraint = Constraint_Range Range
                | Constraint_Index IndexConstraint

data ObjectDeclaration = ObjectDeclaration_Constant ConstantDeclaration
                       | ObjectDeclaration_Signal SignalDeclaration
                       | ObjectDeclaration_Variable VariableDeclaration

newtype ConstantDeclaration = ConstantDeclaration [String] SubtypeIndication (Maybe Expression)

newtype SignalDeclaration = SignalDeclaration [String] SubtypeIndication (Maybe SignalKind) (Maybe Expression)

data SignalKind = Register
                | Bus

newtype VariableDeclaration = VariableDeclaration [String] SubtypeIndication (Maybe Expression)

newtype FileDeclaration = FileDeclaration String SubtypeIndication (Maybe Mode) Expression

newtype InterfaceDeclaration = InterfaceDeclaration (Maybe InterfaceType) [String] (Maybe Mode) SubtypeIndication (Maybe Expression)

data InterfaceType = Constant
                   | Signal
                   | GuardedSignal
                   | Variable

data Mode = In
          | Out
          | Inout
          | Buffer
          | Linkage

newtype InterfaceList = [InterfaceDeclaration]

newtype AssociationList = [AssociationElement]

newtype AssociationElement = AssociationElement (Maybe FormatPart) ActualPart

data FormalPart = FormalPart_Designator String
                | FormalPart_Function String String

data ActualPart = ActualPart_Expression Expression
                | ActualPart_Name String
                | ActualPart_Open
                | ActualPart_Function String String

newtype AliasDeclaration = AliasDeclaration String SubtypeIndication Name

newtype AttributeDeclaration = AttributeDeclaration String String

newtype ComponentDeclaration = ComponentDeclaration String (Maybe GenericClause) (Maybe PortClause)

newtype AttributeSpecification = AttributeSpecification String AttributeSpecificationEntityNameList EntityClass Expression

data AttributeSpecificationEntityNameList = AttributeSpecificationEntityName_List [EntityDesignator]
                                          | AttributeSpecificationEntityName_Others
                                          | AttributeSpecificationEntityName_All

data EntityDesignator = EntityDesignator_Name String
                      | EntityDesignator_Operator OperatorType

newtype ConfigurationSpecification = ConfigurationSpecification InstantiationList Name BindingIndication

data InstantiationList = InstantiationList_Label [Label]
                       | InstantiationList_Others
                       | InstantiationList_All

newtype BindingIndication = BindingIndication EntityAspect (Maybe AssociationList) (Maybe AssociationList)

data EntityAspect = EntityAspect_Entity Name (Maybe String)
                  | EntityAspect_Configuration Name
                  | EntityAspect_Open

newtype DisconnectionSpecification = DisconnectionSpecification GuardedSignalList String Expression

data GuardedSignalList = GuardedSignal_List [Name]
                       | GuardedSignal_Others
                       | GuardedSignal_All

data Name = Name_Simple String
          | Name_Operator OperatorType
          | Name_Selected SelectedName
          | Name_Indexed Prefix [Expression]
          | Name_Slice Prefix DiscreteRange
          | Name_Attribute AttributeName

newtype SelectedName = SelectedName Prefix Suffix

data Prefix = Prefix_Name Name
            | Prefix_Function FunctionCall

data Suffix = Suffix_Name String
            | Suffix_Char Char
            | Suffix_Operator OperatorType
            | Suffix_All

newtype AttributeName = AttributeName Prefix String (Maybe Expression)

data Expression = Expression_And Relation [Relation]
                | Expression_Or Relation [Relation]
                | Expression_Xor Relation [Relation]
                | Expresion_Nand Relation (Maybe Relation)
                | Expression_Nor Relation (Maybe Relation)

newtype Relation = Relation SimpleExpression (Maybe (RelationalOperator, SimpleExpression))

newtype SimpleExpression = SimpleExpression (Maybe Sign) Term [AddingOperation]

data Sign = Positive
          | Negative

newtype Term = Term Factor [MultiplyingOperation]

newtype MultiplyingOperation = MultiplyingOperation MultiplyingOperator Factor

newtype AddingOperation = AddingOperation AddingOperator Factor

data Factor = Factor_Value Primary
            | Factor_Pow Primary Primary
            | Factor_Abs Primary
            | Factor_Not Primary

data Primary = Primary_Name String
             | Primary_Literal (Maybe LiteralType)
             | Primary_Aggregate Aggregate
             | Primary_FunctionCall FunctionCall
             | Primary_QualifiedExpression QualifiedExpression
             | Primary_TypeConversion String Expression
             | Primary_Allocator Allocator
             | Primary_Expression Expression

data RelationalOperator = Relation_Equals
                        | Relation_NotEquals
                        | Relation_LessThan
                        | Relation_LessThanOrEqual
                        | Relation_GreaterThan
                        | Relation_GreaterThanOrEqual

data AddingOperator = Add
                    | Minus
                    | Concat

data MultiplyingOperator = Multiply
                         | Divide
                         | Mod
                         | Rem

newtype ElementAssociation = ElementAssociation (Maybe [Choice]) Expression

newtype Aggregate = [ElementAssociation]

newtype Choice = Choice_Expression SimpleExpression
               | Choice_DiscreteRange DiscreteRange
               | Choice_ElementName String
               | Choice_Others

newtype FunctionCall = FunctionCall String (Maybe AssociationList)

data QualifiedExpression = QualifiedExpression_Expression String Expression
                         | QualifiedExpression_Aggregate Aggregate

data Allocator = Allocator_Subtype SubtypeIndication
               | Allocator_Expression QualifiedExpression

data SequentialStatement = Sequential_WaitStatement (Maybe [Name]) (Maybe Expression) (Maybe Expression)
                         | Sequential_AssertionStatement Expression (Maybe Expression) (Maybe Expression)
                         | Sequential_SignalAssignmentStatement AssertionStatement
                         | Sequential_VariableAssignmentStatement Target Expression
                         | Sequential_ProcedureCallStatement ProcedureCallStatement
                         | Sequential_IfStatement Expression [SequentialStatement] [ElsifStatement] (Maybe [SequentialStatement])
                         | Sequential_CaseStatement Expression [CaseStatementAlternative]
                         | Sequential_LoopStatement (Maybe String) (Maybe IterationScheme) [SequentialStatement]
                         | Sequential_NextStatement (Maybe String) (Maybe Expression)
                         | Sequential_ExitStatement (Maybe String) (Maybe Expression)
                         | Sequential_ReturnStatement (Maybe Expression)
                         | Sequential_NullStatement

newtype AssertionStatement = AssertionStatement Target SignalAssignmentType [WaveformElement]

data Target = Target_Name String
            | Target_Aggregate Aggregate

data SignalAssignmentType = SignalAssignmentNormal | SignalAssignmentTarget

newtype WaveformElement = Waveform_Expression Expression (Maybe Expression)
                        | Waveform_Null (Maybe Expression)

newtype ProcedureCallStatement = ProcedureCallStatement String (Maybe ActualParameterPart)

newtype ElsifStatement = ElsifStatement Expression [SequentialStatement]

newtype CaseStatementAlternative = CaseStatementAlternative [Choice] [SequentialStatement]

data IterationScheme = IterationScheme_While Expression
                     | IterationScheme_For String DiscreteRange

data ConcurrentStatement = Concurrent_BlockStatement String (Maybe Expression) BlockHeader [BlockDeclarativeItem] [ConcurrentStatement]
                         | Concurrent_ProcessStatement (Maybe String) (Maybe SensitivityList) [ProcessDeclarativeItem] [SequentialStatement]
                         | Concurrent_ProcedureCall ConcurrentProcedureCall
                         | Concurrent_AssertionStatement ConcurrentAssertionStatement
                         | Concurrent_SignalAssignmentStatement ConcurrentSignalAssignmentStatement
                         | Concurrent_ComponentInstantiationStatement String Name (Maybe AssociationList) (Maybe AssociationList)
                         | Concurrent_GenerateStatement String GenerationScheme [ConcurrentStatement]

newtype BlockHeader = BlockHeader (Maybe BlockHeader_Generic) (Maybe BlockHeader_Port)

newtype BlockHeader_Generic = BlockHeader_Generic GenericClause (Maybe AssociationList)

newtype BlockHeader_Port = BlockHeader_Port PortClause (Maybe AssociationList)

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

newtype ConcurrentProcedureCall = ConcurrentProcedureCall (Maybe String) ProcedureCallStatement

newtype ConcurrentAssertionStatement = (Maybe String) AssertionStatement

data ConcurrentSignalAssignmentStatement = ConditionalSignalAssignment (Maybe String) Target SignalAssignment_Options ConditionalWaveforms
                                         | SelectedSignalAssignment (Maybe String) Expression Target SignalAssignment_Options [WaveformExpressionPair]

newtype SignalAssignment_Options = (Maybe SignalAssignment_Guarded) (Maybe SignalAssignment_Transport)

data SignalAssignment_Guarded = SignalAssignment_Guarded | SignalAssignment_NonGuarded

data SignalAssignment_Transport = SignalAssignment_Transport | SignalAssignment_NonTransport

newtype ConditionalWaveforms = ConditionalWaveforms [WaveformExpressionPair] Waveform

newtype WaveformExpressionPair = WaveformExpressionPair Waveform Expression

data GenerationScheme = GenerationScheme_For ParameterSpecification
                      | GenerationScheme_If Expression

newtype GenericClause = InterfaceList

newtype PortClause = InterfaceList

newtype UseClause = [SelectedName]

newtype DesignFile = [DesignUnit]

newtype DesignUnit = DesignUnit ContextClause LibraryUnit

data LibraryUnit = Library_PrimaryUnit PrimaryUnit
                 | Library_SecondaryUnit SecondaryUnit

data PrimaryUnit = Primary_EntityDeclaration EntityDeclaration
                 | Primary_ConfigurationDeclaration ConfigurationDeclaration
                 | Primary_PackageDeclaration PackageDeclaration

data SecondaryUnit = Primary_ArchitectureBody ArchitectureBody
                   | Primary_PackageBody PackageBody

newtype LibraryClause = [String]

newtype ContextClause = [ContextItem]

data ContextItem = Context_LibraryClause LibraryClause
                 | Context_UseClause UseClause
