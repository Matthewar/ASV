module Parser.Happy.Types where

import Data.Int (Int64)
import Data.ByteString.Char8 (ByteString)

import qualified Parser.TokenTypes as TokenTypes

data EntityDeclaration = EntityDeclaration String EntityHeader EntityDeclarativePart EntityStatementPart (Maybe String)

data EntityHeader = EntityHeader (Maybe GenericClause) (Maybe PortClause)

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

type EntityStatementPart = [EntityStatement]

data EntityStatement = EntityStatement_ConcurrentAssertionStatement  ConcurrentAssertionStatement
                     | EntityStatement_ConcurrentProcedureCall       ConcurrentProcedureCall
                     | EntityStatement_ProcessStatement              ProcessStatement

data ArchitectureBody = ArchitectureBody String String ArchitectureDeclarativePart ArchitectureStatementPart (Maybe String)

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

type ArchitectureStatementPart = [ConcurrentStatement]

data ConfigurationDeclaration = ConfigurationDeclaration String String ConfigurationDeclarativePart BlockConfiguration (Maybe String)

type ConfigurationDeclarativePart = [ConfigurationDeclarativeItem]

data ConfigurationDeclarativeItem = ConfigurationDeclarativeItem_UseClause                UseClause
                                  | ConfigurationDeclarativeItem_AttributeSpecification   AttributeSpecification

data BlockConfiguration = BlockConfiguration BlockSpecification [UseClause] [ConfigurationItem]

data BlockSpecification = BlockSpecificationIdentifier String -- architecture_name or block_statement_label or generate_statement_label
                        | BlockSpecificationGenerate String IndexSpecification -- generate_statement_label ( index_specification )

data IndexSpecification = IndexSpecification_DiscreteRange  DiscreteRange
                        | IndexSpecification_Expression     Expression -- static_expression

data ConfigurationItem = ConfigurationItem_BlockConfiguration BlockConfiguration
                       | ConfigurationItem_ComponentConfiguration ComponentConfiguration

data ComponentConfiguration = ComponentConfiguration ComponentSpecification (Maybe BindingIndication) (Maybe BlockConfiguration)

data ComponentSpecification = ComponentSpecification InstantiationList Name

newtype SubprogramDeclaration = SubprogramDeclaration SubprogramSpecification

data SubprogramSpecification = ProcedureDeclaration Designator (Maybe FormalParameterList)
                             | FunctionDeclaration Designator (Maybe FormalParameterList) String

type FormalParameterList = InterfaceList

data Designator = Designator_Identifier String
                | Designator_Operator TokenTypes.OperatorType
                | Designator_Keyword TokenTypes.ReservedWord

data SubprogramBody = SubprogramBody SubprogramSpecification SubprogramDeclarativePart SubprogramStatementPart (Maybe Designator)

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

type SubprogramStatementPart = [SequentialStatement]

data PackageDeclaration = PackageDeclaration String PackageDeclarativePart (Maybe String)

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

data PackageBody = PackageBody String PackageBodyDeclarativePart (Maybe String)

type PackageBodyDeclarativePart = [PackageBodyDeclarativeItem]

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

data SecondaryUnitDeclaration = SecondaryUnitDeclaration String PhysicalLiteral

data PhysicalLiteral = PhysicalLiteral (Maybe AbstractLiteral) String

data CompositeTypeDefinition = Composite_ArrayTypeDefinition ArrayTypeDefinition
                             | RecordTypeDefinition [ElementDeclaration]

data ArrayTypeDefinition = UnconstrainedArrayTypeDefinition [String] SubtypeIndication
                         | ConstrainedArrayTypeDefinition IndexConstraint SubtypeIndication

type IndexConstraint = [DiscreteRange]

data DiscreteRange = DiscreteRange_SubtypeIndication SubtypeIndication
                   | DescreteRange_Range Range

data ElementDeclaration = ElementDeclaration [String] SubtypeIndication

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

data SubtypeDeclaration = SubtypeDeclaration String SubtypeIndication

data SubtypeIndication = SubtypeIndication (Maybe String) String (Maybe Constraint)

data Constraint = Constraint_Range Range
                | Constraint_Index IndexConstraint

data ObjectDeclaration = ObjectDeclaration_Constant ConstantDeclaration
                       | ObjectDeclaration_Signal SignalDeclaration
                       | ObjectDeclaration_Variable VariableDeclaration

data ConstantDeclaration = ConstantDeclaration [String] SubtypeIndication (Maybe Expression)

data SignalDeclaration = SignalDeclaration [String] SubtypeIndication (Maybe SignalKind) (Maybe Expression)

data SignalKind = Register
                | Bus

data VariableDeclaration = VariableDeclaration [String] SubtypeIndication (Maybe Expression)

data FileDeclaration = FileDeclaration String SubtypeIndication (Maybe Mode) Expression

data InterfaceDeclaration = InterfaceDeclaration (Maybe InterfaceType) [String] (Maybe Mode) SubtypeIndication (Maybe Expression)

data InterfaceType = Constant
                   | Signal
                   | GuardedSignal
                   | Variable

data Mode = In
          | Out
          | Inout
          | Buffer
          | Linkage

type InterfaceList = [InterfaceDeclaration]

type AssociationList = [AssociationElement]

data AssociationElement = AssociationElement (Maybe FormalPart) ActualPart

data FormalPart = FormalPart_Designator Name
                | FormalPart_Function Name Name

data ActualPart = ActualPart_Designator ActualDesignator
                | ActualPart_Function Name ActualDesignator

data ActualDesignator = ActualDesignator_Expression Expression
                      | ActualDesignator_Name Name
                      | ActualDesignator_Open

data AliasDeclaration = AliasDeclaration String SubtypeIndication Name

data AttributeDeclaration = AttributeDeclaration String String

data ComponentDeclaration = ComponentDeclaration String (Maybe GenericClause) (Maybe PortClause)

data AttributeSpecification = AttributeSpecification String AttributeSpecificationEntityNameList EntityClass Expression

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

data AttributeSpecificationEntityNameList = AttributeSpecificationEntityName_List [EntityDesignator]
                                          | AttributeSpecificationEntityName_Others
                                          | AttributeSpecificationEntityName_All

data EntityDesignator = EntityDesignator_Name String
                      | EntityDesignator_Operator TokenTypes.OperatorType

data ConfigurationSpecification = ConfigurationSpecification ComponentSpecification BindingIndication

data InstantiationList = InstantiationList_Label [String]
                       | InstantiationList_Others
                       | InstantiationList_All

data BindingIndication = BindingIndication EntityAspect (Maybe AssociationList) (Maybe AssociationList)

data EntityAspect = EntityAspect_Entity Name (Maybe String)
                  | EntityAspect_Configuration Name
                  | EntityAspect_Open

data DisconnectionSpecification = DisconnectionSpecification GuardedSignalList String Expression

data GuardedSignalList = GuardedSignal_List [Name]
                       | GuardedSignal_Others
                       | GuardedSignal_All

data Name = Name_Simple SimpleName
          | Name_Operator TokenTypes.OperatorType
          | Name_Selected SelectedName
          | Name_Indexed IndexedName
          | Name_Slice SliceName
          | Name_Attribute AttributeName

type SimpleName = String

data SelectedName = SelectedName Prefix Suffix

data IndexedName = IndexedName Prefix [Expression]

data SliceName = SliceName Prefix DiscreteRange

data Prefix = Prefix_Name Name
            | Prefix_Function FunctionCall

data Suffix = Suffix_Name String
            | Suffix_Char Char
            | Suffix_Operator TokenTypes.OperatorType
            | Suffix_All

data AttributeName = AttributeName Prefix String (Maybe Expression)

data Expression = Expression_And AndExpression
                | Expression_Or OrExpression
                | Expression_Xor XorExpression
                | Expresion_Nand NandExpression
                | Expression_Nor NorExpression
                | Expression_Relation Relation

data AndExpression = AndExpression Relation AndExpression
                   | AndRelation Relation Relation

data OrExpression = OrExpression Relation OrExpression
                   | OrRelation Relation Relation

data XorExpression = XorExpression Relation XorExpression
                   | XorRelation Relation Relation

data NandExpression = NandExpression Relation Relation

data NorExpression = NorExpression Relation Relation

data Relation = Relation_Compare SimpleExpression RelationalOperator SimpleExpression
              | Relation_Term SimpleExpression

data SimpleExpression = SimpleExpression Sign Term [AddingOperation]

data Sign = Positive
          | Negative

data AddingOperation = AddingOperation AddingOperator Term

data Term = Term Factor [MultiplyingOperation]

data MultiplyingOperation = MultiplyingOperation MultiplyingOperator Factor

data Factor = Factor_Value Primary
            | Factor_Pow Primary Primary
            | Factor_Abs Primary
            | Factor_Not Primary

data Primary = Primary_Name Name
             | Primary_Literal Literal
             | Primary_Aggregate Aggregate
             | Primary_FunctionCall FunctionCall
             | Primary_QualifiedExpression QualifiedExpression
             | Primary_TypeConversion TypeConversion
             | Primary_Allocator Allocator
             | Primary_Expression Expression

data TypeConversion = TypeConversion String Expression

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

data Literal = Literal_Numeric NumericLiteral
             | Literal_Enumeration EnumerationLiteral
             | Literal_String StringLiteral
             | Literal_BitStr BitStrLiteral
             | Literal_Null

data NumericLiteral = NumericLiteral_Abstract AbstractLiteral
                    | NumericLiteral_Physical PhysicalLiteral

data AbstractLiteral = UniversalInteger Int64
                     | UniversalReal Double

type StringLiteral = String

data BitStrLiteral = BitStrLiteral TokenTypes.LiteralBase ByteString

data ElementAssociation = ElementAssociation (Maybe [Choice]) Expression

type Aggregate = [ElementAssociation]

data Choice = Choice_Expression SimpleExpression
            | Choice_DiscreteRange DiscreteRange
            | Choice_ElementName String
            | Choice_Others

data FunctionCall = FunctionCall String (Maybe ActualParameterPart)

data QualifiedExpression = QualifiedExpression_Expression String Expression
                         | QualifiedExpression_Aggregate Aggregate

data Allocator = Allocator_Subtype SubtypeIndication
               | Allocator_Expression QualifiedExpression

data SequentialStatement = Sequential_WaitStatement WaitStatement
                         | Sequential_AssertionStatement AssertionStatement
                         | Sequential_SignalAssignmentStatement SignalAssignmentStatement
                         | Sequential_VariableAssignmentStatement VariableAssignmentStatement
                         | Sequential_ProcedureCallStatement ProcedureCallStatement
                         | Sequential_IfStatement IfStatement
                         | Sequential_CaseStatement CaseStatement
                         | Sequential_LoopStatement LoopStatement
                         | Sequential_NextStatement NextStatement
                         | Sequential_ExitStatement ExitStatement
                         | Sequential_ReturnStatement ReturnStatement
                         | Sequential_NullStatement

data WaitStatement = WaitStatement (Maybe [Name]) (Maybe Expression) (Maybe Expression)

data AssertionStatement = AssertionStatement Expression (Maybe Expression) (Maybe Expression)

data SignalAssignmentStatement = SignalAssignmentStatement Target SignalAssignmentType Waveform

data VariableAssignmentStatement = VariableAssignmentStatement Target Expression

type Waveform = [WaveformElement]

data Target = Target_Name Name
            | Target_Aggregate Aggregate

data SignalAssignmentType = SignalAssignmentNormal
                          | SignalAssignmentTransport

data WaveformElement = Waveform_Expression Expression (Maybe Expression)
                     | Waveform_Null (Maybe Expression)

data ProcedureCallStatement = ProcedureCallStatement Name (Maybe ActualParameterPart)

type ActualParameterPart = AssociationList

data IfStatement = IfStatement Expression [SequentialStatement] [ElsifStatement] (Maybe [SequentialStatement])

data ElsifStatement = ElsifStatement Expression [SequentialStatement]

data CaseStatement = CaseStatement Expression [CaseStatementAlternative]

data CaseStatementAlternative = CaseStatementAlternative [Choice] [SequentialStatement]

data LoopStatement = LoopStatement (Maybe (String,String)) (Maybe IterationScheme) [SequentialStatement]

data IterationScheme = IterationScheme_While Expression
                     | IterationScheme_For String DiscreteRange

data NextStatement = NextStatement (Maybe String) (Maybe Expression)

data ExitStatement = ExitStatement (Maybe String) (Maybe Expression)

newtype ReturnStatement = ReturnStatement (Maybe Expression)

data ConcurrentStatement = Concurrent_BlockStatement BlockStatement
                         | Concurrent_ProcessStatement ProcessStatement
                         | Concurrent_ProcedureCall ConcurrentProcedureCall
                         | Concurrent_AssertionStatement ConcurrentAssertionStatement
                         | Concurrent_SignalAssignmentStatement ConcurrentSignalAssignmentStatement
                         | Concurrent_ComponentInstantiationStatement ComponentInstantiationStatement
                         | Concurrent_GenerateStatement GenerateStatement

data BlockStatement = BlockStatement String (Maybe Expression) BlockHeader BlockDeclarativePart BlockStatementPart (Maybe String)

data BlockHeader = BlockHeader (Maybe BlockHeader_Generic) (Maybe BlockHeader_Port)

data BlockHeader_Generic = BlockHeader_Generic GenericClause (Maybe GenericMapAspect)

type GenericMapAspect = AssociationList

data BlockHeader_Port = BlockHeader_Port PortClause (Maybe PortMapAspect)

type PortMapAspect = AssociationList

type BlockDeclarativePart = [BlockDeclarativeItem]

type BlockStatementPart = [ConcurrentStatement]

data ProcessStatement = ProcessStatement (Maybe (String,String)) (Maybe SensitivityList) ProcessDeclarativePart ProcessStatementPart

type ProcessDeclarativePart = [ProcessDeclarativeItem]

type ProcessStatementPart = [SequentialStatement]

type SensitivityList = [Name]

data ComponentInstantiationStatement = ComponentInstantiationStatement String Name (Maybe AssociationList) (Maybe AssociationList)

data GenerateStatement = GenerateStatement String GenerationScheme [ConcurrentStatement] (Maybe String)

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

data ConcurrentProcedureCall = ConcurrentProcedureCall (Maybe String) ProcedureCallStatement

data ConcurrentAssertionStatement = ConcurrentAssertionStatement (Maybe String) AssertionStatement

data ConcurrentSignalAssignmentStatement = ConditionalSignalAssignment (Maybe String) Target SignalAssignmentOptions ConditionalWaveforms
                                         | SelectedSignalAssignment (Maybe String) Expression Target SignalAssignmentOptions [SelectedWaveformPair]

data SignalAssignmentOptions = SignalAssignmentOptions SignalAssignment_Guarded SignalAssignment_Transport

data SignalAssignment_Guarded = SignalAssignment_Guarded | SignalAssignment_NonGuarded

data SignalAssignment_Transport = SignalAssignment_Transport | SignalAssignment_NonTransport

data ConditionalWaveforms = ConditionalWaveforms [ConditionalWaveformPair] Waveform

data ConditionalWaveformPair = ConditionalWaveformPair Waveform Expression

data SelectedWaveformPair = SelectedWaveformPair Waveform [Choice]

data GenerationScheme = GenerationScheme_For String DiscreteRange
                      | GenerationScheme_If Expression

type GenericClause = InterfaceList

type PortClause = InterfaceList

newtype UseClause = UseClause [SelectedName]

newtype DesignFile = DesignFile [DesignUnit]

data DesignUnit = DesignUnit ContextClause LibraryUnit

data LibraryUnit = Library_PrimaryUnit PrimaryUnit
                 | Library_SecondaryUnit SecondaryUnit

data PrimaryUnit = Primary_EntityDeclaration EntityDeclaration
                 | Primary_ConfigurationDeclaration ConfigurationDeclaration
                 | Primary_PackageDeclaration PackageDeclaration

data SecondaryUnit = Secondary_ArchitectureBody ArchitectureBody
                   | Secondary_PackageBody PackageBody

type LibraryClause = [String]

type ContextClause = [ContextItem]

data ContextItem = Context_LibraryClause LibraryClause
                 | Context_UseClause UseClause
