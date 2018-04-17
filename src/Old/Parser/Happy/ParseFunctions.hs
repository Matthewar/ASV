{-|
   Module      : Parser.Happy.ParseFunctions
   Description : Functions that Happy will use to generate nodes
|-}
module Parser.Happy.ParseFunctions where

import Data.Int (Int64)
import Data.ByteString.Char8 (ByteString)

import qualified Lexer.Types.Token as Tokens
import Lexer.Types.Token (Token, WrappedToken)
import Lexer.Types.PositionWrapper
import Parser.Happy.Types

------------------------------------------
-- Design Units
------------------------------------------

-- |New 'DesignUnit'
newDesignUnit :: ContextClause -> WrappedLibraryUnit -> WrappedDesignUnit
newDesignUnit contexts library = PosnWrapper { getPos = getPos $ last contexts, unPos = DesignUnit contexts library }

-- |New 'LibraryUnit'
newLibraryUnit :: WrappedToken -> [WrappedSimpleName] -> WrappedLibraryClause
newLibraryUnit token library = PosnWrapper { getPos = getPos token, unPos = LibraryClause library }

-- |New use clause
newUseClause :: WrappedToken -> [WrappedSelectedName] -> WrappedUseClause
newUseClause token list = PosnWrapper { getPos = getPos token, unPos = UseClause list }

------------------------------------------
-- Design Entities and Configurations
------------------------------------------

-- |New 'EntityDeclaration'
newEntityDeclaration :: WrappedToken -> WrappedSimpleName -> EntityHeader -> EntityDeclarativePart -> (Maybe EntityStatementPart) -> (Maybe WrappedSimpleName) -> WrappedEntityDeclaration
newEntityDeclaration token name1 header declarative statements name2 =
   PosnWrapper { getPos = getPos token, unPos = EntityDeclaration name1 header declarative statements name2 }

-- |New 'GenericClause'
newGenericClause :: WrappedToken -> InterfaceList -> WrappedGenericClause
newGenericClause token list = PosnWrapper { getPos = getPos token, unPos = list }

-- |New 'PortClause'
newPortClause :: WrappedToken -> InterfaceList -> WrappedPortClause
newPortClause token list = PosnWrapper { getPos = getPos token, unPos = list }

-- |New 'ArchitectureBody'
newArchitectureBody :: WrappedToken -> WrappedSimpleName -> WrappedName -> ArchitectureDeclarativePart -> ArchitectureStatementPart -> (Maybe WrappedSimpleName) -> WrappedArchitectureBody
newArchitectureBody token name1 name2 declarations statements name3 =
   PosnWrapper { getPos = getPos token, unPos = ArchitectureBody name1 name2 declarations statements name3 }

-- |New 'ConfigurationDeclaration'
newConfigurationDeclaration :: WrappedToken -> WrappedSimpleName -> WrappedName -> ConfigurationDeclarativePart -> WrappedBlockConfiguration -> (Maybe WrappedSimpleName) -> WrappedConfigurationDeclaration
newConfigurationDeclaration token simpleName1 name declarations configurations simpleName2 =
   PosnWrapper { getPos = getPos token, unPos = ConfigurationDeclaration simpleName1 name declarations configurations simpleName2 }

-- |New 'BlockConfiguration'
newBlockConfiguration :: WrappedToken -> WrappedBlockSpecification -> [WrappedUseClause] -> [WrappedConfigurationItem] -> WrappedBlockConfiguration
newBlockConfiguration token spec uses configures = PosnWrapper { getPos = getPos token, unPos = BlockConfiguration spec uses configures }

-- |New 'BlockSpecification' (name)
newBlockSpecification_Name :: WrappedName -> WrappedBlockSpecification
newBlockSpecification_Name = raisePosition BlockSpecification_Name

-- |New 'BlockSpecification' (generate)
newBlockSpecification_Generate :: WrappedSimpleName -> WrappedIndexSpecification -> WrappedBlockSpecification
newBlockSpecification_Generate name spec = PosnWrapper { getPos = getPos name, unPos = BlockSpecification_Generate name spec }

-- |New 'ComponentConfiguration'
newComponentConfiguration :: WrappedToken -> WrappedComponentSpecification -> (Maybe WrappedBindingIndication) -> (Maybe WrappedBlockConfiguration) -> WrappedComponentConfiguration
newComponentConfiguration token spec indication configuration =
   PosnWrapper { getPos = getPos token, unPos = ComponentConfiguration spec indication configuration }

------------------------------------------
-- Subprograms and Packages
------------------------------------------

-- |New 'SubprogramDeclaration'
newSubprogramDeclaration :: WrappedSubprogramSpecification -> WrappedSubprogramDeclaration
newSubprogramDeclaration = raisePosition SubprogramDeclaration

-- |New 'SubprogramSpecification': 'Procedure'
newSubprogramSpecification_Procedure :: WrappedToken -> WrappedDesignator -> (Maybe FormalParameterList) -> WrappedSubprogramSpecification
newSubprogramSpecification_Procedure token designator list = PosnWrapper { getPos = getPos token, unPos = ProcedureDeclaration designator list }

-- |New 'SubprogramSpecification': 'Function'
newSubprogramSpecification_Function :: WrappedToken -> WrappedDesignator -> (Maybe FormalParameterList) -> WrappedName -> WrappedSubprogramSpecification
newSubprogramSpecification_Function token designator list name =
   PosnWrapper { getPos = getPos token, unPos = FunctionDeclaration designator list name }

-- |New 'Designator': 'Identifier'
newDesignator_Identifier :: WrappedToken -> WrappedDesignator
newDesignator_Identifier = newFromToken (Designator_Identifier) extractIdentifier

-- |New 'Designator': operator type
newDesignator_Operator :: WrappedToken -> WrappedDesignator
newDesignator_Operator = newFromToken (Designator_Operator) extractString

-- |New 'SubprogramBody'
newSubprogramBody :: WrappedSubprogramSpecification -> SubprogramDeclarativePart -> SubprogramStatementPart -> (Maybe WrappedDesignator) -> WrappedSubprogramBody
newSubprogramBody subprogramSpec declarativePart statementPart designator =
   PosnWrapper { getPos = getPos subprogramSpec, unPos = SubprogramBody subprogramSpec declarativePart statementPart designator }

-- |New 'PackageDeclaration'
newPackageDeclaration :: WrappedToken -> WrappedSimpleName -> PackageDeclarativePart -> (Maybe WrappedSimpleName) -> WrappedPackageDeclaration
newPackageDeclaration token name1 declarations name2 = PosnWrapper { getPos = getPos token, unPos = PackageDeclaration name1 declarations name2 }

-- |New 'PackageBody'
newPackageBody :: WrappedToken -> WrappedSimpleName -> PackageBodyDeclarativePart -> (Maybe WrappedSimpleName) -> WrappedPackageBody
newPackageBody token name1 declarations name2 = PosnWrapper { getPos = getPos token, unPos = PackageBody name1 declarations name2 }

------------------------------------------
-- Declarations
------------------------------------------

-- |New full type declaration
newFullTypeDeclaration :: WrappedToken -> WrappedSimpleName -> WrappedTypeDefinition -> WrappedTypeDeclaration
newFullTypeDeclaration token name typeDef = PosnWrapper { getPos = getPos token, unPos = FullTypeDeclaration name typeDef }

-- |New incomplete type declaration
newIncompleteTypeDefinition :: WrappedToken -> WrappedSimpleName -> WrappedTypeDeclaration
newIncompleteTypeDefinition token name = PosnWrapper { getPos = getPos token, unPos = IncompleteTypeDefinition name }

-- |New subtype delcaration
newSubtypeDeclaration :: WrappedToken -> WrappedSimpleName -> WrappedSubtypeIndication -> WrappedSubtypeDeclaration
newSubtypeDeclaration token name subtype = PosnWrapper { getPos = getPos token, unPos = SubtypeDeclaration name subtype }

-- |New constant declaration
newConstantDeclaration :: WrappedToken -> [WrappedSimpleName] -> WrappedSubtypeIndication -> (Maybe WrappedExpression) -> WrappedConstantDeclaration
newConstantDeclaration token names subtype expression =
   PosnWrapper { getPos = getPos token, unPos = ConstantDeclaration names subtype expression }

-- |New signal declaration
newSignalDeclaration :: WrappedToken -> [WrappedSimpleName] -> WrappedSubtypeIndication -> (Maybe WrappedSignalKind) -> (Maybe WrappedExpression) -> WrappedSignalDeclaration
newSignalDeclaration token names subtype signalKind exp =
   PosnWrapper { getPos = getPos token, unPos = SignalDeclaration names subtype signalKind exp }

-- |New variable declaration
newVariableDeclaration :: WrappedToken -> [WrappedSimpleName] -> WrappedSubtypeIndication -> (Maybe WrappedExpression) -> WrappedVariableDeclaration
newVariableDeclaration token names subtype expression =
   PosnWrapper { getPos = getPos token, unPos = VariableDeclaration names subtype expression }

-- |New file declaration
newFileDeclaration :: WrappedToken -> WrappedSimpleName -> WrappedSubtypeIndication -> (Maybe WrappedMode) -> WrappedExpression -> WrappedFileDeclaration
newFileDeclaration token name subtype mode expression =
   PosnWrapper { getPos = getPos token, unPos = FileDeclaration name subtype mode expression }

-- |New alias declaration
newAliasDeclaration :: WrappedToken -> WrappedSimpleName -> WrappedSubtypeIndication -> WrappedName -> WrappedAliasDeclaration
newAliasDeclaration token simpleName subtype name =
   PosnWrapper { getPos = getPos token, unPos = AliasDeclaration simpleName subtype name }

-- |New attribute declaration
newAttributeDeclaration :: WrappedToken -> WrappedSimpleName -> WrappedName -> WrappedAttributeDeclaration
newAttributeDeclaration token simpleName name = PosnWrapper { getPos = getPos token, unPos = AttributeDeclaration simpleName name }

-- |New component declaration
newComponentDeclaration :: WrappedToken -> WrappedSimpleName -> (Maybe WrappedGenericClause) -> (Maybe WrappedPortClause) -> WrappedComponentDeclaration
newComponentDeclaration token name generics ports = PosnWrapper { getPos = getPos token, unPos = ComponentDeclaration name generics ports }

------------------------------------------
-- Specifications
------------------------------------------

-- |New attribute specification
newAttributeSpecification :: WrappedToken -> WrappedSimpleName -> WrappedEntityNameList -> WrappedEntityClass -> WrappedExpression -> WrappedAttributeSpecification
newAttributeSpecification token name list entityClass exp =
   PosnWrapper { getPos = getPos token, unPos = AttributeSpecification name list entityClass exp }

-- |New entity name list
newEntityNameList_List :: [WrappedEntityDesignator] -> WrappedEntityNameList
newEntityNameList_List list = PosnWrapper { getPos = getPos $ last list, unPos = EntityNameList_List list }

-- |New entity name list (others)
newEntityNameList_Others :: WrappedToken -> WrappedEntityNameList
newEntityNameList_Others token = PosnWrapper { getPos = getPos token, unPos = EntityNameList_Others }

-- |New entity name list (all)
newEntityNameList_All :: WrappedToken -> WrappedEntityNameList
newEntityNameList_All token = PosnWrapper { getPos = getPos token, unPos = EntityNameList_All }

-- |New 'ConfigurationSpecification'
newConfigurationSpecification :: WrappedToken -> WrappedComponentSpecification -> WrappedBindingIndication -> WrappedConfigurationSpecification
newConfigurationSpecification token spec indication = PosnWrapper { getPos = getPos token, unPos = ConfigurationSpecification spec indication }

-- |New 'ComponentSpecification'
newComponentSpecification :: WrappedInstantiationList -> WrappedName -> WrappedComponentSpecification
newComponentSpecification list name = PosnWrapper { getPos = getPos list, unPos = ComponentSpecification list name }

-- |New 'InstantiationList' (list)
newInstantiation_List :: [WrappedSimpleName] -> WrappedInstantiationList
newInstantiation_List names = PosnWrapper { getPos = getPos $ last names, unPos = InstantiationList_Label names }

-- |New 'InstantiationList' (others)
newInstantiation_Others :: WrappedToken -> WrappedInstantiationList
newInstantiation_Others token = PosnWrapper { getPos = getPos token, unPos = InstantiationList_Others }

-- |New 'InstantiationList' (all)
newInstantiation_All :: WrappedToken -> WrappedInstantiationList
newInstantiation_All token = PosnWrapper { getPos = getPos token, unPos = InstantiationList_All }

-- |New 'BindingIndication'
newBindingIndication :: WrappedEntityAspect -> (Maybe AssociationList) -> (Maybe AssociationList) -> WrappedBindingIndication
newBindingIndication entity list1 list2 = PosnWrapper { getPos = getPos entity, unPos = BindingIndication entity list1 list2 }

-- |New 'EntityAspect' (entity)
newEntityAspect_Entity :: WrappedToken -> WrappedName -> (Maybe WrappedSimpleName) -> WrappedEntityAspect
newEntityAspect_Entity token name simpleName = PosnWrapper { getPos = getPos token, unPos = EntityAspect_Entity name simpleName }

-- |New 'EntityAspect' (configuration)
newEntityAspect_Configuration :: WrappedToken -> WrappedName -> WrappedEntityAspect
newEntityAspect_Configuration token name = PosnWrapper { getPos = getPos token, unPos = EntityAspect_Configuration name }

-- |New 'EntityAspect' (open)
newEntityAspect_Open :: WrappedToken -> WrappedEntityAspect
newEntityAspect_Open token = PosnWrapper { getPos = getPos token, unPos = EntityAspect_Open }

-- |New disconnection specification
newDisconnectionSpecification :: WrappedToken -> WrappedGuardedSignalList -> WrappedName -> WrappedExpression -> WrappedDisconnectionSpecification
newDisconnectionSpecification token list name exp =
   PosnWrapper { getPos = getPos token, unPos = DisconnectionSpecification list name exp }

-- |New guarded signal list
newGuardedSignal_List :: [WrappedName] -> WrappedGuardedSignalList
newGuardedSignal_List list = PosnWrapper { getPos = getPos $ last list, unPos = GuardedSignal_List list }

-- |New guarded signal list (others)
newGuardedSignal_Others :: WrappedToken -> WrappedGuardedSignalList
newGuardedSignal_Others token = PosnWrapper { getPos = getPos token, unPos = GuardedSignal_Others }

-- |New guarded signal list (all)
newGuardedSignal_All :: WrappedToken -> WrappedGuardedSignalList
newGuardedSignal_All token = PosnWrapper { getPos = getPos token, unPos = GuardedSignal_All }

------------------------------------------
-- Sequential statements
------------------------------------------

-- |New 'WaitStatement'
newWaitStatement :: WrappedToken -> (Maybe SensitivityList) -> (Maybe WrappedExpression) -> (Maybe WrappedExpression) -> WrappedWaitStatement
newWaitStatement token list exp1 exp2 = PosnWrapper { getPos = getPos token, unPos = WaitStatement list exp1 exp2 }

-- |New 'AssertionStatement'
newAssertionStatement :: WrappedToken -> WrappedExpression -> (Maybe WrappedExpression) -> (Maybe WrappedExpression) -> WrappedAssertionStatement
newAssertionStatement token exp1 exp2 exp3 = PosnWrapper { getPos = getPos token, unPos = AssertionStatement exp1 exp2 exp3 }

-- |New 'SignalAssignmentStatement'
newSignalAssignmentStatement :: WrappedTarget -> (Maybe WrappedSignalAssignmentTransport) -> Waveform -> WrappedSignalAssignmentStatement
newSignalAssignmentStatement target transport waveform = PosnWrapper { getPos = getPos target, unPos = SignalAssignmentStatement target transport waveform }

-- |New 'Target': 'Name'
newTarget_Name :: WrappedName -> WrappedTarget
newTarget_Name = raisePosition Target_Name

-- |New 'Target': 'Aggregate'
newTarget_Aggregate :: WrappedToken -> Aggregate -> WrappedTarget
newTarget_Aggregate token aggregate = PosnWrapper { getPos = getPos token, unPos = Target_Aggregate aggregate }

-- |New 'WaveformElement': 'Expression'
newWaveform_Expression :: WrappedExpression -> (Maybe WrappedExpression) -> WrappedWaveformElement
newWaveform_Expression exp1 exp2 = PosnWrapper { getPos = getPos exp1, unPos = Waveform_Expression exp1 exp2 }

-- |New 'WaveformElement': null
newWaveform_Null :: WrappedToken -> (Maybe WrappedExpression) -> WrappedWaveformElement
newWaveform_Null token exp = PosnWrapper { getPos = getPos token, unPos = Waveform_Null exp }

-- |New 'VariableAssignmentStatement'
newVariableAssignmentStatement :: WrappedTarget -> WrappedExpression -> WrappedVariableAssignmentStatement
newVariableAssignmentStatement target exp = PosnWrapper { getPos = getPos target, unPos = VariableAssignmentStatement target exp }

-- |New 'IfStatement'
newIfStatement :: WrappedToken -> WrappedExpression -> SequenceOfStatements -> [WrappedElsifStatement] -> (Maybe SequenceOfStatements) -> WrappedIfStatement
newIfStatement token exp statements elsifs elses = PosnWrapper { getPos = getPos token, unPos = IfStatement exp statements elsifs elses }

-- |New 'ElsifStatement'
newElsifStatement :: WrappedToken -> WrappedExpression -> SequenceOfStatements -> WrappedElsifStatement
newElsifStatement token exp statements = PosnWrapper { getPos = getPos token, unPos = ElsifStatement exp statements }

-- |New 'CaseStatement'
newCaseStatement :: WrappedToken -> WrappedExpression -> [WrappedCaseStatementAlternative] -> WrappedCaseStatement
newCaseStatement token expression alternatives = PosnWrapper { getPos = getPos token, unPos = CaseStatement expression alternatives }

-- |New 'CaseStatementAlternative'
newCaseStatementAlternative :: WrappedToken -> Choices -> SequenceOfStatements -> WrappedCaseStatementAlternative
newCaseStatementAlternative token choices statements = PosnWrapper { getPos = getPos token, unPos = CaseStatementAlternative choices statements }

-- |New 'LoopStatement'
newLoopStatement :: (Maybe WrappedSimpleName) -> (Either WrappedToken WrappedIterationScheme) -> SequenceOfStatements -> (Maybe WrappedSimpleName) -> WrappedLoopStatement
newLoopStatement (Just name1) (Right scheme) statements name2 = PosnWrapper { getPos = getPos name1, unPos = LoopStatement (Just name1) (Just scheme) statements name2 }
newLoopStatement (Just name1) (Left _) statements name2 = PosnWrapper { getPos = getPos name1, unPos = LoopStatement (Just name1) Nothing statements name2 }
newLoopStatement Nothing (Right scheme) statements name = PosnWrapper { getPos = getPos scheme, unPos = LoopStatement Nothing (Just scheme) statements name }
newLoopStatement Nothing (Left token) statements name = PosnWrapper { getPos = getPos token, unPos = LoopStatement Nothing Nothing statements name }

-- |New 'IterationScheme': while
newIterationScheme_While :: WrappedToken -> WrappedExpression -> WrappedIterationScheme
newIterationScheme_While token exp = PosnWrapper { getPos = getPos token, unPos = IterationScheme_While exp }

-- |New 'IterationScheme': for
newIterationScheme_For :: WrappedToken -> WrappedSimpleName -> WrappedDiscreteRange -> WrappedIterationScheme
newIterationScheme_For token name range = PosnWrapper { getPos = getPos token, unPos = IterationScheme_For name range }

-- |New 'NextStatement'
newNextStatement :: WrappedToken -> (Maybe WrappedSimpleName) -> (Maybe WrappedExpression) -> WrappedNextStatement
newNextStatement token name expression = PosnWrapper { getPos = getPos token, unPos = NextStatement name expression }

-- |New 'ExitStatement'
newExitStatement :: WrappedToken -> (Maybe WrappedSimpleName) -> (Maybe WrappedExpression) -> WrappedExitStatement
newExitStatement token name expression = PosnWrapper { getPos = getPos token, unPos = ExitStatement name expression }

-- |New 'ReturnStatement'
newReturnStatement :: WrappedToken -> (Maybe WrappedExpression) -> WrappedReturnStatement
newReturnStatement token expression = PosnWrapper { getPos = getPos token, unPos = ReturnStatement expression }

-- |New procedure call statement
newProcedureCallStatement :: WrappedName -> (Maybe ActualParameterPart) -> WrappedProcedureCallStatement
newProcedureCallStatement name parameters = PosnWrapper { getPos = getPos name, unPos = ProcedureCallStatement name parameters }

------------------------------------------
-- Concurrent Statements
------------------------------------------

-- |New 'BlockStatement'
newBlockStatement :: WrappedSimpleName -> (Maybe WrappedExpression) -> BlockHeader -> BlockDeclarativePart -> BlockStatementPart -> (Maybe WrappedSimpleName) -> WrappedBlockStatement
newBlockStatement name1 exp header declarations statements name2 =
   PosnWrapper { getPos = getPos name1, unPos = BlockStatement name1 exp header declarations statements name2 }

-- |New 'BlockHeader_Generic'
newBlockHeader_Generic :: WrappedGenericClause -> (Maybe WrappedGenericMapAspect) -> WrappedBlockHeader_Generic
newBlockHeader_Generic clause aspect = PosnWrapper { getPos = getPos clause, unPos = BlockHeader_Generic clause aspect }

-- |New 'BlockHeader_Port'
newBlockHeader_Port :: WrappedPortClause -> (Maybe WrappedPortMapAspect) -> WrappedBlockHeader_Port
newBlockHeader_Port clause aspect = PosnWrapper { getPos = getPos clause, unPos = BlockHeader_Port clause aspect }

-- |New 'GenericMapAspect'
newGenericMapAspect :: WrappedToken -> GenericMapAspect -> WrappedGenericMapAspect
newGenericMapAspect token aspect = PosnWrapper { getPos = getPos token, unPos = aspect }

-- |New 'PortMapAspect'
newPortMapAspect :: WrappedToken -> PortMapAspect -> WrappedPortMapAspect
newPortMapAspect token aspect = PosnWrapper { getPos = getPos token, unPos = aspect }

-- |New process statement
newProcessStatement :: (Either WrappedSimpleName WrappedToken) -> (Maybe SensitivityList) -> ProcessDeclarativePart -> ProcessStatementPart -> (Maybe WrappedSimpleName) -> WrappedProcessStatement
newProcessStatement (Left label1) list declaratives statements label2 =
   PosnWrapper { getPos = getPos label1, unPos = ProcessStatement (Just label1) list declaratives statements label2 }
newProcessStatement (Right token) list declaratives statements label =
   PosnWrapper { getPos = getPos token, unPos = ProcessStatement Nothing list declaratives statements label }

-- |New concurrent assertion statement
newConcurrentAssertionStatement :: (Maybe WrappedSimpleName) -> WrappedAssertionStatement -> WrappedConcurrentAssertionStatement
newConcurrentAssertionStatement (Just name) assertion = PosnWrapper { getPos = getPos name, unPos = ConcurrentAssertionStatement (Just name) assertion }
newConcurrentAssertionStatement Nothing assertion = PosnWrapper { getPos = getPos assertion, unPos = ConcurrentAssertionStatement Nothing assertion }

-- |New concurrent procedure call
newConcurrentProcedureCall :: (Maybe WrappedSimpleName) -> WrappedProcedureCallStatement -> WrappedConcurrentProcedureCall
newConcurrentProcedureCall (Just name) procedure = PosnWrapper { getPos = getPos name, unPos = ConcurrentProcedureCall (Just name) procedure }
newConcurrentProcedureCall Nothing procedure = PosnWrapper { getPos = getPos procedure, unPos = ConcurrentProcedureCall Nothing procedure }

-- |New 'ConcurrentSignalAssignmentStatement': conditional
newConcurrentSignalAssignmentStatement_Conditional :: (Maybe WrappedSimpleName) -> WrappedTarget -> SignalAssignmentOptions -> [WrappedConditionalWaveformPair] -> Waveform -> WrappedConcurrentSignalAssignmentStatement
newConcurrentSignalAssignmentStatement_Conditional (Just name) target options waveforms waveform = PosnWrapper { getPos = getPos name, unPos = ConditionalSignalAssignment (Just name) target options waveforms waveform }
newConcurrentSignalAssignmentStatement_Conditional Nothing target options waveforms waveform = PosnWrapper { getPos = getPos target, unPos = ConditionalSignalAssignment Nothing target options waveforms waveform }

-- |New 'ConcurrentSignalAssignmentStatement': selected
newConcurrentSignalAssignmentStatement_Selected :: (Maybe WrappedSimpleName) -> WrappedExpression -> WrappedTarget -> SignalAssignmentOptions -> [WrappedSelectedWaveformPair] -> WrappedConcurrentSignalAssignmentStatement
newConcurrentSignalAssignmentStatement_Selected (Just name) expression target options waveforms = PosnWrapper { getPos = getPos name, unPos = SelectedSignalAssignment (Just name) expression target options waveforms }
newConcurrentSignalAssignmentStatement_Selected Nothing expression target options waveforms = PosnWrapper { getPos = getPos expression, unPos = SelectedSignalAssignment Nothing expression target options waveforms }

-- |New 'ConditionalWaveformPair'
newConditionalWaveformPair :: Waveform -> WrappedExpression -> WrappedConditionalWaveformPair
newConditionalWaveformPair waveforms exp = PosnWrapper { getPos = getPos $ last waveforms, unPos = ConditionalWaveformPair waveforms exp }

-- |New 'SelectedWaveformPair'
newSelectedWaveformPair :: Waveform -> Choices -> WrappedSelectedWaveformPair
newSelectedWaveformPair waveforms choices = PosnWrapper { getPos = getPos $ last waveforms, unPos = SelectedWaveformPair waveforms choices }

-- |New 'ComponentInstantiationStatement'
newComponentInstantiationStatement :: WrappedSimpleName -> WrappedName -> (Maybe WrappedGenericMapAspect) -> (Maybe WrappedPortMapAspect) -> WrappedComponentInstantiationStatement
newComponentInstantiationStatement simpleName name generics ports =
   PosnWrapper { getPos = getPos simpleName, unPos = ComponentInstantiationStatement simpleName name generics ports }

-- |New 'GenerateStatement'
newGenerateStatement :: WrappedSimpleName -> WrappedGenerationScheme -> [WrappedConcurrentStatement] -> (Maybe WrappedSimpleName) -> WrappedGenerateStatement
newGenerateStatement name1 scheme statements name2 = PosnWrapper { getPos = getPos name1, unPos = GenerateStatement name1 scheme statements name2 }

-- |New 'GenerationScheme': for
newGenerationScheme_For :: WrappedToken -> WrappedSimpleName -> WrappedDiscreteRange -> WrappedGenerationScheme
newGenerationScheme_For token name discrete = PosnWrapper { getPos = getPos token, unPos = GenerationScheme_For name discrete }

-- |New 'GenerationScheme': if
newGenerationScheme_If :: WrappedToken -> WrappedExpression -> WrappedGenerationScheme
newGenerationScheme_If token exp = PosnWrapper { getPos = getPos token, unPos = GenerationScheme_If exp }

------------------------------------------
-- Types
------------------------------------------

-- |New enumeration type
newEnumerationType :: WrappedToken -> [WrappedEnumerationLiteral] -> WrappedTypeDefinition
newEnumerationType token list = PosnWrapper { getPos = getPos token, unPos = EnumerationTypeDefinition list }

-- |New universal type
newUniversalType :: WrappedToken -> WrappedRange -> WrappedTypeDefinition
newUniversalType token range = PosnWrapper { getPos = getPos token, unPos = UniversalTypeDefinition range }

-- |New physical type
newPhysicalType :: WrappedToken -> WrappedRange -> WrappedSimpleName -> [WrappedSecondaryUnitDeclaration] -> WrappedTypeDefinition
newPhysicalType token range name list = PosnWrapper { getPos = getPos token, unPos = PhysicalTypeDefinition range name list }

-- |New secondary unit declaration
newSecondaryUnitDeclaration :: WrappedSimpleName -> WrappedPhysicalLiteral -> WrappedSecondaryUnitDeclaration
newSecondaryUnitDeclaration name physicalLiteral = PosnWrapper { getPos = getPos name, unPos = SecondaryUnitDeclaration name physicalLiteral }

-- |New unconstrained array type
newUnconstrainedArrayType :: WrappedToken -> [WrappedName] -> WrappedSubtypeIndication -> WrappedTypeDefinition
newUnconstrainedArrayType token names subtype = PosnWrapper { getPos = getPos token, unPos = UnconstrainedArrayTypeDefinition names subtype }

-- |New constrained array type
newConstrainedArrayType :: WrappedToken -> IndexConstraint -> WrappedSubtypeIndication -> WrappedTypeDefinition
newConstrainedArrayType token indices subtype = PosnWrapper { getPos = getPos token, unPos = ConstrainedArrayTypeDefinition indices subtype }

-- |New record type
newRecordType :: WrappedToken -> [WrappedElementDeclaration] -> WrappedTypeDefinition
newRecordType token list = PosnWrapper { getPos = getPos token, unPos = RecordTypeDefinition list }

-- |New element declaration
newElementDeclaration :: [WrappedSimpleName] -> WrappedSubtypeIndication -> WrappedElementDeclaration
newElementDeclaration names subtype = PosnWrapper { getPos = getPos $ last names, unPos = ElementDeclaration names subtype }

-- |New access type
newAccessType :: WrappedToken -> WrappedSubtypeIndication -> WrappedTypeDefinition
newAccessType token subtype = PosnWrapper { getPos = getPos token, unPos = AccessTypeDefinition subtype }

-- |New file type
newFileType :: WrappedToken -> WrappedName -> WrappedTypeDefinition
newFileType token name = PosnWrapper { getPos = getPos token, unPos = FileTypeDefinition name }

------------------------------------------
-- Names and Expressions
------------------------------------------

-- |New 'Name': 'SimpleName'
-- Convert 'WrappedToken' into 'WrappedName' containing simple name
newName_Simple :: WrappedToken -> WrappedName
newName_Simple = newFromToken (Name_Simple) extractIdentifier

-- |New 'Name': 'StringLiteral' (holding string form of operator)
-- Convert 'WrappedToken' into 'WrappedName' containing operator
newName_Operator :: WrappedToken -> WrappedName
newName_Operator = newFromToken (Name_Operator) extractString

-- |New 'Name': 'SelectedName'
newName_Selected :: WrappedPrefix -> WrappedSuffix -> WrappedName
newName_Selected prefix suffix = raisePosition Name_Selected $ newSelectedName prefix suffix

-- |New 'SelectedName'
newSelectedName :: WrappedPrefix -> WrappedSuffix -> WrappedSelectedName
newSelectedName prefix suffix =
   PosnWrapper { getPos = getPos prefix, unPos = SelectedName prefix suffix }

-- |New 'Name': 'IndexedName'
-- Convert 'WrappedPrefix', 'WrappedExpressionList' into 'WrappedName' containing indexed name
newName_Indexed :: WrappedPrefix -> [WrappedExpression] -> WrappedName
newName_Indexed prefix expressionList =
   PosnWrapper { getPos = getPos prefix, unPos = Name_Indexed $ IndexedName prefix expressionList }

-- |New 'Name': 'SliceName'
-- Convert 'WrappedPrefix', 'WrappedDiscreteRange' into 'WrappedName' containing slice name
newName_Slice :: WrappedPrefix -> WrappedDiscreteRange -> WrappedName
newName_Slice prefix discreteRange =
   PosnWrapper { getPos = getPos prefix, unPos = Name_Slice $ SliceName prefix discreteRange }

-- |New 'Name': 'AttributeName'
newName_Attribute :: WrappedAttributeName -> WrappedName
newName_Attribute attributeName = PosnWrapper { getPos = getPos attributeName, unPos = Name_Attribute $ unPos attributeName }

-- |New 'AttributeName'
newAttributeName :: WrappedPrefix -> WrappedSimpleName -> Maybe WrappedExpression -> WrappedAttributeName
newAttributeName prefix attributeDesignator expression =
   PosnWrapper { getPos = getPos prefix, unPos = AttributeName prefix attributeDesignator expression }

-- |New 'Prefix': 'Name'
-- Convert 'WrappedName' into 'WrappedPrefix' containing name
newPrefix_Name :: WrappedName -> WrappedPrefix
newPrefix_Name name = PosnWrapper { getPos = getPos name, unPos = Prefix_Name $ unPos name }

-- |New 'Prefix': 'FunctionCall'
-- Convert 'WrappedName', 'ActualParameterPart' into 'WrappedPrefix' containing function call
newPrefix_Function :: WrappedName -> ActualParameterPart -> WrappedPrefix
newPrefix_Function name actualParamPart =
   let functionCall = newFunctionCall name actualParamPart
   in PosnWrapper { getPos = getPos functionCall, unPos = Prefix_Function $ unPos functionCall }

-- |New 'Suffix': 'SimpleName'
-- Convert 'WrappedToken' into 'WrappedSuffix' containing simple name
newSuffix_Identifier :: WrappedToken -> WrappedSuffix
newSuffix_Identifier = newFromToken (Suffix_Name) extractIdentifier

-- |New 'Suffix': 'Char'
-- Convert 'WrappedToken' into 'WrappedSuffix' containing character literal
newSuffix_Char :: WrappedToken -> WrappedSuffix
newSuffix_Char = newFromToken (Suffix_Char) extractChar

-- |New 'Suffix': 'OperatorSymbol'
-- Convert 'WrappedToken' into 'WrappedSuffix' containing operator symbol
newSuffix_Operator :: WrappedToken -> WrappedSuffix
newSuffix_Operator = newFromToken (Suffix_Operator) extractString

-- |New 'Suffix': *all* keyword
-- Convert 'WrappedToken' into 'WrappedSuffix'
newSuffix_All :: WrappedToken -> WrappedSuffix
newSuffix_All token = PosnWrapper { getPos = getPos token, unPos = Suffix_All }

-- |New 'SimpleName'
-- Convert 'WrappedToken' into 'WrappedSimpleName'
newSimpleName :: WrappedToken -> WrappedSimpleName
newSimpleName token = PosnWrapper { getPos = getPos token, unPos = extractIdentifier $ unPos token }

-- |New 'AssociationElement'
-- Convert 'WrappedFormalPart', 'WrappedActualPart' into 'WrappedAssociationElement'
newAssociationElement :: Maybe WrappedFormalPart -> WrappedActualPart -> WrappedAssociationElement
newAssociationElement (Just formalPart) actualPart =
   PosnWrapper { getPos = getPos formalPart, unPos = AssociationElement (Just formalPart) actualPart }
newAssociationElement Nothing actualPart =
   PosnWrapper { getPos = getPos actualPart, unPos = AssociationElement Nothing actualPart }

-- |New 'FormalPart': 'Name'
-- Convert 'WrappedName' into 'WrappedFormalPart'
newFormalPart_Name :: WrappedName -> WrappedFormalPart
newFormalPart_Name designator =
   PosnWrapper { getPos = getPos designator, unPos = FormalPart_Designator designator }

-- |New 'FormalPart': multiple 'Name's
-- Convert 'WrappedName', 'WrappedName' into 'WrappedFormalPart'
newFormalPart_Function :: WrappedName -> WrappedName -> WrappedFormalPart
newFormalPart_Function functionName designator =
   PosnWrapper { getPos = getPos functionName, unPos = FormalPart_Function functionName designator }

-- |New 'ActualPart': 'ActualDesignator'
-- Convert 'WrappedActualDesignator' into 'WrappedActualPart'
newActualPart_Designator :: WrappedActualDesignator -> WrappedActualPart
newActualPart_Designator actualDesignator =
   PosnWrapper { getPos = getPos actualDesignator, unPos = ActualPart_Designator $ unPos actualDesignator }

-- |New 'ActualPart': 'Name', 'ActualDesignator' (function)
-- Convert 'WrappedName', 'WrappedActualDesignator' into 'WrappedActualPart'
newActualPart_Function :: WrappedName -> WrappedActualDesignator -> WrappedActualPart
newActualPart_Function name actualDesignator =
   PosnWrapper { getPos = getPos name, unPos = ActualPart_Function name actualDesignator }

-- |New 'ActualDesignator': 'Expression'
-- Convert 'WrappedExpression' into 'WrappedActualDesignator'
newActualDesignator_Expression :: WrappedExpression -> WrappedActualDesignator
newActualDesignator_Expression expression =
   PosnWrapper { getPos = getPos expression, unPos = ActualDesignator_Expression $ unPos expression }

-- |New 'ActualDesignator': 'Name'
-- Convert 'WrappedName' into 'WrappedActualDesignator'
newActualDesignator_Name :: WrappedName -> WrappedActualDesignator
newActualDesignator_Name name =
   PosnWrapper { getPos = getPos name, unPos = ActualDesignator_Name $ unPos name }

-- |New 'ActualDesignator': __open__ keyword
-- Convert 'WrappedOpen' into 'WrappedActualDesignator'
newActualDesignator_Open :: WrappedToken -> WrappedActualDesignator
newActualDesignator_Open token =
   PosnWrapper { getPos = getPos token, unPos = ActualDesignator_Open }

-- |New 'Expression': (single 'Relation')
-- Convert 'WrappedRelation' into 'WrappedExpression'
newExpression_Relation :: WrappedRelation -> WrappedExpression
newExpression_Relation relation =
   PosnWrapper { getPos = getPos relation, unPos = Expression_Relation $ unPos relation }

-- |New 'Expression': (and expression)
-- Convert 'WrappedRelation's into 'WrappedExpression'
newExpression_And :: [WrappedRelation] -> WrappedExpression
newExpression_And relations =
   PosnWrapper { getPos = getPos $ last relations, unPos = Expression_And relations }

-- |New 'Expression': (or expression)
-- Convert 'WrappedRelation's into 'WrappedExpression'
newExpression_Or :: [WrappedRelation] -> WrappedExpression
newExpression_Or relations =
   PosnWrapper { getPos = getPos $ last relations, unPos = Expression_Or relations }

-- |New 'Expression': (xor expression)
-- Convert 'WrappedRelation's into 'WrappedExpression'
newExpression_Xor :: [WrappedRelation] -> WrappedExpression
newExpression_Xor relations =
   PosnWrapper { getPos = getPos $ last relations, unPos = Expression_Xor relations }

-- |New 'Expression': (nand expression)
-- Convert 'WrappedRelation', 'WrappedRelation' into 'WrappedExpression'
newExpression_Nand :: WrappedRelation -> WrappedRelation -> WrappedExpression
newExpression_Nand relation1 relation2 =
   PosnWrapper { getPos = getPos relation1, unPos = Expression_Nand relation1 relation2 }

-- |New 'Expression': (nor expression)
-- Convert 'WrappedRelation', 'WrappedRelation' into 'WrappedExpression'
newExpression_Nor :: WrappedRelation -> WrappedRelation -> WrappedExpression
newExpression_Nor relation1 relation2 =
   PosnWrapper { getPos = getPos relation1, unPos = Expression_Nor relation1 relation2 }

-- |New 'Relation': compare two simple expressions
-- Convert 'WrappedSimpleExpression', 'WrappedSimpleExpression' into 'WrappedRelation'
newRelation_Compare :: WrappedSimpleExpression -> WrappedRelationalOperator -> WrappedSimpleExpression -> WrappedRelation
newRelation_Compare simpleExp1 relationalOperator simpleExp2 =
   PosnWrapper { getPos = getPos simpleExp1, unPos = Relation_Compare simpleExp1 relationalOperator simpleExp2 }

-- |New 'Relation': single simple expressions
-- Convert 'WrappedSimpleExpression' into 'WrappedRelation'
newRelation_Term :: WrappedSimpleExpression -> WrappedRelation
newRelation_Term simpleExp =
   PosnWrapper { getPos = getPos simpleExp, unPos = Relation_Term $ unPos simpleExp }

-- |New 'SimpleExpression'
-- Convert 'WrappedSign', 'WrappedTerm', '[WrappedAddingOperation]' into 'WrappedExpression'
newSimpleExpression :: (Maybe WrappedSign) -> WrappedTerm -> [WrappedAddingOperation] -> WrappedSimpleExpression
newSimpleExpression (Just sign) term operationList =
   PosnWrapper { getPos = getPos sign, unPos = SimpleExpression (Just sign) term operationList }
newSimpleExpression Nothing term operationList =
   PosnWrapper { getPos = getPos term, unPos = SimpleExpression Nothing term operationList }

-- |New 'Term'
-- Convert 'WrappedFactor', '[WrappedMultiplyingOperation]' into 'WrappedTerm'
newTerm :: WrappedFactor -> [WrappedMultiplyingOperation] -> WrappedTerm
newTerm factor multiplies = PosnWrapper { getPos = getPos factor, unPos = Term factor multiplies }

-- |New 'AddingOperation'
-- Convert 'WrappedOperator', 'WrappedTerm' into 'WrappedAddingOperation'
newAddingOperation :: WrappedAddingOperator -> WrappedTerm -> WrappedAddingOperation
newAddingOperation operator term = PosnWrapper { getPos = getPos operator, unPos = AddingOperation operator term }

-- |New 'MultiplyingOperation'
-- Convert 'WrappedOperator', 'WrappedFactor' into 'WrappedMultiplyingOperation'
newMultiplyingOperation :: WrappedMultiplyingOperator -> WrappedFactor -> WrappedMultiplyingOperation
newMultiplyingOperation operator factor = PosnWrapper { getPos = getPos operator, unPos = MultiplyingOperation operator factor }

-- |New 'Factor': single 'Primary'
-- Convert 'WrappedPrimary' into 'WrappedFactor'
newFactor_Value :: WrappedPrimary -> WrappedFactor
newFactor_Value primary = PosnWrapper { getPos = getPos primary, unPos = Factor_Value $ unPos primary }

-- |New 'Factor': primary power primary
-- Convert 'WrappedPrimary' into 'WrappedFactor'
newFactor_Pow :: WrappedPrimary -> WrappedPrimary -> WrappedFactor
newFactor_Pow primary1 primary2 = PosnWrapper { getPos = getPos primary1, unPos = Factor_Pow primary1 primary2 }

-- |New 'Factor': absolute primary
-- Convert 'WrappedToken', 'WrappedPrimary' into 'WrappedFactor'
newFactor_Abs :: WrappedToken -> WrappedPrimary -> WrappedFactor
newFactor_Abs token primary = PosnWrapper { getPos = getPos token, unPos = Factor_Abs primary }

-- |New 'Factor': not primary
-- Convert 'WrappedToken', 'WrappedPrimary' into 'WrappedFactor'
newFactor_Not :: WrappedToken -> WrappedPrimary -> WrappedFactor
newFactor_Not token primary = PosnWrapper { getPos = getPos token, unPos = Factor_Not primary }

-- |New 'Primary': 'Name'
-- Convert 'WrappedName' into 'WrappedPrimary'
newPrimary_Name :: WrappedName -> WrappedPrimary
newPrimary_Name name = PosnWrapper { getPos = getPos name, unPos = Primary_Name $ unPos name }

-- |New 'Primary': 'Literal'
-- Convert 'WrappedLiteral' into 'WrappedPrimary'
newPrimary_Literal :: WrappedLiteral -> WrappedPrimary
newPrimary_Literal literal = PosnWrapper { getPos = getPos literal, unPos = Primary_Literal $ unPos literal }

-- |New 'Primary': 'Aggregate'
-- Convert 'Aggregate' into 'WrappedPrimary'
newPrimary_Aggregate :: WrappedToken -> Aggregate -> WrappedPrimary
newPrimary_Aggregate token aggregate =
   PosnWrapper { getPos = getPos token, unPos = Primary_Aggregate aggregate }

-- |New 'Primary': 'FunctionCall'
-- Convert 'WrappedName', 'WrappedFunctionCall' into 'WrappedPrimary'
newPrimary_FunctionCall :: WrappedFunctionCall -> WrappedPrimary
newPrimary_FunctionCall = raisePosition Primary_FunctionCall

-- |New 'Primary': 'QualifiedExpression'
-- Convert 'WrappedQualifiedExpression' into 'WrappedPrimary'
newPrimary_QualifiedExpression :: WrappedQualifiedExpression -> WrappedPrimary
newPrimary_QualifiedExpression qualifiedExpression =
   PosnWrapper { getPos = getPos qualifiedExpression, unPos = Primary_QualifiedExpression $ unPos qualifiedExpression }

-- |New 'Primary': 'TypeConversion'
-- Convert 'WrappedTypeConversion' into 'WrappedPrimary'
newPrimary_TypeConversion :: WrappedName -> WrappedExpression -> WrappedPrimary
newPrimary_TypeConversion name expression =
   PosnWrapper { getPos = getPos name, unPos = Primary_TypeConversion $ TypeConversion name expression }

-- |New 'Primary': 'Allocator'
-- Convert 'WrappedToken', 'WrappedAllocator' into 'WrappedPrimary'
newPrimary_Allocator :: WrappedAllocator -> WrappedPrimary
newPrimary_Allocator allocator =
   PosnWrapper { getPos = getPos allocator, unPos = Primary_Allocator $ unPos allocator }

-- |New 'Primary': 'FunctionCall'
-- Convert 'WrappedName', 'WrappedFunctionCall' into 'WrappedPrimary'
newPrimary_Expression :: WrappedToken -> WrappedExpression -> WrappedPrimary
newPrimary_Expression token expression = PosnWrapper { getPos = getPos token, unPos = Primary_Expression expression }

-- |New 'FunctionCall'
-- Convert 'WrappedName', 'ActualParameterPart' into 'WrappedFunction'
newFunctionCall :: WrappedName -> ActualParameterPart -> WrappedFunctionCall
newFunctionCall name actualParamPart = PosnWrapper { getPos = getPos name, unPos = FunctionCall name actualParamPart }

-- |New 'QualifiedExpression': 'Expression'
-- Convert 'WrappedName', 'WrappedExpression' into 'WrappedQualifiedExpression'
newQualifiedExpression_Expression :: WrappedName -> WrappedExpression -> WrappedQualifiedExpression
newQualifiedExpression_Expression name expression =
   PosnWrapper { getPos = getPos name, unPos = QualifiedExpression_Expression name expression }

-- |New 'QualifiedExpression': 'Aggregate'
-- Convert 'WrappedName', '[WrappedElementAssociation]' into 'WrappedQualifiedExpression'
newQualifiedExpression_Aggregate :: WrappedName -> [WrappedElementAssociation] -> WrappedQualifiedExpression
newQualifiedExpression_Aggregate name list =
   PosnWrapper { getPos = getPos name, unPos = QualifiedExpression_Aggregate name list }

-- |New 'Allocator': 'SubtypeIndication'
newAllocator_SubtypeIndication :: WrappedToken -> WrappedSubtypeIndication -> WrappedAllocator
newAllocator_SubtypeIndication token subtypeIndication =
   PosnWrapper { getPos = getPos token, unPos = Allocator_Subtype subtypeIndication }

-- |New 'Allocator': 'Expression'
newAllocator_Expression :: WrappedToken -> WrappedQualifiedExpression -> WrappedAllocator
newAllocator_Expression token qualifiedExpression =
   PosnWrapper { getPos = getPos token, unPos = Allocator_Expression qualifiedExpression }

-- |New 'SubtypeIndication'
newSubtypeIndication :: (Maybe WrappedName) -> WrappedName -> (Maybe WrappedConstraint) -> WrappedSubtypeIndication
newSubtypeIndication (Just name1) name2 constraint =
   PosnWrapper { getPos = getPos name1, unPos = SubtypeIndication (Just name1) name2 constraint }
newSubtypeIndication Nothing name constraint =
   PosnWrapper { getPos = getPos name, unPos = SubtypeIndication Nothing name constraint }

-- |New 'InterfaceDeclaration'
newInterfaceDeclaration :: (Maybe WrappedInterfaceType) -> [WrappedSimpleName] -> (Maybe WrappedMode) -> WrappedSubtypeIndication -> (Maybe WrappedExpression) -> WrappedInterfaceDeclaration
newInterfaceDeclaration (Just interfaceType) names mode subtype exp =
   PosnWrapper { getPos = getPos interfaceType, unPos = InterfaceDeclaration (Just interfaceType) names mode subtype exp }
newInterfaceDeclaration Nothing names mode subtype exp =
   PosnWrapper { getPos = getPos $ last names, unPos = InterfaceDeclaration Nothing names mode subtype exp }

-- |New 'Constraint': 'RangeConstraint'
newConstraint_Range :: WrappedRangeConstraint -> WrappedConstraint
newConstraint_Range rangeConstraint =
   PosnWrapper { getPos = getPos rangeConstraint, unPos = Constraint_Range $ unPos rangeConstraint }

-- |New 'Constraint': 'IndexConstraint'
newConstraint_Index :: WrappedToken -> IndexConstraint -> WrappedConstraint
newConstraint_Index token indexConstraint = PosnWrapper { getPos = getPos token, unPos = Constraint_Index indexConstraint }

-- |New 'RangeConstraint'
newRangeConstraint :: WrappedToken -> WrappedRange -> WrappedRangeConstraint
newRangeConstraint token range = PosnWrapper { getPos = getPos token, unPos = RangeConstraint range }

-- |New 'ElementAssociation'
newElementAssociation :: (Maybe Choices) -> WrappedExpression -> WrappedElementAssociation
newElementAssociation (Just choices) expression =
   PosnWrapper { getPos = getPos $ last choices, unPos = ElementAssociation (Just choices) expression }
newElementAssociation Nothing expression =
   PosnWrapper { getPos = getPos expression, unPos = ElementAssociation Nothing expression }

-- |New 'Choice': 'Expression'
newChoice_Expression :: WrappedSimpleExpression -> WrappedChoice
newChoice_Expression expression = PosnWrapper { getPos = getPos expression, unPos = Choice_Expression $ unPos expression }

-- |New 'Choice': 'DiscreteRange'
newChoice_DiscreteRange :: WrappedDiscreteRange -> WrappedChoice
newChoice_DiscreteRange discreteRange = PosnWrapper { getPos = getPos discreteRange, unPos = Choice_DiscreteRange $ unPos discreteRange }

-- |New 'Choice': 'SimpleName'
newChoice_ElementName :: WrappedSimpleName -> WrappedChoice
newChoice_ElementName name = PosnWrapper { getPos = getPos name, unPos = Choice_ElementName $ unPos name }

-- |New 'Choice': __others__ keyword
newChoice_Others :: WrappedToken -> WrappedChoice
newChoice_Others token = PosnWrapper { getPos = getPos token, unPos = Choice_Others }

-- |New 'Range': 'AttributeName'
newRange_AttributeName :: WrappedAttributeName -> WrappedRange
newRange_AttributeName attributeName = PosnWrapper { getPos = getPos attributeName, unPos = RangeAttributeName $ unPos attributeName }

-- |New 'Range': range expression
newRange_Expression :: WrappedSimpleExpression -> WrappedDirection -> WrappedSimpleExpression -> WrappedRange
newRange_Expression exp1 direction exp2 = PosnWrapper { getPos = getPos exp1, unPos = RangeExpression exp1 direction exp2 }

-- |New 'Direction'
newDirection :: WrappedToken -> Direction -> WrappedDirection
newDirection token direction = PosnWrapper { getPos = getPos token, unPos = direction }

-- |New 'DiscreteRange': 'SubtypeIndication'
newDiscreteRange_SubtypeIndication :: WrappedSubtypeIndication -> WrappedDiscreteRange
newDiscreteRange_SubtypeIndication subtypeIndication = PosnWrapper { getPos = getPos subtypeIndication, unPos = DiscreteRange_SubtypeIndication $ unPos subtypeIndication }

-- |New 'DiscreteRange': 'Range'
newDiscreteRange_Range :: WrappedRange -> WrappedDiscreteRange
newDiscreteRange_Range range = PosnWrapper { getPos = getPos range, unPos = DiscreteRange_Range $ unPos range }

------------------------------------------
-- Literals
------------------------------------------

-- |New 'NumericLiteral', 'AbstractLiteral': 'UniversalInteger'
-- Convert 'WrappedToken' into 'WrappedLiteral'
newLiteral_Numeric_Abstract_Integer :: WrappedToken -> WrappedLiteral
newLiteral_Numeric_Abstract_Integer = newFromToken (Literal_Numeric . NumericLiteral_Abstract . UniversalInteger) extractUniversalInteger

-- |New 'NumericLiteral', 'AbstractLiteral': 'UniversalReal'
-- Convert 'WrappedToken' into 'WrappedLiteral'
newLiteral_Numeric_Abstract_Real :: WrappedToken -> WrappedLiteral
newLiteral_Numeric_Abstract_Real = newFromToken (Literal_Numeric . NumericLiteral_Abstract . UniversalReal) extractUniversalReal

-- |New 'NumericLiteral': 'PhysicalLiteral'
-- Convert 'WrappedToken' into 'WrappedLiteral' containing physical type, using conversion function
newLiteral_Numeric_Physical :: WrappedPhysicalLiteral -> WrappedLiteral
newLiteral_Numeric_Physical = raisePosition (Literal_Numeric . NumericLiteral_Physical)

-- |New 'EnumerationLiteral': 'String' (identifier)
-- Convert 'WrappedToken' into 'WrappedLiteral'
newLiteral_Enumeration_Identifier :: WrappedToken -> WrappedLiteral
newLiteral_Enumeration_Identifier = newFromToken (Literal_Enumeration . EnumerationLiteral_Identifier) extractIdentifier

-- |New 'EnumerationLiteral': 'Char' (character literal)
-- Convert 'WrappedToken' into 'WrappedLiteral'
newLiteral_Enumeration_Char :: WrappedToken -> WrappedLiteral
newLiteral_Enumeration_Char = newFromToken (Literal_Enumeration . EnumerationLiteral_Char) extractChar

-- |New 'StringLiteral' (string literal)
-- Convert 'WrappedToken' into 'WrappedLiteral'
newLiteral_String :: WrappedToken -> WrappedLiteral
newLiteral_String = newFromToken Literal_String extractString

-- |New 'BitStrLiteral' (bit string literal)
-- Convert 'WrappedToken' into 'WrappedLiteral'
newLiteral_BitStr :: WrappedToken -> WrappedLiteral
newLiteral_BitStr = newFromToken (Literal_BitStr) extractBitStr

-- |New null literal
-- Convert 'WrappedToken' into 'WrappedLiteral'
newLiteral_Null :: WrappedToken -> WrappedLiteral
newLiteral_Null = passPosition Literal_Null

-- |New physical literal
newPhysicalLiteral :: (Maybe WrappedAbstractLiteral) -> WrappedName -> WrappedPhysicalLiteral
newPhysicalLiteral (Just abstractLit) name = PosnWrapper { getPos = getPos abstractLit, unPos = PhysicalLiteral (Just abstractLit) name }
newPhysicalLiteral Nothing name = PosnWrapper { getPos = getPos name, unPos = PhysicalLiteral Nothing name }

------------------------------------------
-- Basic Low Level Types
------------------------------------------

-- |Extract 'Int64' (universal integer type) from 'Token'
extractUniversalInteger :: Token -> Int64
extractUniversalInteger (Tokens.Literal (Tokens.Univ_Int val)) = val

-- |Extract 'Double' (universal real type) from 'Token'
extractUniversalReal :: Token -> Double
extractUniversalReal (Tokens.Literal (Tokens.Univ_Real val)) = val

-- |New 'AbstractLiteral': 'UniversalInteger'
-- Convert 'WrappedToken' into 'WrappedAbstractLiteral'
newAbstractLiteral_Integer :: WrappedToken -> WrappedAbstractLiteral
newAbstractLiteral_Integer = newFromToken (UniversalInteger) extractUniversalInteger

-- |New 'AbstractLiteral': 'UniversalReal'
-- Convert 'WrappedToken' into 'WrappedAbstractLiteral'
newAbstractLiteral_Real :: WrappedToken -> WrappedAbstractLiteral
newAbstractLiteral_Real = newFromToken (UniversalReal) extractUniversalReal

-- |Extract 'String' (string literal type) from 'Token'
extractIdentifier :: Token -> String
extractIdentifier (Tokens.Identifier identifier) = identifier

-- |Extract 'Char' (character literal type) from 'Token'
extractChar :: Token -> Char
extractChar (Tokens.Literal (Tokens.Character char)) = char

-- |Extract 'String' (string literal type) from 'Token'
extractString :: Token -> String
extractString (Tokens.Literal (Tokens.Str str)) = str

-- |Extract 'BitStrLiteral' (bit string literal type) from 'Token'
extractBitStr :: Token -> BitStrLiteral
extractBitStr (Tokens.Literal (Tokens.BitStr base str)) = BitStrLiteral base str

-- |New 'RelationalOperator'
-- Convert 'WrappedToken', 'RelationalOperator' into 'WrappedRelationalOperator'
newRelationalOperator :: WrappedToken -> RelationalOperator -> WrappedRelationalOperator
newRelationalOperator token operator = PosnWrapper { getPos = getPos token, unPos = operator }

-- |New 'Sign'
-- Convert 'WrappedToken', 'Sign' into 'WrappedSign'
newSign :: WrappedToken -> Sign -> WrappedSign
newSign token sign = PosnWrapper { getPos = getPos token, unPos = sign }

-- |New 'AddingOperator'
-- Convert 'WrappedToken', 'AddingOperator' into 'WrappedAddingOperator'
newAddingOperator :: WrappedToken -> AddingOperator -> WrappedAddingOperator
newAddingOperator token operator = PosnWrapper { getPos = getPos token, unPos = operator }

-- |New 'MultiplyingOperator'
-- Convert 'WrappedToken', 'MultiplyingOperator' into 'WrappedMultiplyingOperator'
newMultiplyingOperator :: WrappedToken -> MultiplyingOperator -> WrappedMultiplyingOperator
newMultiplyingOperator token operator = PosnWrapper { getPos = getPos token, unPos = operator }
