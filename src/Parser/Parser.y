{
module Parser.Parser where
import Parser.Happy.Types
import Parser.Happy.Functions
import Parser.Alex.Types (Alex)
import qualified Parser.Lexer as Lex
}

%name v1987
%lexer{Lex.lexer}{Lex.EOF}
%tokentype{Lex.Token}
%monad{Alex}
%error{parseError}

%token
   abs            {Keyword Abs}
   access         {Keyword Access}
   after          {Keyword After}
   alias          {Keyword Alias}
   all            {Keyword All}
   and            {Keyword And}
   architecture   {Keyword Architecture}
   array          {Keyword Array}
   assert         {Keyword Assert}
   attribute      {Keyword Attribute}
   begin          {Keyword Begin}
   block          {Keyword Block}
   body           {Keyword Body}
   buffer         {Keyword Buffer}
   bus            {Keyword Bus}
   case           {Keyword Case}
   component      {Keyword Component}
   configuration  {Keyword Configuration}
   constant       {Keyword Constant}
   disconnect     {Keyword Disconnect}
   downto         {Keyword Downto}
   else           {Keyword Else}
   elsif          {Keyword Elsif}
   end            {Keyword End}
   entity         {Keyword Entity}
   exit           {Keyword Exit}
   file           {Keyword File}
   for            {Keyword For}
   function       {Keyword Function}
   generate       {Keyword Generate}
   generic        {Keyword Generic}
   guarded        {Keyword Guarded}
   if             {Keyword If}
   in             {Keyword In}
   inout          {Keyword Inout}
   is             {Keyword Is}
   label          {Keyword Label}
   library        {Keyword Library}
   linkage        {Keyword Linkage}
   loop           {Keyword Loop}
   map            {Keyword Map}
   mod            {Keyword Mod}
   nand           {Keyword Nand}
   new            {Keyword New}
   next           {Keyword Next}
   nor            {Keyword Nor}
   not            {Keyword Not}
   null           {Keyword Null}
   of             {Keyword Of}
   on             {Keyword On}
   open           {Keyword Open}
   or             {Keyword Or}
   others         {Keyword Others}
   out            {Keyword Out}
   package        {Keyword Package}
   port           {Keyword Port}
   procedure      {Keyword Procedure}
   process        {Keyword Process}
   range          {Keyword Range}
   record         {Keyword Record}
   register       {Keyword Register}
   rem            {Keyword Rem}
   report         {Keyword Report}
   return         {Keyword Return}
   select         {Keyword Select}
   severity       {Keyword Severity}
   signal         {Keyword Signal}
   subtype        {Keyword Subtype}
   then           {Keyword Then}
   to             {Keyword To}
   transport      {Keyword Transport}
   type           {Keyword Type}
   units          {Keyword Units}
   until          {Keyword Until}
   use            {Keyword Use}
   variable       {Keyword Variable}
   wait           {Keyword Wait}
   when           {Keyword When}
   while          {Keyword While}
   with           {Keyword With}
   xor            {Keyword Xor}
   '=>'           {Operator Arrow}
   '**'           {Operator DoubleStar}
   ':='           {Operator VarAssign}
   '/='           {Operator Inequality}
   '>='           {Operator GreaterThanOrEqual}
   '<='           {Operator SignAssign}
   '<>'           {Operator Box}
   '&'            {Operator Ampersand}
   '\''           {Operator Apostrophe}
   '('            {Operator LeftParen}
   ')'            {Operator RightParen}
   '*'            {Operator Star}
   '+'            {Operator Plus}
   ','            {Operator Comma}
   '-'            {Operator Hyphen}
   '.'            {Operator Period}
   '/'            {Operator Slash}
   ':'            {Operator Colon}
   ';'            {Operator Semicolon}
   '<'            {Operator LessThan}
   '='            {Operator Equal}
   '>'            {Operator GreaterThan}
   '|'            {Operator Bar}
   identifier     {Identifier $$}
   integer        {Literal Univ_Int $$}
   real           {Literal Univ_Real $$}
   bitstr         {Literal BitStr _ _}
   str            {Literal Str $$}
   char           {Literal Character $$}

%%

entity_declaration : entity identifier is entity_header entity_declarative_part begin entity_statement_part end identifier ';'  {EntityDeclaration $2 $4 $5 (Just $7) (Just $9)}
                   | entity identifier is entity_header entity_declarative_part begin entity_statement_part end ';'             {EntityDeclaration $2 $4 $5 (Just $7) Nothing}
                   | entity identifier is entity_header entity_declarative_part end identifier ';'                                     {makeEntityDeclaration $2 $4 $5 Nothing (Just $6)}
                   | entity identifier is entity_header entity_declarative_part end ';'                                         {EntityDeclaration $2 $4 $5 Nothing Nothing}

-- formal_generic_clause and formal_port_clause ??
entity_header : generic_clause port_clause   {EntityHeader (Just $1) (Just $2)}
              | generic_clause               {EntityHeader (Just $1) Nothing}
              | port_clause                  {EntityHeader Nothing (Just $1)}
              | {- empty -}                  {EntityHeader Nothing Nothing}

generic_clause : generic '(' generic_list ')' ';' {GenericClause $3}

port_clause : port '(' port_list ')' ';' {PortClause $3}

-- ?? generic_interface_list
generic_list : interface_list {$1}

-- ?? port_interface_list
port_list : interface_list {$1}

entity_declarative_part : {- empty -} {[]}
                        | entity_declarative_part entity_declarative_item {$2 : $1}

entity_declarative_item : subprogram_declaration      {EntityDeclaration_SubprogramDeclaration $1}
                        | subprogram_body             {EntityDeclaration_SubprogramBody $1}
                        | type_declaration            {EntityDeclaration_TypeDeclaration $1}
                        | subtype_declaration         {EntityDeclaration_SubtypeDeclaration $1}
                        | constant_declaration        {EntityDeclaration_ConstantDeclaration $1}
                        | signal_declaration          {EntityDeclaration_SignalDeclaration $1}
                        | file_declaration            {EntityDeclaration_FileDeclaration $1}
                        | alias_declaration           {EntityDeclaration_AliasDeclaration $1}
                        | attribute_declaration       {EntityDeclaration_AttributeDeclaration $1}
                        | attribute_specification     {EntityDeclaration_AttributeSpecification $1}
                        | disconnection_specification {EntityDeclaration_DisconnectionSpecification $1}
                        | use_clause                  {EntityDeclaration_UseClause $1}

entity_statement_part : {- empty -} {[]}
                      | entity_statement_part entity_statement {$2 : $1}

-- ?? passive_concurrent_procedure_call and passive_process_statement
entity_statement : concurrent_assertion_statement  {EntityStatement_ConcurrentAssertionStatement $1}
                 | concurrent_procedure_call       {EntityStatement_ConcurrentProcedureCall $1}
                 | process_statement               {EntityStatement_ProcessStatement $1}

architecture_body : architecture identifier of identifier is architecture_declarative_part begin architecture_statement_part end identifier ';' {ArchitectureBody $2 $4 $6 $8 (Just $10)}
                  | architecture identifier of identifier is architecture_declarative_part begin architecture_statement_part end ';' {ArchitectureBody $2 $4 $6 $8 Nothing}

architecture_declarative_part : {- empty -} {[]}
                              | architecture_declarative_part block_declarative_item {$2 : $1}

block_declarative_item : subprogram_declaration       {BlockDeclarativeItem_SubprogramDeclaration $1}
                       | subprogram_body              {BlockDeclarativeItem_SubprogramBody $1}
                       | type_declaration             {BlockDeclarativeItem_TypeDeclaration $1}
                       | subtype_declaration          {BlockDeclarativeItem_SubtypeDeclaration $1}
                       | constant_declaration         {BlockDeclarativeItem_ConstantDeclaration $1}
                       | signal_declaration           {BlockDeclarativeItem_SignalDeclaration $1}
                       | file_declaration             {BlockDeclarativeItem_FileDeclaration $1}
                       | alias_declaration            {BlockDeclarativeItem_AliasDeclaration $1}
                       | component_declaration        {BlockDeclarativeItem_ComponentDeclaration $1}
                       | attribute_declaration        {BlockDeclarativeItem_AttributeDeclaration $1}
                       | attribute_specification      {BlockDeclarativeItem_AttributeSpecification $1}
                       | configuration_specification  {BlockDeclarativeItem_ConfigurationSpecification $1}
                       | disconnection_specification  {BlockDeclarativeItem_DisconnectionSpecification $1}
                       | use_clause                   {BlockDeclarativeItem_UseClause $1}

architecture_statement_part : {- empty -} {[]}
                            | architecture_statement_part concurrent_statement {$2 : $1}

configuration_declaration : configuration identifier of identifier is configuration_declarative_part block_configuration end identifier ';'   {ConfigurationDeclaration $2 $4 $6 $7 (Just $9)}
                          | configuration identifier of identifier is configuration_declarative_part block_configuration end ';'              {ConfigurationDeclaration $2 $4 $6 $7 Nothing}

configuration_declarative_part : {- empty -} {[]}
                               | configuration_declarative_part configuration_declarative_item {$2 : $1}

configuration_declarative_item : use_clause              {ConfigurationDeclarativeItem_UseClause $1}
                               | attribute_specification {ConfigurationDeclarativeItem_AttributeSpecification $1}

block_configuration : for block_specification use_clause_list configuration_item_list end for ';' {BlockConfiguration $2 $3 $4}

use_clause_list : {- empty -}                {[]}
                | use_clause_list use_clause {$2 : $1}

configuration_item_list : {- empty -}                                {[]}
                        | configuration_item_list configuration_item {$2 : $1}

block_specification : identifier                               {BlockSpecificationIdentifier $1}
                    | identifier '(' index_specification ')'   {BlockSpecificationGenerate $1 $2}

-- ?? static_expression
index_specification : discrete_range   {IndexSpecification_DiscreteRange $1}
                    | expression       {IndexSpecification_Expression $1}

configuration_item : block_configuration     {ConfigurationItem_BlockConfiguration $1}
                   | component_configuration {ConfigurationItem_ComponentConfiguration $1}

component_configuration : for component_specification use binding_indication ';' block_configuration end for ';'  {ComponentConfiguration $2 (Just $4) (Just $6)}
                            | for component_specification use binding_indication ';' end for ';'                                {ComponentConfiguration $2 (Just $4) Nothing}
                            | for component_specification block_configuration end for ';'                                {ComponentConfiguration $2 Nothing (Just $3)}
                            | for component_specification end for ';'                                {ComponentConfiguration $2 Nothing Nothing}

component_specification : instantiation_list ':' name {ComponentSpecification $1 $3}

subprogram_declaration : subprogram_specification ';' {SubprogramDeclaration $1}

subprogram_specification : procedure designator '(' formal_parameter_list ')'                   {ProcedureDeclaration $2 (Just $4)}
                         | procedure designator                                                    {ProcedureDeclaration $2 Nothing}
                         | function designator '(' formal_parameter_list ')' return type_mark      {FunctionDeclaration $2 (Just $6) $9}
                         | function designator return type_mark                                    {FunctionDeclaration $2 Nothing $4}

designator : identifier {Designator_Identifier $1}
           | '='        {Designator_Operator Equal}
           | '/='       {Designator_Operator Inequality}
           | '<'        {Designator_Operator LessThan}
           | '<='       {Designator_Operator SignAssign}
           | '>'        {Designator_Operator GreaterThan}
           | '>='       {Designator_Operator GreaterThanOrEqual}
           | '+'        {Designator_Operator Plus}
           | '-'        {Designator_Operator Hyphen}
           | '&'        {Designator_Operator Ampersand}
           | '*'        {Designator_Operator Star}
           | '/'        {Designator_Operator Slash}
           | '**'       {Designator_Operator DoubleStar}
           | and        {Designator_Keyword And}
           | or         {Designator_Keyword Or}
           | nand       {Designator_Keyword Nand}
           | nor        {Designator_Keyword Nor}
           | xor        {Designator_Keyword Xor}
           | mod        {Designator_Keyword Mod}
           | rem        {Designator_Keyword Rem}
           | abs        {Designator_Keyword Abs}
           | not        {Designator_Keyword Not}

-- ?? parameter_interface_list
formal_parameter_list : interface_list {$1} -- ?? Is this even needed

subprogram_body : subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end designator ';'  {SubprogramBody $1 $3 $5 (Just $7)}
                | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end ';'             {SubprogramBody $1 $3 $5 Nothing}

subprogram_declarative_part : {- empty -}                                              {[]}
                            | subprogram_declarative_part subprogram_declarative_item  {$2 : $1}

subprogram_declarative_item : subprogram_declaration  {SubprogramDeclarativeItem_SubprogramDeclaration $1}
                            | subprogram_body         {SubprogramDeclarativeItem_SubprogramBody $1}
                            | type_declaration        {SubprogramDeclarativeItem_TypeDeclaration $1}
                            | subtype_declaration     {SubprogramDeclarativeItem_SubtypeDeclaration $1}
                            | constant_declaration    {SubprogramDeclarativeItem_ConstantDeclaration $1}
                            | variable_declaration    {SubprogramDeclarativeItem_VariableDeclaration $1}
                            | file_declaration        {SubprogramDeclarativeItem_FileDeclaration $1}
                            | alias_declaration       {SubprogramDeclarativeItem_AliasDeclaration $1}
                            | attribute_declaration   {SubprogramDeclarativeItem_AttributeDeclaration $1}
                            | attribute_specification {SubprogramDeclarativeItem_AttributeSpecification $1}
                            | use_clause              {SubprogramDeclarativeItem_UseClause $1}

subprogram_statement_part : {- empty -}                                    {[]}
                          | subprogram_statement_part sequential_statement {$2 : $1}

package_declaration : package identifier is package_declarative_part end identifier ';'   {PackageDeclaration $2 $4 (Just $6)}
                    | package identifier is package_declarative_part end ';'                         {PackageDeclaration $2 $4 Nothing}

package_declarative_part : {- empty -}                                        {[]}
                         | package_declarative_part package_declarative_item  {$2 : $1}

package_declarative_item : subprogram_declaration        {PackageDeclarativeItem_SubprogramDeclaration $1}
                         | type_declaration              {PackageDeclarativeItem_TypeDeclaration $1}
                         | subtype_declaration           {PackageDeclarativeItem_SubtypeDeclaration $1}
                         | constant_declaration          {PackageDeclarativeItem_ConstantDeclaration $1}
                         | signal_declaration            {PackageDeclarativeItem_SignalDeclaration $1}
                         | file_declaration              {PackageDeclarativeItem_FileDeclaration $1}
                         | alias_declaration             {PackageDeclarativeItem_AliasDeclaration $1}
                         | component_declaration         {PackageDeclarativeItem_ComponentDeclaration $1}
                         | attribute_declaration         {PackageDeclarativeItem_AttributeDeclaration $1}
                         | attribute_specification       {PackageDeclarativeItem_AttributeSpecification $1}
                         | disconnection_specification   {PackageDeclarativeItem_DisconnectionSpecification $1}
                         | use_clause                    {PackageDeclarativeItem_UseClause $1}

package_body : package body identifier is package_body_declarative_part end identifier ';'   {PackageBody $3 $5 (Just $7)}
             | package body identifier is package_body_declarative_part end ';'              {PackageBody $3 $5 Nothing}

package_body_declarative_part : {- empty -}                                                  {[]}
                              | package_body_declarative_part package_body_declarative_item  {$2 : $1}

package_body_declarative_item : subprogram_declaration                           {PackageBodyDeclarativeItem_SubprogramDeclaration $1}
                              | subprogram_body                                  {PackageBodyDeclarativeItem_SubprogramBody $1}
                              | type_declaration                                 {PackageBodyDeclarativeItem_TypeDeclaration $1}
                              | subtype_declaration                              {PackageBodyDeclarativeItem_SubtypeDeclaration $1}
                              | constant_declaration                             {PackageBodyDeclarativeItem_ConstantDeclaration $1}
                              | file_declaration                                 {PackageBodyDeclarativeItem_FileDeclaration $1}
                              | alias_declaration                                {PackageBodyDeclarativeItem_AliasDeclaration $1}
                              | use_clause                                       {PackageBodyDeclarativeItem_UseClause $1}

scalar_type_definition : enumeration_type_definition  {$1}
                       | integer_type_definition      {$1}
                       | floating_type_definition     {$1}
                       | physical_type_definition     {$1}

range_constraint : range range_definition {$2}

range_definition : attribute_name                              {RangeAttributeName $1}
                 | simple_expression to simple_expression      {RangeExpression $1 To $3}
                 | simple_expression downto simple_expression  {RangeExpression $1 Downto $3}

enumeration_type_definition : '(' enumeration_list ')' {EnumerationTypeDefinition $2}

enumeration_list : enumeration_literal                      {[$1]}
                 | enumeration_list ',' enumeration_literal {$3 : $1}

enumeration_literal : identifier {EnumerationLiteral_Identifier $1}
                    | char       {EnumerationLiteral_Char $1}

integer_type_definition : range_constraint  {IntegerTypeDefinition $1}

physical_type_definition : range_constraint units identifier secondary_unit_declaration_list end units {PhysicalTypeDefinition $1 $3 $4}

secondary_unit_declaration_list : {- empty -}                                                {[]}
                                | secondary_unit_declaration_list secondary_unit_declaration {$2 : $1}

secondary_unit_declaration : identifier '=' physical_literal ';' {SecondaryUnitDeclaration $1 $3}

physical_literal : abstract_literal identifier  {PhysicalLiteral (Just $1) $2}
                 | identifier                   {PhysicalLiteral Nothing $1}

floating_type_definition : range_constraint {FloatingTypeDefinition $1}

composite_type_definition : array_type_definition  {Composite_ArrayTypeDefinition $1}
                          | record element_declaration_list end record {RecordTypeDefinition $2}

array_type_definition : array '(' index_subtype_definition_list ')' of subtype_indication {UnconstrainedArrayTypeDefinition $3 $6}
                      | array '(' discrete_range_list ')' of subtype_indication           {ConstrainedArrayTypeDefinition $3 $6}

index_subtype_definition_list : index_subtype_definition                               {[$1]}
                              | index_subtype_definition_list index_subtype_definition {$2 : $1}

index_subtype_definition : identifier range '<>' {$1}

discrete_range_list : discrete_range                     {[$1]}
                    | discrete_range_list discrete_range {$2 : $1}

discrete_range : subtype_indication {DiscreteRange_SubtypeIndication $1}
               | range_definition   {DiscreteRange_Range $1}

element_declaration_list : element_declaration                          {[$1]}
                         | element_declaration_list element_declaration {$2 : $1}

element_declaration : identifier_list ':' subtype_indication {ElementDeclaration $1 $3}

identifier_list : identifier                       {[$1]}
                | identifier_list ',' identifier   {$3 : $1}

access_type_definition : access subtype_indication {TypeDefinition_Access $2}

incomplete_type_declaration : type identifier ';' {IncompleteTypeDefinition $2}

file_type_definition : file of type_mark {TypeDefinition_File $3}

type_mark : name {$1}

declaration : type_declaration            {Declaration_Type $1}
            | subtype_declaration         {Declaration_Subtype $1}
            | object_declaration          {Declaration_Object $1}
            | file_declaration            {Declaration_File $1}
            | interface_declaration       {Declaration_Interface $1}
            | alias_declaration           {Declaration_Alias $1}
            | attribute_declaration       {Declaration_Attribute $1}
            | component_declaration       {Declaration_Component $1}
            | entity_declaration          {Declaration_Entity $1}
            | configuration_declaration   {Declaration_Configuration $1}
            | subprogram_declaration      {Declaration_Subprogram $1}
            | package_declaration         {Declaration_Package $1}

type_declaration : full_type_declaration        {$1}
                 | incomplete_type_declaration  {$1}

full_type_declaration : type identifier is type_definition {FullTypeDeclaration $2 $4}

type_definition : scalar_type_definition     {TypeDefinition_Scalar $1}
                | composite_type_definition  {TypeDefinition_Composite $1}
                | access_type_definition     {TypeDefinition_Access $1}
                | file_type_definition       {TypeDefinition_File $1}

subtype_declaration : subtype identifier is subtype_indication ';' {SubtypeDeclaration $2 $4}

subtype_indication : name type_mark constraint  {SubtypeIndication (Just $1) $2 (Just $3)}
                   | name type_mark             {SubtypeIndication (Just $1) $2 Nothing}
                   | type_mark constraint       {SubtypeIndication Nothing $1 (Just $2)}
                   | type_mark                  {SubtypeIndication Nothing $1 Nothing}

constraint : range_constraint {Constraint_Range $1}
           | index_constraint {Constraint_Index $1}

index_constraint : '(' index_constraint_list ')' {$2}

index_constraint_list : discrete_range                            {[$1]}
                      | index_constraint_list ',' discrete_range  {$3 : $1}

object_declaration : constant_declaration {ObjectDeclaration_Constant $1}
                   | signal_declaration   {ObjectDeclaration_Signal $1}
                   | variable_declaration {ObjectDeclaration_Variable $1}

constant_declaration : constant identifier_list ':' subtype_indication ':=' expression ';'   {ConstantDeclaration $2 $4 (Just $6)}
                     | constant identifier_list ':' subtype_indication ';'                   {ConstantDeclaration $2 $4 Nothing}

signal_declaration : signal identifier_list ':' subtype_indication signal_kind ':=' expression ';' {SignalDeclaration $2 $4 (Just $5) (Just $7)}
                   | signal identifier_list ':' subtype_indication signal_kind ';'                 {SignalDeclaration $2 $4 (Just $5) Nothing}
                   | signal identifier_list ':' subtype_indication ':=' expression ';'             {SignalDeclaration $2 $4 Nothing (Just $6)}
                   | signal identifier_list ':' subtype_indication ';'                             {SignalDeclaration $2 $4 Nothing Nothing}

signal_kind : register {Register}
            | bus {Bus}

variable_declaration : variable identifier_list ':' subtype_indication ':=' expression ';'   {VariableDeclaration $2 $4 (Just $6)}
                     | variable identifier_list ':' subtype_indication ';'                   {VariableDeclaration $2 $4 Nothing}

file_declaration : file identifier ':' subtype_indication is mode file_logical_name ';'   {FileDeclaration $2 $4 (Just $6) $7}
                 | file identifier ':' subtype_indication is file_logical_name ';'        {FileDeclaration $2 $4 Nothing $6}

-- ??  string_expression
file_logical_name : expression {$1}

interface_declaration : constant identifier_list ':' in     subtype_indication      ':=' expression   {InterfaceDeclaration (Just Constant)        $2 (Just In)   $5 (Just $7)}
                      | constant identifier_list ':' in     subtype_indication                        {InterfaceDeclaration (Just Constant)        $2 (Just In)   $5 Nothing}
                      | constant identifier_list ':'        subtype_indication      ':=' expression   {InterfaceDeclaration (Just Constant)        $2 Nothing     $4 (Just $6)}
                      | constant identifier_list ':'        subtype_indication                        {InterfaceDeclaration (Just Constant)        $2 Nothing     $4 Nothing}
                      | signal   identifier_list ':' mode   subtype_indication bus  ':=' expression   {InterfaceDeclaration (Just GuardedSignal)   $2 (Just $4)   $5 (Just $8)}
                      | signal   identifier_list ':' mode   subtype_indication bus                    {InterfaceDeclaration (Just GuardedSignal)   $2 (Just $4)   $5 Nothing}
                      | signal   identifier_list ':' mode   subtype_indication      ':=' expression   {InterfaceDeclaration (Just Signal)          $2 (Just $4)   $5 (Just $6)}
                      | signal   identifier_list ':' mode   subtype_indication                        {InterfaceDeclaration (Just Signal)          $2 (Just $4)   $5 Nothing}
                      | signal   identifier_list ':'        subtype_indication bus  ':=' expression   {InterfaceDeclaration (Just GuardedSignal)   $2 Nothing     $4 (Just $7)}
                      | signal   identifier_list ':'        subtype_indication bus                    {InterfaceDeclaration (Just GuardedSignal)   $2 Nothing     $4 Nothing}
                      | signal   identifier_list ':'        subtype_indication      ':=' expression   {InterfaceDeclaration (Just Signal)          $2 Nothing     $4 (Just $6)}
                      | signal   identifier_list ':'        subtype_indication                        {InterfaceDeclaration (Just Signal)          $2 Nothing     $4 Nothing}
                      |          identifier_list ':' mode   subtype_indication bus  ':=' expression   {InterfaceDeclaration (Just GuardedSignal)   $1 (Just $3)   $4 (Just $7)}
                      |          identifier_list ':' mode   subtype_indication bus                    {InterfaceDeclaration (Just GuardedSignal)   $1 (Just $3)   $4 Nothing}
                      |          identifier_list ':'        subtype_indication bus  ':=' expression   {InterfaceDeclaration (Just GuardedSignal)   $1 Nothing     $3 (Just $6)}
                      |          identifier_list ':'        subtype_indication bus                    {InterfaceDeclaration (Just GuardedSignal)   $1 Nothing     $3 Nothing}
                      | variable identifier_list ':' mode   subtype_indication      ':=' expression   {InterfaceDeclaration (Just Variable)        $2 (Just $4)   $5 (Just $7)}
                      | variable identifier_list ':' mode   subtype_indication                        {InterfaceDeclaration (Just Variable)        $2 (Just $4)   $5 Nothing}
                      | variable identifier_list ':'        subtype_indication      ':=' expression   {InterfaceDeclaration (Just Variable)        $2 Nothing     $4 (Just $6)}
                      | variable identifier_list ':'        subtype_indication                        {InterfaceDeclaration (Just Variable)        $2 Nothing     $4 Nothing}
                      |          identifier_list ':' mode   subtype_indication      ':=' expression   {InterfaceDeclaration Nothing                $1 (Just $3)   $4 (Just $6)}
                      |          identifier_list ':' mode   subtype_indication                        {InterfaceDeclaration Nothing                $1 (Just $3)   $4 Nothing}
                      |          identifier_list ':'        subtype_indication      ':=' expression   {InterfaceDeclaration Nothing                $1 Nothing     $3 (Just $5)}
                      |          identifier_list ':'        subtype_indication                        {InterfaceDeclaration Nothing                $1 Nothing     $3 Nothing}

mode : in      {In}
     | out     {Out}
     | inout   {Inout}
     | buffer  {Buffer}
     | linkage {Linkage}

interface_list : interface_element                    {[$1]}
               | interface_list ';' interface_element {$3 : $1}

interface_element : interface_declaration {$1}

association_list : association_element                      {[$1]}
                 | association_list ',' association_element {$3 : $1}

association_element : formal_part '=>' actual_part {AssociationElement (Just $1) $3}
                    |                  actual_part {AssociationElement Nothing $1}

formal_part : formal_designator              {FormalPart_Designator $1}
            | name '(' formal_designator ')' {FormalPart_Function $1 $3}

formal_designator : name {$1}

actual_part : actual_designator              {ActualPart_Designator $1}
            | name '(' actual_designator ')' {ActualPart_Function $1 $3}

actual_designator : expression   {ActualDesignator_Expression $1}
                  | name         {ActualDesignator_Name $1}
                  | open         {ActualDesignator_Open}

alias_declaration : alias identifier ':' subtype_indication is name ';' {AliasDeclaration $2 $4 $6}

attribute_declaration : attribute identifier ':' type_mark ';' {AttributeDeclaration $2 $4}

component_declaration : component identifier generic_clause port_clause end component ';' {ComponentDeclaration $2 (Just $3) (Just $4)}
component_declaration : component identifier generic_clause             end component ';' {ComponentDeclaration $2 (Just $3) Nothing}
component_declaration : component identifier                port_clause end component ';' {ComponentDeclaration $2 Nothing (Just $3)}
component_declaration : component identifier                            end component ';' {ComponentDeclaration $2 Nothing Nothing}

attribute_specification : attribute attribute_designator of entity_name_list ':' entity_class is expression ';' {AttributeSpecification $2 $4 $6 $8}

entity_name_list : entity_designator_list {AttributeSpecificationEntityName_List $1}
                 | others                 {AttributeSpecificationEntityName_Others}
                 | all                    {AttributeSpecificationEntityName_All}

entity_designator_list : entity_designator                              {[$1]}
                       | entity_designator_list ',' entity_designator   {$3 : $1}

entity_designator : simple_name     {EntityDesignator_Name $1}
                  | operator_symbol {EntityDesignator_Operator $1}

entity_class : entity         {EntityClass_Entity}
             | architecture   {EntityClass_Architecture}
             | configuration  {EntityClass_Configuration}
             | procedure      {EntityClass_Procedure}
             | function       {EntityClass_Function}
             | package        {EntityClass_Package}
             | type           {EntityClass_Type}
             | subtype        {EntityClass_Subtype}
             | constant       {EntityClass_Constant}
             | signal         {EntityClass_Signal}
             | variable       {EntityClass_Variable}
             | component      {EntityClass_Component}
             | label          {EntityClass_Label}

configuration_specification : for component_specification use binding_indication ';' {ConfigurationSpecification $2 $4}

instantiation_list : label_list  {InstantiationList_Label $1}
                   | others      {InstantiationList_Others}
                   | all         {InstantiationList_All}

binding_indication : entity_aspect generic_map_aspect port_map_aspect   {BindingIndication $1 (Just $2) (Just $3)}
                   | entity_aspect generic_map_aspect                   {BindingIndication $1 (Just $2) Nothing}
                   | entity_aspect                    port_map_aspect   {BindingIndication $1 Nothing (Just $2)}
                   | entity_aspect                                      {BindingIndication $1 Nothing Nothing}

entity_aspect : entity name '(' identifier ')'  {EntityAspect_Entity $2 (Just $4)}
              | entity name                     {EntityAspect_Entity $2 Nothing}
              | configuration name              {EntityAspect_Configuration $2}
              | open                            {EntityAspect_Open}

generic_map_aspect : generic map '(' association_list ')' {$3}

port_map_aspect : port map '(' association_list ')' {$3}

disconnection_specification : disconnection signal_list ':' type_mark after expression ';' {DisconnectionSpecification $2 $4 $6}

signal_list : name_list {GuardedSignal_List $1}
            | others    {GuardedSignal_Others}
            | all       {GuardedSignal_All}

name_list : name                 {[$1]}
          | name_list ',' name   {$3 : $1}

name : simple_name      {Name_Simple $1}
     | operator_symbol  {Name_Operator $1}
     | selected_name    {Name_Selected $1}
     | indexed_name     {Name_Indexed $1}
     | slice_name       {Name_Slice $1}
     | attribute_name   {Name_Attribute $1}

prefix : name           {Prefix_Name $1}
       | function_call  {Prefix_Name $1}

simple_name : identifier {$1}

selected_name : prefix '.' suffix {SelectedName $1 $3}

suffix : simple_name       {Suffix_Name $1}
       | char              {Suffix_Char $1}
       | operator_symbol   {Suffix_Operator $1}
       | all               {Suffix_All}

indexed_name : prefix '(' expression_list ')' {IndexedName $1 $3}

expression_list : expression                       {[$1]}
                | expression_list ',' expression   {$3 : $1}

slice_name : prefix '(' discrete_range ')' {SliceName $1 $3}

attribute_name : prefix '\'' attribute_designator '(' expression ')' {AttributeName $1 $3 (Just $5)}
               | prefix '\'' attribute_designator                    {AttributeName $1 $3 Nothing}

attribute_designator : simple_name {$1}

expression : relation         {Expression_Relation $1}
           | and_expression   {Expression_And $1}
           | or_expression    {Expression_Or $1}
           | xor_expression   {Expression_Xor $1}
           | nand_expression  {Expression_Nand $1}
           | nor_expression   {Expression_Nor $1}

and_expression : relation and and_expression {AndExpression $1 $3}
               | relation and relation       {AndRelation $1 $3}

or_expression : relation or or_expresssion   {OrExpression $1 $3}
              | realtion or relation         {OrRelation $1 $3}

xor_expression : relation xor xor_expression {XorExpression $1 $3}
               | relation xor relation       {XorRelation $1 $3}

nand_expression : relation nand relation {NandExpression $1 $3}

nor_expression : relation nor relation {NorExpression $1 $3}

relation : simple_expression relational_operator simple_expression   {Relation_Compare $1 $2 $3}
         | simple_expression                                         {Relation_Term $1}

simple_expression : '+' term adding_operation_list {SimpleExpression Positive $2 $3}
                  | '-' term adding_operation_list {SimpleExpression Negative $2 $3}
                  |     term adding_operation_list {SimpleExpression Positive $1 $2}

adding_operation_list : {- empty -}                            {[]}
                      | adding_operation_list adding_operation {$2 : $1}

adding_operation : adding_operator term {AddingOperation $1 $2}

adding_operator : '+' {Add}
                | '-' {Minus}
                | '&' {Concat}

term : factor multiplying_operation_list {Term $1 $2}

multiplying_operation_list : {- empty -}                                      {[]}
                           | multiplying_operation_list multiplying_operation {$2 : $1}

multiplying_operation : multiplying_operator factor {MultiplyingOperation $1 $2}

multiplying_operator : '*' {Multiply}
                     | '/' {Divide}
                     | mod {Mod}
                     | rem {Rem}

factor : primary '**' primary {Factor_Pow $1 $3}
       | primary              {Factor_Value $1}
       | abs primary          {Factor_Abs $1}
       | not primary          {Factor_Not $1}

primary : name                   {Primary_Name $1}
        | literal                {Primary_Literal $1}
        | aggregate              {Primary_Aggregate $1}
        | function_call          {Primary_FunctionCall $1}
        | qualified_expression   {Primary_QualifiedExpression $1}
        | type_conversion        {Primary_TypeConversion $1}
        | allocator              {Primary_Allocator $1}
        | '(' expression ')'     {Primary_Expression $2}

literal : numeric_literal     {Literal_Numeric $1}
        | enumeration_literal {Literal_Enumeration $1}
        | string_literal      {Literal_String $1}
        | bit_string_literal  {Literal_BitStr $1}
        | null                {Literal_Null}

numeric_literal : abstract_literal {NumericLiteral_Abstract $1}
                | physical_literal {NumericLiteral_Physical $1}

abstract_literal : integer {UniversalInteger $1}
                 | real    {UniversalReal $1}

string_literal : str {$1}

bit_string_literal : bitstr {$1 & \Literal (BitStr (base,value)) -> BitStrLiteral base value}

aggregate : '(' element_association_list ')' {$2}

element_association_list : element_association                                {[$1]}
                         | element_association_list ',' element_association   {$3 : $1}

element_association : choices '=>'  expression {ElementAssociation (Just $1) $3}
                    |               expression {ElementAssociation Nothing $1}

choices : choice              {[$1]}
        | choices '|' choice  {$3 : $1}

choice : simple_expression {Choice_Expression $1}
       | discrete_range    {Choice_DiscreteRange $1}
       | simple_name       {Choice_ElementName $1}
       | others            {Choice_Others}

-- Second part may never be triggered because higher level section has 'name' on its own
function_call : name '(' actual_parameter_part ')' {FunctionCall $1 (Just $3)}
              | name                               {FunctionCall $1 Nothing}

actual_parameter_part : association_list {$1}

qualified_expression : type_mark '\'' '(' expression ')' {QualifiedExpression_Expression $1 $4}
                     | type_mark '\'' aggregate          {QualifiedExpression_Aggregate $1 $3}

type_conversion : type_mark '(' expression ')' {TypeConversion $1 $3}

allocator : new subtype_indication     {Allocator_Subtype $2}
          | new qualified_expression   {Allocator_Expression $2}

sequence_of_statements : {- empty -}                                 {[]}
                       | sequence_of_statements sequential_statement {$2 : $1}

sequential_statement : wait_statement                 {SequentialStatement_Wait $1}
                     | assertion_statement            {SequentialStatement_Assertion $1}
                     | signal_assignment_statement    {SequentialStatement_SignalAssignment $1}
                     | variable_assignment_statement  {SequentialStatement_VariableAssignment $1}
                     | procedure_call_statement       {SequentialStatement_ProcedureCall $1}
                     | if_statement                   {SequentialStatement_If $1}
                     | case_statement                 {SequentialStatement_Case $1}
                     | loop_statement                 {SequentialStatement_Loop $1}
                     | next_statement                 {SequentialStatement_Next $1}
                     | exit_statement                 {SequentialStatement_Exit $1}
                     | return_statement               {SequentialStatement_Return $1}
                     | null                           {SequentialStatement_Null}

wait_statement : wait   on sensitivity_list  until condition   for expression ';'   {WaitStatement (Just $3)   (Just $5)   (Just $7)}
               | wait   on sensitivity_list  until condition                  ';'   {WaitStatement (Just $3)   (Just $5)   Nothing}
               | wait   on sensitivity_list                    for expression ';'   {WaitStatement (Just $3)   Nothing     (Just $5)}
               | wait   on sensitivity_list                                   ';'   {WaitStatement (Just $3)   Nothing     Nothing}
               | wait                        until condition   for expression ';'   {WaitStatement Nothing     (Just $3)   (Just $5)}
               | wait                        until condition                  ';'   {WaitStatement Nothing     (Just $3)   Nothing}
               | wait                                          for expression ';'   {WaitStatement Nothing     Nothing     (Just $3)}
               | wait                                                         ';'   {WaitStatement Nothing     Nothing     Nothing}

assertion_statement : assert condition report expression severity expression  ';' {AssertionStatement $2 (Just $4)   (Just $6)}
                    | assert condition report expression                      ';' {AssertionStatement $2 (Just $4)   Nothing}
                    | assert condition                   severity expression  ';' {AssertionStatement $2 Nothing     (Just $4)}
                    | assert condition                                        ';' {AssertionStatement $2 Nothing     Nothing}

signal_assignment_statement : target '<=' transport   waveform ';' {SignalAssignmentStatement $1 SignalAssignmentTransport $4}
                            | target '<='             waveform ';' {SignalAssignmentStatement $1 SignalAssignmentNormal $3}

target : name        {Target_Name $1}
       | aggregate   {Target_Aggregate $1}

waveform : waveform_element               {[$1]}
         | waveform ',' waveform_element  {$3 : $1}

waveform_element : expression after expression  {Waveform_Expression $1 (Just $3)}
                 | expression                   {Waveform_Expression $1 Nothing}
                 | null       after expression  {Waveform_Null (Just $3)}
                 | null                         {Waveform_Null Nothing}

variable_assignment_statement : target ':=' expression ';' {VariableAssignmentStatement $1 $3}

procedure_call_statement : name '(' actual_parameter_part ')'  ';' {ProcedureCallStatement $1 (Just $3)}
                         | name                                ';' {ProcedureCallStatement $1 Nothing}

if_statement : if condition then sequence_of_statements elsif_statement_list else sequence_of_statements end if ';' {IfStatement $2 $3 $4 (Just $6)}
             | if condition then sequence_of_statements elsif_statement_list                             end if ';' {IfStatement $2 $3 $4 Nothing}

elsif_statement_list : {- empty -}                          {[]}
                     | elsif_statement_list elsif_statement {$2 : $1}

elsif_statement : elsif condition then sequence_of_statements {ElsifStatement $2 $4}

case_statement : case expression is case_statement_alternative_list end case ';' {CaseStatement $2 $4}

case_statement_alternative_list : case_statement_alternative                                 {[$1]}
                                | case_statement_alternative_list case_statement_alternative {$2 : $1}

case_statement_alternative : when choices '=>' sequence_of_statements {CaseStatementAlternative $2 $4}

loop_statement : identifier ':'  iteration_scheme  loop sequence_of_statements end loop identifier ';' {LoopStatement (Just ($1,$8)) (Just $3) $5}
               |                 iteration_scheme  loop sequence_of_statements end loop            ';' {LoopStatement Nothing (Just $1) $3}
               | identifier ':'                    loop sequence_of_statements end loop identifier ';' {LoopStatement (Just ($1,$7)) Nothing $4}
               |                                   loop sequence_of_statements end loop            ';' {LoopStatement Nothing Nothing $2}

iteration_scheme : while condition                    {IterationScheme_While $2}
                 | for identifier in discrete_range   {IterationScheme_For $2 $4}

next_statement : next identifier when condition ';' {NextStatement (Just $2) (Just $4)}
               | next identifier                ';' {NextStatement (Just $2) Nothing}
               | next            when condition ';' {NextStatement Nothing (Just $3)}
               | next                           ';' {NextStatement Nothing Nothing}

exit_statement : exit identifier when condition ';' {ExitStatement (Just $2) (Just $4)}
               | exit identifier                ';' {ExitStatement (Just $2) Nothing}
               | exit            when condition ';' {ExitStatement Nothing (Just $3)}
               | exit                           ';' {ExitStatement Nothing Nothing}

return_statement : return expression   ';' {ReturnStatement (Just $2)}
                 | return              ';' {ReturnStatement Nothing}

concurrent_statement : block_statement                         {Concurrent_BlockStatement $1}
                     | process_statement                       {Concurrent_ProcessStatement $1}
                     | concurrent_procedure_call               {Concurrent_ProcedureCall $1}
                     | concurrent_assertion_statement          {Concurrent_AssertionStatement $1}
                     | concurrent_signal_assignment_statement  {Concurrent_SignalAssignmentStatement $1}
                     | component_instantiation_statement       {Concurrent_ComponentInstantiationStatement $1}
                     | generate_statement                      {Concurrent_GenerateStatement $1}

block_statement : identifier ':' block '(' expression ')'   block_header block_declarative_part begin block_statement_part end block identifier ';' {BlockStatement $1 (Just $5) $7 $8 $10 (Just $13)}
                | identifier ':' block '(' expression ')'   block_header block_declarative_part begin block_statement_part end block            ';' {BlockStatement $1 (Just $5) $7 $8 $10 Nothing}
                | identifier ':' block                      block_header block_declarative_part begin block_statement_part end block identifier ';' {BlockStatement $1 Nothing $4 $5 $7 (Just $10)}
                | identifier ':' block                      block_header block_declarative_part begin block_statement_part end block            ';' {BlockStatement $1 Nothing $4 $5 $7 Nothing}

block_header : block_header_generic block_header_port {BlockHeader (Just $1) (Just $2)}
             | block_header_generic                   {BlockHeader (Just $1) Nothing}
             |                      block_header_port {BlockHeader Nothing (Just $1)}
             | {- empty -}                            {BlockHeader Nothing Nothing}

block_header_generic : generic_clause generic_map_aspect ';' {BlockHeader_Generic $1 (Just $2)}
                     | generic_clause                    ';' {BlockHeader_Generic $1 Nothing}

block_header_port : port_clause port_map_aspect ';' {BlockHeader_Port $1 (Just $2)}
                  | port_clause                 ';' {BlockHeader_Port $1 Nothing}

block_declarative_part : {- empty -}                                    {[]}
                       | block_declarative_part block_declarative_item  {$2 : $1}

block_statement_part : {- empty -}                                {[]}
                     | block_statement_part concurrent_statement  {$2 : $1}

process_statement : identifier ':'  process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process identifier  ';'   {ProcessStatement (Just ($1,$12)) (Just $5) $7 $9}
                  |                 process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process             ';'   {ProcessStatement Nothing (Just $3) $5 $7}
                  | identifier ':'  process                          process_declarative_part begin process_statement_part end process identifier  ';'   {ProcessStatement (Just ($1,$9)) Nothing $4 $6}
                  |                 process                          process_declarative_part begin process_statement_part end process             ';'   {ProcessStatement Nothing Nothing $2 $4}

process_declarative_part : {- empty -}                                        {[]}
                         | process_declarative_part process_declarative_item  {$2 : $1}

process_statement_part : {- empty -}                                 {[]}
                       | process_statement_part sequential_statement {$2 : $1}

process_declarative_item : subprogram_declaration  {ProcessDeclarative_SubtypeDeclaration $1}
                         | subprogram_body         {ProcessDeclarative_SubprogramBody $1}
                         | type_declaration        {ProcessDeclarative_TypeDeclaration $1}
                         | subtype_declaration     {ProcessDeclarative_SubtypeDeclaration $1}
                         | constant_declaration    {ProcessDeclarative_ConstantDeclaration $1}
                         | variable_declaration    {ProcessDeclarative_VariableDeclaration $1}
                         | file_declaration        {ProcessDeclarative_FileDeclaration $1}
                         | alias_declaration       {ProcessDeclarative_AliasDeclaration $1}
                         | attribute_declaration   {ProcessDeclarative_AttributeDeclaration $1}
                         | attribute_specification {ProcessDeclarative_AttributeSpecification $1}
                         | use_clause              {ProcessDeclarative_UseClause $1}

concurrent_procedure_call : identifier ':'   procedure_call_statement {ConcurrentProcedureCall (Just $1) $3}
                          |                  procedure_call_statement {ConcurrentProcedureCall Nothing $1}

concurrent_assertion_statement : identifier ':' assertion_statement {ConcurrentAssertionStatement (Just $1) $3}
                               |                assertion_statement {ConcurrentAssertionStatement Nothing $1}

concurrent_signal_assignment_statement : identifier ':'  target '<=' options conditional_waveforms ';'                     {ConditionalSignalAssignment (Just $1) $3 $5 $6}
                                       |                 target '<=' options conditional_waveforms ';'                     {ConditionalSignalAssignment Nothing $1 $3 $4}
                                       | identifier ':'  with expression select target '<=' options selected_waveforms ';' {SelectedSignalAssignment (Just $1) $4 $6 $8 $9}
                                       |                 with expression select target '<=' options selected_waveforms ';' {SelectedSignalAssignment Nothing $2 $4 $6 $7}

options : guarded transport   {SignalAssignmentOptions SignalAssignment_Guarded SignalAssignment_Transport}
        | guarded             {SignalAssignmentOptions SignalAssignment_Guarded SignalAssignment_NonTransport}
        |         transport   {SignalAssignmentOptions SignalAssignment_NonGuarded SignalAssignment_Transport}
        | {- empty -}         {SignalAssignmentOptions SignalAssignment_NonGuarded SignalAssignment_NonTransport}

conditional_waveforms : conditional_waveform_pairs waveform {ConditionalWaveforms $1 $2}

conditional_waveform_pairs : conditional_waveform_pair                              {[$1]}
                           | conditional_waveform_pairs conditional_waveform_pair   {$2 : $1}

conditional_waveform_pair : waveform when condition else {ConditionalWaveformPair $1 $3}

selected_waveforms : selected_waveform_pair                          {[$1]}
                   | selected_waveforms ',' selected_waveform_pair   {$3 : $1}

selected_waveform_pair : waveform when choices {SelectedWaveformPair $1 $3}

component_instantiation_statement : identifier ':' name generic_map_aspect port_map_aspect   ';' {ComponentInstantiationStatement $1 $3 (Just $4) (Just $5)}
                                  | identifier ':' name generic_map_aspect                   ';' {ComponentInstantiationStatement $1 $3 (Just $4) Nothing}
                                  | identifier ':' name                    port_map_aspect   ';' {ComponentInstantiationStatement $1 $3 Nothing (Just $4)}
                                  | identifier ':' name                                      ';' {ComponentInstantiationStatement $1 $3 Nothing Nothing}

generate_statement : identifier ':' generation_scheme generate concurrent_statement_list end generate identifier  ';' {GenerateStatement $1 $3 $5 (Just $8)}
                   | identifier ':' generation_scheme generate concurrent_statement_list end generate             ';' {GenerateStatement $1 $3 $5 Nothing}

generation_scheme : for parameter_specification {GenerationScheme_For $2}
                  | if condition                {GenerationScheme_If $2}

concurrent_statement_list : {- empty -}                                    {[]}
                          | concurrent_statement_list concurrent_statement {$2 : $1}

use_clause : use selected_name_list ';' {UseClause $2}

selected_name_list : selected_name                          {[$1]}
                   | selected_name_list ',' selected_name   {$3 : $1}

design_file : design_unit_list {DesignFile $1}

design_unit_list : design_unit                  {[$1]}
                 | design_unit_list design_unit {$2 : $1}

design_unit : context_clause library_unit {DesignUnit $1 $2}

library_unit : primary_unit   {Library_PrimaryUnit $1}
             | secondary_unit {Library_SecondaryUnit $1}

primary_unit : entity_declaration         {Primary_EntityDeclaration $1}
             | configuration_declaration  {Primary_ConfigurationDeclaration $1}
             | package_declaration        {Primary_PackageDeclaration $1}

secondary_unit : architecture_body  {Secondary_ArchitectureBody $1}
               | package_body       {Secondary_PackageBody $1}

library_clause : library identifier_list ';' {$2}

context_clause : {- empty -}                 {[]}
               | context_clause context_item {$2 : $1}

context_item : library_clause {Context_LibraryClause $1}
             | use_clause {Context_UseClause $1}
