{
module Parser.Parser where

import Parser.Happy.Types
import Parser.Happy.Functions
import Parser.Alex.Monad (Alex)
import qualified Parser.Lexer as Lex
import qualified Parser.TokenTypes as Tokens
}

%name v1987
%lexer{Lex.lexer}{Tokens.EOF}
%tokentype{Tokens.Token}
%monad{Alex}
%error{parseError}

%token
   abs            {Tokens.Keyword Tokens.Abs}
   access         {Tokens.Keyword Tokens.Access}
   after          {Tokens.Keyword Tokens.After}
   alias          {Tokens.Keyword Tokens.Alias}
   all            {Tokens.Keyword Tokens.All}
   and            {Tokens.Keyword Tokens.And}
   architecture   {Tokens.Keyword Tokens.Architecture}
   array          {Tokens.Keyword Tokens.Array}
   assert         {Tokens.Keyword Tokens.Assert}
   attribute      {Tokens.Keyword Tokens.Attribute}
   begin          {Tokens.Keyword Tokens.Begin}
   block          {Tokens.Keyword Tokens.Block}
   body           {Tokens.Keyword Tokens.Body}
   buffer         {Tokens.Keyword Tokens.Buffer}
   bus            {Tokens.Keyword Tokens.Bus}
   case           {Tokens.Keyword Tokens.Case}
   component      {Tokens.Keyword Tokens.Component}
   configuration  {Tokens.Keyword Tokens.Configuration}
   constant       {Tokens.Keyword Tokens.Constant}
   disconnect     {Tokens.Keyword Tokens.Disconnect}
   downto         {Tokens.Keyword Tokens.Downto}
   else           {Tokens.Keyword Tokens.Else}
   elsif          {Tokens.Keyword Tokens.Elsif}
   end            {Tokens.Keyword Tokens.End}
   entity         {Tokens.Keyword Tokens.Entity}
   exit           {Tokens.Keyword Tokens.Exit}
   file           {Tokens.Keyword Tokens.File}
   for            {Tokens.Keyword Tokens.For}
   function       {Tokens.Keyword Tokens.Function}
   generate       {Tokens.Keyword Tokens.Generate}
   generic        {Tokens.Keyword Tokens.Generic}
   guarded        {Tokens.Keyword Tokens.Guarded}
   if             {Tokens.Keyword Tokens.If}
   in             {Tokens.Keyword Tokens.In}
   inout          {Tokens.Keyword Tokens.Inout}
   is             {Tokens.Keyword Tokens.Is}
   label          {Tokens.Keyword Tokens.Label}
   library        {Tokens.Keyword Tokens.Library}
   linkage        {Tokens.Keyword Tokens.Linkage}
   loop           {Tokens.Keyword Tokens.Loop}
   map            {Tokens.Keyword Tokens.Map}
   mod            {Tokens.Keyword Tokens.Mod}
   nand           {Tokens.Keyword Tokens.Nand}
   new            {Tokens.Keyword Tokens.New}
   next           {Tokens.Keyword Tokens.Next}
   nor            {Tokens.Keyword Tokens.Nor}
   not            {Tokens.Keyword Tokens.Not}
   null           {Tokens.Keyword Tokens.Null}
   of             {Tokens.Keyword Tokens.Of}
   on             {Tokens.Keyword Tokens.On}
   open           {Tokens.Keyword Tokens.Open}
   or             {Tokens.Keyword Tokens.Or}
   others         {Tokens.Keyword Tokens.Others}
   out            {Tokens.Keyword Tokens.Out}
   package        {Tokens.Keyword Tokens.Package}
   port           {Tokens.Keyword Tokens.Port}
   procedure      {Tokens.Keyword Tokens.Procedure}
   process        {Tokens.Keyword Tokens.Process}
   range          {Tokens.Keyword Tokens.Range}
   record         {Tokens.Keyword Tokens.Record}
   register       {Tokens.Keyword Tokens.Register}
   rem            {Tokens.Keyword Tokens.Rem}
   report         {Tokens.Keyword Tokens.Report}
   return         {Tokens.Keyword Tokens.Return}
   select         {Tokens.Keyword Tokens.Select}
   severity       {Tokens.Keyword Tokens.Severity}
   signal         {Tokens.Keyword Tokens.Signal}
   subtype        {Tokens.Keyword Tokens.Subtype}
   then           {Tokens.Keyword Tokens.Then}
   to             {Tokens.Keyword Tokens.To}
   transport      {Tokens.Keyword Tokens.Transport}
   type           {Tokens.Keyword Tokens.Type}
   units          {Tokens.Keyword Tokens.Units}
   until          {Tokens.Keyword Tokens.Until}
   use            {Tokens.Keyword Tokens.Use}
   variable       {Tokens.Keyword Tokens.Variable}
   wait           {Tokens.Keyword Tokens.Wait}
   when           {Tokens.Keyword Tokens.When}
   while          {Tokens.Keyword Tokens.While}
   with           {Tokens.Keyword Tokens.With}
   xor            {Tokens.Keyword Tokens.Xor}
   '=>'           {Tokens.Operator Tokens.Arrow}
   '**'           {Tokens.Operator Tokens.DoubleStar}
   ':='           {Tokens.Operator Tokens.VarAssign}
   '/='           {Tokens.Operator Tokens.Inequality}
   '>='           {Tokens.Operator Tokens.GreaterThanOrEqual}
   '<='           {Tokens.Operator Tokens.SignAssign}
   '<>'           {Tokens.Operator Tokens.Box}
   '&'            {Tokens.Operator Tokens.Ampersand}
   '\''           {Tokens.Operator Tokens.Apostrophe}
   '('            {Tokens.Operator Tokens.LeftParen}
   ')'            {Tokens.Operator Tokens.RightParen}
   '*'            {Tokens.Operator Tokens.Star}
   '+'            {Tokens.Operator Tokens.Plus}
   ','            {Tokens.Operator Tokens.Comma}
   '-'            {Tokens.Operator Tokens.Hyphen}
   '.'            {Tokens.Operator Tokens.Period}
   '/'            {Tokens.Operator Tokens.Slash}
   ':'            {Tokens.Operator Tokens.Colon}
   ';'            {Tokens.Operator Tokens.Semicolon}
   '<'            {Tokens.Operator Tokens.LessThan}
   '='            {Tokens.Operator Tokens.Equal}
   '>'            {Tokens.Operator Tokens.GreaterThan}
   '|'            {Tokens.Operator Tokens.Bar}
   identifier     {Tokens.Identifier $$}
   integer        {Tokens.Literal (Tokens.Univ_Int $$)}
   real           {Tokens.Literal (Tokens.Univ_Real $$)}
   bitstr         {Tokens.Literal (Tokens.BitStr _ _)}
   str            {Tokens.Literal (Tokens.Str $$)}
   char           {Tokens.Literal (Tokens.Character $$)}

%%

design_file :: { DesignFile }
            : design_unit_list {DesignFile $1}

-- entity_declaration ::=
--    entity identifier is
--       entity_header
--       entity_declarative_part
--  [ begin
--       entity_statement_part
--    end [ <entity>_simple_name ] ;
-- <entity>_simple_name must repeat identifier
entity_declaration :: { EntityDeclaration }
                   : entity identifier is entity_header entity_declarative_part begin entity_statement_part end identifier ';'  {EntityDeclaration $2 $4 $5 (Just $7) (Just $9)}
                   | entity identifier is entity_header entity_declarative_part begin entity_statement_part end ';'             {EntityDeclaration $2 $4 $5 (Just $7) Nothing}
                   | entity identifier is entity_header entity_declarative_part end identifier ';'                              {EntityDeclaration $2 $4 $5 Nothing (Just $7)}
                   | entity identifier is entity_header entity_declarative_part end ';'                                         {EntityDeclaration $2 $4 $5 Nothing Nothing}

-- entity_header ::=
--    [ <formal>_generic_clause ]
--    [ <formal>_port_clause ]
-- generic_clause ::=
--    generic ( generic_list ) ;
-- port_clause ::=
--    port ( port_list ) ;
-- generic_list ::= <generic>_interface_list
-- port_list ::= <port>_interface_list
entity_header :: { EntityHeader }
              : generic '(' interface_list ')' ';' port '(' interface_list ')' ';'  {EntityHeader (Just $3) (Just $8)}
              | generic '(' interface_list ')' ';'                                  {EntityHeader (Just $3) Nothing}
              |                                    port '(' interface_list ')' ';'  {EntityHeader Nothing (Just $3)}
              | {- empty -}                                                         {EntityHeader Nothing Nothing}

-- entity_declarative_part ::= { entity_declarative_item }
entity_declarative_part :: { EntityDeclarativePart }
                        : {- empty -} {[]}
                        | entity_declarative_part entity_declarative_item {$2 : $1}

-- entity_declarative_item ::=
--    subprogram_declaration
--    | subprogram_body
--    | type_declaration
--    | subtype_declaration
--    | constant_declaration
--    | signal_declaration
--    | file_declaration
--    | alias_declaration
--    | attribute_declaration
--    | attribute_specification
--    | disconnection_specification
--    | use_clause
entity_declarative_item :: { EntityDeclarativeItem }
                        : subprogram_declaration      {EntityDeclaration_SubprogramDeclaration $1}
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

-- entity_statement_part ::=
--    { entity_statement }
entity_statement_part :: {EntityStatementPart}
                      : {- empty -} {[]}
                      | entity_statement_part entity_statement {$2 : $1}

-- entity_statement ::=
--    concurrent_assertion_statement
--    | <passive>_concurrent_procedure_call
--    | <passive>_process_statement
entity_statement :: { EntityStatement }
                 : concurrent_assertion_statement  {EntityStatement_ConcurrentAssertionStatement $1}
                 | concurrent_procedure_call       {EntityStatement_ConcurrentProcedureCall $1}
                 | process_statement               {EntityStatement_ProcessStatement $1}

-- architecture_body ::=
--    architecture identifier of <entity>_name is
--       architecture_declarative_part
--    begin
--       architecture_statement_part
--    end [ <architecture>_simple_name ] ;
architecture_body :: { ArchitectureBody }
                  : architecture identifier of name is architecture_declarative_part begin architecture_statement_part end identifier ';' {ArchitectureBody $2 $4 $6 $8 (Just $10)}
                  | architecture identifier of name is architecture_declarative_part begin architecture_statement_part end ';'            {ArchitectureBody $2 $4 $6 $8 Nothing}

architecture_declarative_part :: { ArchitectureDeclarativePart }
                              : {- empty -} {[]}
                              | architecture_declarative_part block_declarative_item {$2 : $1}

block_declarative_item :: { BlockDeclarativeItem }
                       : subprogram_declaration       {BlockDeclarativeItem_SubprogramDeclaration $1}
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

architecture_statement_part :: { ArchitectureStatementPart }
                            : {- empty -} {[]}
                            | architecture_statement_part concurrent_statement {$2 : $1}

configuration_declaration :: { ConfigurationDeclaration }
                          : configuration identifier of identifier is configuration_declarative_part block_configuration end identifier ';'   {ConfigurationDeclaration $2 $4 $6 $7 (Just $9)}
                          | configuration identifier of identifier is configuration_declarative_part block_configuration end ';'              {ConfigurationDeclaration $2 $4 $6 $7 Nothing}

configuration_declarative_part :: { ConfigurationDeclarativePart }
                               : {- empty -} {[]}
                               | configuration_declarative_part configuration_declarative_item {$2 : $1}

configuration_declarative_item :: { ConfigurationDeclarativeItem }
                               : use_clause              {ConfigurationDeclarativeItem_UseClause $1}
                               | attribute_specification {ConfigurationDeclarativeItem_AttributeSpecification $1}

-- block_configuration ::=
--    for block_specification
--       { use_clause }
--       { configuration_item }
--    end for ;
-- block_specification ::=
--    <architecture>_name
--    | <block_statement>_label
--    | <generate_statement>_label [ ( index_specification ) ]
-- NOTE: Labels will be caught by 'name' parser
block_configuration :: { BlockConfiguration }
                    : for identifier '(' index_specification ')'  use_clause_list configuration_item_list end for ';' {BlockConfiguration (BlockSpecification_Generate $2 $4) $6 $7}
                    | for name                                    use_clause_list configuration_item_list end for ';' {BlockConfiguration (BlockSpecification_Name $2) $3 $4}

use_clause_list :: { [UseClause] }
                : {- empty -}                {[]}
                | use_clause_list use_clause {$2 : $1}

configuration_item_list :: { [ConfigurationItem] }
                        : {- empty -}                                {[]}
                        | configuration_item_list configuration_item {$2 : $1}

-- ?? static_expression
index_specification :: { IndexSpecification }
                    : discrete_range   {IndexSpecification_DiscreteRange $1}
                    | expression       {IndexSpecification_Expression $1}

configuration_item :: { ConfigurationItem }
                   : block_configuration     {ConfigurationItem_BlockConfiguration $1}
                   | component_configuration {ConfigurationItem_ComponentConfiguration $1}

component_configuration :: { ComponentConfiguration }
                        : for component_specification use binding_indication ';' block_configuration end for ';'  {ComponentConfiguration $2 (Just $4) (Just $6)}
                        | for component_specification use binding_indication ';' end for ';'                      {ComponentConfiguration $2 (Just $4) Nothing}
                        | for component_specification block_configuration end for ';'                             {ComponentConfiguration $2 Nothing (Just $3)}
                        | for component_specification end for ';'                                                 {ComponentConfiguration $2 Nothing Nothing}

component_specification :: { ComponentSpecification }
                        : instantiation_list ':' name {ComponentSpecification $1 $3}

subprogram_declaration :: { SubprogramDeclaration }
                       : subprogram_specification ';' {SubprogramDeclaration $1}

subprogram_specification :: { SubprogramSpecification }
                         : procedure designator '(' formal_parameter_list ')'                   {ProcedureDeclaration $2 (Just $4)}
                         | procedure designator                                                 {ProcedureDeclaration $2 Nothing}
                         | function designator '(' formal_parameter_list ')' return type_mark   {FunctionDeclaration $2 (Just $4) $7}
                         | function designator return type_mark                                 {FunctionDeclaration $2 Nothing $4}

designator :: { Designator }
           : identifier {Designator_Identifier $1}
           | '='        {Designator_Operator Tokens.Equal}
           | '/='       {Designator_Operator Tokens.Inequality}
           | '<'        {Designator_Operator Tokens.LessThan}
           | '<='       {Designator_Operator Tokens.SignAssign}
           | '>'        {Designator_Operator Tokens.GreaterThan}
           | '>='       {Designator_Operator Tokens.GreaterThanOrEqual}
           | '+'        {Designator_Operator Tokens.Plus}
           | '-'        {Designator_Operator Tokens.Hyphen}
           | '&'        {Designator_Operator Tokens.Ampersand}
           | '*'        {Designator_Operator Tokens.Star}
           | '/'        {Designator_Operator Tokens.Slash}
           | '**'       {Designator_Operator Tokens.DoubleStar}
           | and        {Designator_Keyword Tokens.And}
           | or         {Designator_Keyword Tokens.Or}
           | nand       {Designator_Keyword Tokens.Nand}
           | nor        {Designator_Keyword Tokens.Nor}
           | xor        {Designator_Keyword Tokens.Xor}
           | mod        {Designator_Keyword Tokens.Mod}
           | rem        {Designator_Keyword Tokens.Rem}
           | abs        {Designator_Keyword Tokens.Abs}
           | not        {Designator_Keyword Tokens.Not}

-- ?? parameter_interface_list
formal_parameter_list :: { InterfaceList }
                      : interface_list {$1} -- ?? Is this even needed

subprogram_body :: { SubprogramBody }
                : subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end designator ';'  {SubprogramBody $1 $3 $5 (Just $7)}
                | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end ';'             {SubprogramBody $1 $3 $5 Nothing}

subprogram_declarative_part :: { SubprogramDeclarativePart }
                            : {- empty -}                                              {[]}
                            | subprogram_declarative_part subprogram_declarative_item  {$2 : $1}

subprogram_declarative_item :: { SubprogramDeclarativeItem }
                            : subprogram_declaration  {SubprogramDeclarativeItem_SubprogramDeclaration $1}
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

subprogram_statement_part :: { SubprogramStatementPart }
                          : {- empty -}                                    {[]}
                          | subprogram_statement_part sequential_statement {$2 : $1}

package_declaration :: { PackageDeclaration }
                    : package identifier is package_declarative_part end identifier ';'   {PackageDeclaration $2 $4 (Just $6)}
                    | package identifier is package_declarative_part end ';'              {PackageDeclaration $2 $4 Nothing}

package_declarative_part :: { PackageDeclarativePart }
                         : {- empty -}                                        {[]}
                         | package_declarative_part package_declarative_item  {$2 : $1}

package_declarative_item :: { PackageDeclarativeItem }
                         : subprogram_declaration        {PackageDeclarativeItem_SubprogramDeclaration $1}
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

package_body :: { PackageBody }
             : package body identifier is package_body_declarative_part end identifier ';'   {PackageBody $3 $5 (Just $7)}
             | package body identifier is package_body_declarative_part end ';'              {PackageBody $3 $5 Nothing}

package_body_declarative_part :: { PackageBodyDeclarativePart }
                              : {- empty -}                                                  {[]}
                              | package_body_declarative_part package_body_declarative_item  {$2 : $1}

package_body_declarative_item :: { PackageBodyDeclarativeItem }
                              : subprogram_declaration                           {PackageBodyDeclarativeItem_SubprogramDeclaration $1}
                              | subprogram_body                                  {PackageBodyDeclarativeItem_SubprogramBody $1}
                              | type_declaration                                 {PackageBodyDeclarativeItem_TypeDeclaration $1}
                              | subtype_declaration                              {PackageBodyDeclarativeItem_SubtypeDeclaration $1}
                              | constant_declaration                             {PackageBodyDeclarativeItem_ConstantDeclaration $1}
                              | file_declaration                                 {PackageBodyDeclarativeItem_FileDeclaration $1}
                              | alias_declaration                                {PackageBodyDeclarativeItem_AliasDeclaration $1}
                              | use_clause                                       {PackageBodyDeclarativeItem_UseClause $1}

scalar_type_definition :: { ScalarTypeDefinition }
                       : '(' enumeration_list ')'                                                     {EnumerationTypeDefinition $2}
                       | range_constraint                                                             {UniversalTypeDefinition $1}
                       | range_constraint units identifier secondary_unit_declaration_list end units  {PhysicalTypeDefinition $1 $3 $4}

range_constraint :: { Range }
                 : range range_definition {$2}

range_definition :: { Range }
                 : attribute_name                              {RangeAttributeName $1}
                 | simple_expression to simple_expression      {RangeExpression $1 To $3}
                 | simple_expression downto simple_expression  {RangeExpression $1 Downto $3}

enumeration_list :: { [EnumerationLiteral] }
                 : enumeration_literal                      {[$1]}
                 | enumeration_list ',' enumeration_literal {$3 : $1}

enumeration_literal :: { EnumerationLiteral }
                    : identifier {EnumerationLiteral_Identifier $1}
                    | char       {EnumerationLiteral_Char $1}

secondary_unit_declaration_list :: { [SecondaryUnitDeclaration] }
                                : {- empty -}                                                {[]}
                                | secondary_unit_declaration_list secondary_unit_declaration {$2 : $1}

secondary_unit_declaration :: { SecondaryUnitDeclaration }
                           : identifier '=' physical_literal ';' {SecondaryUnitDeclaration $1 $3}

physical_literal :: { PhysicalLiteral }
                 : abstract_literal identifier  {PhysicalLiteral (Just $1) $2}
                 | identifier                   {PhysicalLiteral Nothing $1}

composite_type_definition :: { CompositeTypeDefinition }
                          : array_type_definition                       {Composite_ArrayTypeDefinition $1}
                          | record element_declaration_list end record  {RecordTypeDefinition $2}

array_type_definition :: { ArrayTypeDefinition }
                      : array '(' index_subtype_definition_list ')' of subtype_indication {UnconstrainedArrayTypeDefinition $3 $6}
                      | array '(' discrete_range_list ')' of subtype_indication           {ConstrainedArrayTypeDefinition $3 $6}

index_subtype_definition_list :: { [String] }
                              : index_subtype_definition                               {[$1]}
                              | index_subtype_definition_list index_subtype_definition {$2 : $1}

index_subtype_definition :: { String }
                         : identifier range '<>' {$1}

discrete_range_list :: { [DiscreteRange] }
                    : discrete_range                     {[$1]}
                    | discrete_range_list discrete_range {$2 : $1}

discrete_range :: { DiscreteRange }
               : subtype_indication {DiscreteRange_SubtypeIndication $1}
               | range_definition   {DiscreteRange_Range $1}

element_declaration_list :: { [ElementDeclaration] }
                         : element_declaration                          {[$1]}
                         | element_declaration_list element_declaration {$2 : $1}

element_declaration :: { ElementDeclaration }
                    : identifier_list ':' subtype_indication {ElementDeclaration $1 $3}

identifier_list :: { [String] }
                : identifier                       {[$1]}
                | identifier_list ',' identifier   {$3 : $1}

access_type_definition :: { SubtypeIndication }
                       : access subtype_indication {$2}

type_declaration :: { TypeDeclaration }
                 : type identifier is type_definition {FullTypeDeclaration $2 $4}
                 | type identifier ';'                {IncompleteTypeDefinition $2}

file_type_definition :: { Name }
                     : file of type_mark {$3}

type_mark :: { Name }
          : name {$1}

declaration :: { Declaration }
            : type_declaration            {Declaration_Type $1}
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

type_definition :: { TypeDefinition }
                : scalar_type_definition     {TypeDefinition_Scalar $1}
                | composite_type_definition  {TypeDefinition_Composite $1}
                | access_type_definition     {TypeDefinition_Access $1}
                | file_type_definition       {TypeDefinition_File $1}

subtype_declaration :: { SubtypeDeclaration }
                    : subtype identifier is subtype_indication ';' {SubtypeDeclaration $2 $4}

subtype_indication :: { SubtypeIndication }
                   : name type_mark constraint  {SubtypeIndication (Just $1) $2 (Just $3)}
                   | name type_mark             {SubtypeIndication (Just $1) $2 Nothing}
                   | type_mark constraint       {SubtypeIndication Nothing $1 (Just $2)}
                   | type_mark                  {SubtypeIndication Nothing $1 Nothing}

constraint :: { Constraint }
           : range_constraint {Constraint_Range $1}
           | index_constraint {Constraint_Index $1}

index_constraint :: { [DiscreteRange] }
                 : '(' index_constraint_list ')' {$2}

index_constraint_list :: { [DiscreteRange] }
                      : discrete_range                            {[$1]}
                      | index_constraint_list ',' discrete_range  {$3 : $1}

object_declaration :: { ObjectDeclaration }
                   : constant_declaration {ObjectDeclaration_Constant $1}
                   | signal_declaration   {ObjectDeclaration_Signal $1}
                   | variable_declaration {ObjectDeclaration_Variable $1}

constant_declaration :: { ConstantDeclaration }
                     : constant identifier_list ':' subtype_indication ':=' expression ';'   {ConstantDeclaration $2 $4 (Just $6)}
                     | constant identifier_list ':' subtype_indication ';'                   {ConstantDeclaration $2 $4 Nothing}

signal_declaration :: { SignalDeclaration }
                   : signal identifier_list ':' subtype_indication signal_kind ':=' expression ';' {SignalDeclaration $2 $4 (Just $5) (Just $7)}
                   | signal identifier_list ':' subtype_indication signal_kind ';'                 {SignalDeclaration $2 $4 (Just $5) Nothing}
                   | signal identifier_list ':' subtype_indication ':=' expression ';'             {SignalDeclaration $2 $4 Nothing (Just $6)}
                   | signal identifier_list ':' subtype_indication ';'                             {SignalDeclaration $2 $4 Nothing Nothing}

signal_kind :: { SignalKind }
            : register {Register}
            | bus {Bus}

variable_declaration :: { VariableDeclaration }
                     : variable identifier_list ':' subtype_indication ':=' expression ';'   {VariableDeclaration $2 $4 (Just $6)}
                     | variable identifier_list ':' subtype_indication ';'                   {VariableDeclaration $2 $4 Nothing}

file_declaration :: { FileDeclaration }
                 : file identifier ':' subtype_indication is mode file_logical_name ';'   {FileDeclaration $2 $4 (Just $6) $7}
                 | file identifier ':' subtype_indication is file_logical_name ';'        {FileDeclaration $2 $4 Nothing $6}

-- ??  string_expression
file_logical_name :: { Expression }
                  : expression {$1}

interface_declaration :: { InterfaceDeclaration }
                      : constant identifier_list ':' in     subtype_indication      ':=' expression   {InterfaceDeclaration (Just Constant)        $2 (Just In)   $5 (Just $7)}
                      | constant identifier_list ':' in     subtype_indication                        {InterfaceDeclaration (Just Constant)        $2 (Just In)   $5 Nothing}
                      | constant identifier_list ':'        subtype_indication      ':=' expression   {InterfaceDeclaration (Just Constant)        $2 Nothing     $4 (Just $6)}
                      | constant identifier_list ':'        subtype_indication                        {InterfaceDeclaration (Just Constant)        $2 Nothing     $4 Nothing}
                      | signal   identifier_list ':' mode   subtype_indication bus  ':=' expression   {InterfaceDeclaration (Just GuardedSignal)   $2 (Just $4)   $5 (Just $8)}
                      | signal   identifier_list ':' mode   subtype_indication bus                    {InterfaceDeclaration (Just GuardedSignal)   $2 (Just $4)   $5 Nothing}
                      | signal   identifier_list ':' mode   subtype_indication      ':=' expression   {InterfaceDeclaration (Just Signal)          $2 (Just $4)   $5 (Just $7)}
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

mode :: { Mode }
     : in      {In}
     | out     {Out}
     | inout   {Inout}
     | buffer  {Buffer}
     | linkage {Linkage}

interface_list :: { InterfaceList }
               : interface_element                    {[$1]}
               | interface_list ';' interface_element {$3 : $1}

interface_element :: { InterfaceDeclaration }
                  : interface_declaration {$1}

association_list :: { AssociationList }
                 : association_element                      {[$1]}
                 | association_list ',' association_element {$3 : $1}

association_element :: { AssociationElement }
                    : formal_part '=>' actual_part {AssociationElement (Just $1) $3}
                    |                  actual_part {AssociationElement Nothing $1}

formal_part :: { FormalPart }
            : formal_designator              {FormalPart_Designator $1}
            | name '(' formal_designator ')' {FormalPart_Function $1 $3}

formal_designator :: { Name }
                  : name {$1}

actual_part :: { ActualPart }
            : actual_designator              {ActualPart_Designator $1}
            | name '(' actual_designator ')' {ActualPart_Function $1 $3}

actual_designator :: { ActualDesignator }
                  : expression   {ActualDesignator_Expression $1}
                  | name         {ActualDesignator_Name $1}
                  | open         {ActualDesignator_Open}

alias_declaration :: { AliasDeclaration }
                  : alias identifier ':' subtype_indication is name ';' {AliasDeclaration $2 $4 $6}

attribute_declaration :: { AttributeDeclaration }
                      : attribute identifier ':' type_mark ';' {AttributeDeclaration $2 $4}

component_declaration :: { ComponentDeclaration }
                      : component identifier generic '(' interface_list ')' ';'  port '(' interface_list ')' ';'  end component ';' {ComponentDeclaration $2 (Just $5) (Just $10)}
                      | component identifier generic '(' interface_list ')' ';'                                   end component ';' {ComponentDeclaration $2 (Just $5) Nothing}
                      | component identifier                                     port '(' interface_list ')' ';'  end component ';' {ComponentDeclaration $2 Nothing (Just $5)}
                      | component identifier                                                                      end component ';' {ComponentDeclaration $2 Nothing Nothing}

attribute_specification :: { AttributeSpecification }
                        : attribute attribute_designator of entity_name_list ':' entity_class is expression ';' {AttributeSpecification $2 $4 $6 $8}

entity_name_list :: { AttributeSpecificationEntityNameList }
                 : entity_designator_list {AttributeSpecificationEntityName_List $1}
                 | others                 {AttributeSpecificationEntityName_Others}
                 | all                    {AttributeSpecificationEntityName_All}

entity_designator_list :: { [EntityDesignator] }
                       : entity_designator                              {[$1]}
                       | entity_designator_list ',' entity_designator   {$3 : $1}

entity_designator :: { EntityDesignator }
                  : simple_name     {EntityDesignator_Name $1}
                  | operator_symbol {EntityDesignator_Operator $1}

entity_class :: { EntityClass }
             : entity         {EntityClass_Entity}
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

configuration_specification :: { ConfigurationSpecification }
                            : for component_specification use binding_indication ';' {ConfigurationSpecification $2 $4}

instantiation_list :: { InstantiationList }
                   : label_list  {InstantiationList_Label $1}
                   | others      {InstantiationList_Others}
                   | all         {InstantiationList_All}

label_list :: { [String] }
           : identifier_list {$1}

binding_indication :: { BindingIndication }
                   : entity_aspect generic_map_aspect port_map_aspect   {BindingIndication $1 (Just $2) (Just $3)}
                   | entity_aspect generic_map_aspect                   {BindingIndication $1 (Just $2) Nothing}
                   | entity_aspect                    port_map_aspect   {BindingIndication $1 Nothing (Just $2)}
                   | entity_aspect                                      {BindingIndication $1 Nothing Nothing}

entity_aspect :: { EntityAspect }
              : entity name '(' identifier ')'  {EntityAspect_Entity $2 (Just $4)}
              | entity name                     {EntityAspect_Entity $2 Nothing}
              | configuration name              {EntityAspect_Configuration $2}
              | open                            {EntityAspect_Open}

generic_map_aspect :: { AssociationList }
                   : generic map '(' association_list ')' {$4}

port_map_aspect :: { AssociationList }
                : port map '(' association_list ')' {$4}

disconnection_specification :: { DisconnectionSpecification }
                            : disconnect signal_list ':' type_mark after expression ';' {DisconnectionSpecification $2 $4 $6}

signal_list :: { GuardedSignalList }
            : name_list {GuardedSignal_List $1}
            | others    {GuardedSignal_Others}
            | all       {GuardedSignal_All}

name_list :: { [Name] }
          : name                 {[$1]}
          | name_list ',' name   {$3 : $1}

name :: { Name }
     : simple_name      {Name_Simple $1}
     | operator_symbol  {Name_Operator $1}
     | selected_name    {Name_Selected $1}
     | indexed_name     {Name_Indexed $1}
     | slice_name       {Name_Slice $1}
     | attribute_name   {Name_Attribute $1}

-- prefix ::=
--    name
--    | function_call
-- NOTE: One case of function_call is caught by 'name' parser
prefix :: { Prefix }
       : name           {Prefix_Name $1}
       | function_call  {Prefix_Function $1}

simple_name :: { String }
            : identifier {$1}

operator_symbol :: { Tokens.OperatorType }
                : '=>'  {Tokens.Arrow}
                | '**'  {Tokens.DoubleStar}
                | ':='  {Tokens.VarAssign}
                | '/='  {Tokens.Inequality}
                | '>='  {Tokens.GreaterThanOrEqual}
                | '<='  {Tokens.SignAssign}
                | '<>'  {Tokens.Box}
                | '&'   {Tokens.Ampersand}
                | '\''  {Tokens.Apostrophe}
                | '('   {Tokens.LeftParen}
                | ')'   {Tokens.RightParen}
                | '*'   {Tokens.Star}
                | '+'   {Tokens.Plus}
                | ','   {Tokens.Comma}
                | '-'   {Tokens.Hyphen}
                | '.'   {Tokens.Period}
                | '/'   {Tokens.Slash}
                | ':'   {Tokens.Colon}
                | ';'   {Tokens.Semicolon}
                | '<'   {Tokens.LessThan}
                | '='   {Tokens.Equal}
                | '>'   {Tokens.GreaterThan}
                | '|'   {Tokens.Bar}

selected_name :: { SelectedName }
              : prefix '.' suffix {SelectedName $1 $3}

suffix :: { Suffix }
       : simple_name       {Suffix_Name $1}
       | char              {Suffix_Char $1}
       | operator_symbol   {Suffix_Operator $1}
       | all               {Suffix_All}

indexed_name :: { IndexedName }
             : prefix '(' expression_list ')' {IndexedName $1 $3}

expression_list :: { [Expression] }
                : expression                       {[$1]}
                | expression_list ',' expression   {$3 : $1}

slice_name :: { SliceName }
           : prefix '(' discrete_range ')' {SliceName $1 $3}

attribute_name :: { AttributeName }
               : prefix '\'' attribute_designator '(' expression ')' {AttributeName $1 $3 (Just $5)}
               | prefix '\'' attribute_designator                    {AttributeName $1 $3 Nothing}

attribute_designator :: { String }
                     : simple_name {$1}

expression :: { Expression }
           : relation         {Expression_Relation $1}
           | and_expression   {Expression_And $1}
           | or_expression    {Expression_Or $1}
           | xor_expression   {Expression_Xor $1}
           | nand_expression  {Expression_Nand $1}
           | nor_expression   {Expression_Nor $1}

and_expression :: { AndExpression }
               : relation and and_expression {AndExpression $1 $3}
               | relation and relation       {AndRelation $1 $3}

or_expression :: { OrExpression }
              : relation or or_expression   {OrExpression $1 $3}
              | relation or relation         {OrRelation $1 $3}

xor_expression :: { XorExpression }
               : relation xor xor_expression {XorExpression $1 $3}
               | relation xor relation       {XorRelation $1 $3}

nand_expression :: { NandExpression }
                : relation nand relation {NandExpression $1 $3}

nor_expression :: { NorExpression }
               : relation nor relation {NorExpression $1 $3}

relation :: { Relation }
         : simple_expression relational_operator simple_expression   {Relation_Compare $1 $2 $3}
         | simple_expression                                         {Relation_Term $1}

simple_expression :: { SimpleExpression }
                  : '+' term adding_operation_list {SimpleExpression Positive $2 $3}
                  | '-' term adding_operation_list {SimpleExpression Negative $2 $3}
                  |     term adding_operation_list {SimpleExpression Positive $1 $2}

relational_operator :: { RelationalOperator }
                    : '='  {Relation_Equals}
                    | '/=' {Relation_NotEquals}
                    | '<'  {Relation_LessThan}
                    | '<=' {Relation_LessThanOrEqual}
                    | '>'  {Relation_GreaterThan}
                    | '>=' {Relation_GreaterThanOrEqual}

adding_operation_list :: { [AddingOperation] }
                      : {- empty -}                            {[]}
                      | adding_operation_list adding_operation {$2 : $1}

adding_operation :: { AddingOperation }
                 : adding_operator term {AddingOperation $1 $2}

adding_operator :: { AddingOperator }
                : '+' {Add}
                | '-' {Minus}
                | '&' {Concat}

term :: { Term }
     : factor multiplying_operation_list {Term $1 $2}

multiplying_operation_list :: { [MultiplyingOperation] }
                           : {- empty -}                                      {[]}
                           | multiplying_operation_list multiplying_operation {$2 : $1}

multiplying_operation :: { MultiplyingOperation }
                      : multiplying_operator factor {MultiplyingOperation $1 $2}

multiplying_operator :: { MultiplyingOperator }
                     : '*' {Multiply}
                     | '/' {Divide}
                     | mod {Mod}
                     | rem {Rem}

factor :: { Factor }
       : primary '**' primary {Factor_Pow $1 $3}
       | primary              {Factor_Value $1}
       | abs primary          {Factor_Abs $2}
       | not primary          {Factor_Not $2}

-- primary ::=
--    name
--    | literal
--    | aggregate
--    | function_call
--    | qualified_expression
--    | type_conversion
--    | allocator
--    | ( expression )
-- NOTE: One case of function_call is caught by 'name' parser
primary :: { Primary }
        : name                   {Primary_Name $1}
        | literal                {Primary_Literal $1}
        | aggregate              {Primary_Aggregate $1}
        | function_call          {Primary_FunctionCall $1}
        | qualified_expression   {Primary_QualifiedExpression $1}
        | type_conversion        {Primary_TypeConversion $1}
        | allocator              {Primary_Allocator $1}
        | '(' expression ')'     {Primary_Expression $2}

-- Second part may never be triggered because higher level section has 'name' on its own
-- function_call ::=
--    <function>_name [ ( actual_parameter_part ) ]
-- NOTE: Only matches full case of this
function_call :: { FunctionCall }
              : name '(' actual_parameter_part ')' {FunctionCall $1 $3}

literal :: { Literal }
        : numeric_literal     {Literal_Numeric $1}
        | enumeration_literal {Literal_Enumeration $1}
        | string_literal      {Literal_String $1}
        | bit_string_literal  {Literal_BitStr $1}
        | null                {Literal_Null}

numeric_literal :: { NumericLiteral }
                : abstract_literal {NumericLiteral_Abstract $1}
                | physical_literal {NumericLiteral_Physical $1}

abstract_literal :: { AbstractLiteral }
                 : integer {UniversalInteger $1}
                 | real    {UniversalReal $1}

string_literal :: { String }
               : str {$1}

bit_string_literal :: { BitStrLiteral }
                   : bitstr {(\(Tokens.Literal (Tokens.BitStr base value)) -> BitStrLiteral base value) $1}

aggregate :: { Aggregate }
          : '(' element_association_list ')' {$2}

element_association_list :: { [ElementAssociation] }
                         : element_association                                {[$1]}
                         | element_association_list ',' element_association   {$3 : $1}

element_association :: { ElementAssociation }
                    : choices '=>'  expression {ElementAssociation (Just $1) $3}
                    |               expression {ElementAssociation Nothing $1}

choices :: { [Choice] }
        : choice              {[$1]}
        | choices '|' choice  {$3 : $1}

choice :: { Choice }
       : simple_expression {Choice_Expression $1}
       | discrete_range    {Choice_DiscreteRange $1}
       | simple_name       {Choice_ElementName $1}
       | others            {Choice_Others}

actual_parameter_part :: { AssociationList }
                      : association_list {$1}

qualified_expression :: { QualifiedExpression }
                     : type_mark '\'' '(' expression ')' {QualifiedExpression_Expression $1 $4}
                     | type_mark '\'' aggregate          {QualifiedExpression_Aggregate $1 $3}

type_conversion :: { TypeConversion }
                : type_mark '(' expression ')' {TypeConversion $1 $3}

allocator :: { Allocator }
          : new subtype_indication     {Allocator_Subtype $2}
          | new qualified_expression   {Allocator_Expression $2}

sequence_of_statements :: { [SequentialStatement] }
                       : {- empty -}                                 {[]}
                       | sequence_of_statements sequential_statement {$2 : $1}

sequential_statement :: { SequentialStatement }
                     : wait_statement                 {SequentialStatement_Wait $1}
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

wait_statement :: { WaitStatement }
               : wait   on sensitivity_list  until condition   for expression ';'   {WaitStatement (Just $3)   (Just $5)   (Just $7)}
               | wait   on sensitivity_list  until condition                  ';'   {WaitStatement (Just $3)   (Just $5)   Nothing}
               | wait   on sensitivity_list                    for expression ';'   {WaitStatement (Just $3)   Nothing     (Just $5)}
               | wait   on sensitivity_list                                   ';'   {WaitStatement (Just $3)   Nothing     Nothing}
               | wait                        until condition   for expression ';'   {WaitStatement Nothing     (Just $3)   (Just $5)}
               | wait                        until condition                  ';'   {WaitStatement Nothing     (Just $3)   Nothing}
               | wait                                          for expression ';'   {WaitStatement Nothing     Nothing     (Just $3)}
               | wait                                                         ';'   {WaitStatement Nothing     Nothing     Nothing}

sensitivity_list :: { [Name] }
                 : name_list {$1}

assertion_statement :: { AssertionStatement }
                    : assert condition report expression severity expression  ';' {AssertionStatement $2 (Just $4)   (Just $6)}
                    | assert condition report expression                      ';' {AssertionStatement $2 (Just $4)   Nothing}
                    | assert condition                   severity expression  ';' {AssertionStatement $2 Nothing     (Just $4)}
                    | assert condition                                        ';' {AssertionStatement $2 Nothing     Nothing}

signal_assignment_statement :: { SignalAssignmentStatement }
                            : target '<=' transport   waveform ';' {SignalAssignmentStatement $1 SignalAssignmentTransport $4}
                            | target '<='             waveform ';' {SignalAssignmentStatement $1 SignalAssignmentNormal $3}

target :: { Target }
       : name        {Target_Name $1}
       | aggregate   {Target_Aggregate $1}

waveform :: { Waveform }
         : waveform_element               {[$1]}
         | waveform ',' waveform_element  {$3 : $1}

waveform_element :: { WaveformElement }
                 : expression after expression  {Waveform_Expression $1 (Just $3)}
                 | expression                   {Waveform_Expression $1 Nothing}
                 | null       after expression  {Waveform_Null (Just $3)}
                 | null                         {Waveform_Null Nothing}

variable_assignment_statement :: { VariableAssignmentStatement }
                              : target ':=' expression ';' {VariableAssignmentStatement $1 $3}

procedure_call_statement :: { ProcedureCallStatement }
                         : name '(' actual_parameter_part ')'  ';' {ProcedureCallStatement $1 (Just $3)}
                         | name                                ';' {ProcedureCallStatement $1 Nothing}

if_statement :: { IfStatement }
             : if condition then sequence_of_statements elsif_statement_list else sequence_of_statements end if ';' {IfStatement $2 $4 $5 (Just $7)}
             | if condition then sequence_of_statements elsif_statement_list                             end if ';' {IfStatement $2 $4 $5 Nothing}

elsif_statement_list :: { [ElsifStatement] }
                     : {- empty -}                          {[]}
                     | elsif_statement_list elsif_statement {$2 : $1}

elsif_statement :: { ElsifStatement }
                : elsif condition then sequence_of_statements {ElsifStatement $2 $4}

case_statement :: { CaseStatement }
               : case expression is case_statement_alternative_list end case ';' {CaseStatement $2 $4}

case_statement_alternative_list :: { [CaseStatementAlternative] }
                                : case_statement_alternative                                 {[$1]}
                                | case_statement_alternative_list case_statement_alternative {$2 : $1}

case_statement_alternative :: { CaseStatementAlternative }
                           : when choices '=>' sequence_of_statements {CaseStatementAlternative $2 $4}

loop_statement :: { LoopStatement }
               : identifier ':'  iteration_scheme  loop sequence_of_statements end loop identifier ';' {LoopStatement (Just ($1,$8)) (Just $3) $5}
               |                 iteration_scheme  loop sequence_of_statements end loop            ';' {LoopStatement Nothing (Just $1) $3}
               | identifier ':'                    loop sequence_of_statements end loop identifier ';' {LoopStatement (Just ($1,$7)) Nothing $4}
               |                                   loop sequence_of_statements end loop            ';' {LoopStatement Nothing Nothing $2}

iteration_scheme :: { IterationScheme }
                 : while condition                    {IterationScheme_While $2}
                 | for identifier in discrete_range   {IterationScheme_For $2 $4}

next_statement :: { NextStatement }
               : next identifier when condition ';' {NextStatement (Just $2) (Just $4)}
               | next identifier                ';' {NextStatement (Just $2) Nothing}
               | next            when condition ';' {NextStatement Nothing (Just $3)}
               | next                           ';' {NextStatement Nothing Nothing}

exit_statement :: { ExitStatement }
               : exit identifier when condition ';' {ExitStatement (Just $2) (Just $4)}
               | exit identifier                ';' {ExitStatement (Just $2) Nothing}
               | exit            when condition ';' {ExitStatement Nothing (Just $3)}
               | exit                           ';' {ExitStatement Nothing Nothing}

return_statement :: { ReturnStatement }
                 : return expression   ';' {ReturnStatement (Just $2)}
                 | return              ';' {ReturnStatement Nothing}

concurrent_statement :: { ConcurrentStatement }
                     : block_statement                         {Concurrent_BlockStatement $1}
                     | process_statement                       {Concurrent_ProcessStatement $1}
                     | concurrent_procedure_call               {Concurrent_ProcedureCall $1}
                     | concurrent_assertion_statement          {Concurrent_AssertionStatement $1}
                     | concurrent_signal_assignment_statement  {Concurrent_SignalAssignmentStatement $1}
                     | component_instantiation_statement       {Concurrent_ComponentInstantiationStatement $1}
                     | generate_statement                      {Concurrent_GenerateStatement $1}

block_statement :: { BlockStatement }
                : identifier ':' block '(' expression ')'   block_header block_declarative_part begin block_statement_part end block identifier ';' {BlockStatement $1 (Just $5) $7 $8 $10 (Just $13)}
                | identifier ':' block '(' expression ')'   block_header block_declarative_part begin block_statement_part end block            ';' {BlockStatement $1 (Just $5) $7 $8 $10 Nothing}
                | identifier ':' block                      block_header block_declarative_part begin block_statement_part end block identifier ';' {BlockStatement $1 Nothing $4 $5 $7 (Just $10)}
                | identifier ':' block                      block_header block_declarative_part begin block_statement_part end block            ';' {BlockStatement $1 Nothing $4 $5 $7 Nothing}

block_header :: { BlockHeader }
             : block_header_generic block_header_port {BlockHeader (Just $1) (Just $2)}
             | block_header_generic                   {BlockHeader (Just $1) Nothing}
             |                      block_header_port {BlockHeader Nothing (Just $1)}
             | {- empty -}                            {BlockHeader Nothing Nothing}

block_header_generic :: { BlockHeader_Generic }
                     : generic '(' interface_list ')' ';' generic_map_aspect ';'  {BlockHeader_Generic $3 (Just $6)}
                     | generic '(' interface_list ')' ';'                         {BlockHeader_Generic $3 Nothing}

block_header_port :: { BlockHeader_Port }
                  : port '(' interface_list ')' ';' port_map_aspect ';' {BlockHeader_Port $3 (Just $6)}
                  | port '(' interface_list ')' ';'                     {BlockHeader_Port $3 Nothing}

block_declarative_part :: { BlockDeclarativePart }
                       : {- empty -}                                    {[]}
                       | block_declarative_part block_declarative_item  {$2 : $1}

block_statement_part :: { BlockStatementPart }
                     : {- empty -}                                {[]}
                     | block_statement_part concurrent_statement  {$2 : $1}

process_statement :: { ProcessStatement }
                  : identifier ':'  process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process identifier  ';'   {ProcessStatement (Just ($1,$12)) (Just $5) $7 $9}
                  |                 process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process             ';'   {ProcessStatement Nothing (Just $3) $5 $7}
                  | identifier ':'  process                          process_declarative_part begin process_statement_part end process identifier  ';'   {ProcessStatement (Just ($1,$9)) Nothing $4 $6}
                  |                 process                          process_declarative_part begin process_statement_part end process             ';'   {ProcessStatement Nothing Nothing $2 $4}

process_declarative_part :: { ProcessDeclarativePart }
                         : {- empty -}                                        {[]}
                         | process_declarative_part process_declarative_item  {$2 : $1}

process_statement_part :: { ProcessStatementPart }
                       : {- empty -}                                 {[]}
                       | process_statement_part sequential_statement {$2 : $1}

process_declarative_item :: { ProcessDeclarativeItem }
                         : subprogram_declaration  {ProcessDeclarative_SubprogramDeclaration $1}
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

concurrent_procedure_call :: { ConcurrentProcedureCall }
                          : identifier ':'   procedure_call_statement {ConcurrentProcedureCall (Just $1) $3}
                          |                  procedure_call_statement {ConcurrentProcedureCall Nothing $1}

concurrent_assertion_statement :: { ConcurrentAssertionStatement }
                               : identifier ':' assertion_statement {ConcurrentAssertionStatement (Just $1) $3}
                               |                assertion_statement {ConcurrentAssertionStatement Nothing $1}

concurrent_signal_assignment_statement :: { ConcurrentSignalAssignmentStatement }
                                       : identifier ':'  target '<=' options conditional_waveforms ';'                     {ConditionalSignalAssignment (Just $1) $3 $5 $6}
                                       |                 target '<=' options conditional_waveforms ';'                     {ConditionalSignalAssignment Nothing $1 $3 $4}
                                       | identifier ':'  with expression select target '<=' options selected_waveforms ';' {SelectedSignalAssignment (Just $1) $4 $6 $8 $9}
                                       |                 with expression select target '<=' options selected_waveforms ';' {SelectedSignalAssignment Nothing $2 $4 $6 $7}

options :: { SignalAssignmentOptions }
        : guarded transport   {SignalAssignmentOptions SignalAssignment_Guarded SignalAssignment_Transport}
        | guarded             {SignalAssignmentOptions SignalAssignment_Guarded SignalAssignment_NonTransport}
        |         transport   {SignalAssignmentOptions SignalAssignment_NonGuarded SignalAssignment_Transport}
        | {- empty -}         {SignalAssignmentOptions SignalAssignment_NonGuarded SignalAssignment_NonTransport}

conditional_waveforms :: { ConditionalWaveforms }
                      : conditional_waveform_pairs waveform {ConditionalWaveforms $1 $2}

conditional_waveform_pairs :: { [ConditionalWaveformPair] }
                           : conditional_waveform_pair                              {[$1]}
                           | conditional_waveform_pairs conditional_waveform_pair   {$2 : $1}

conditional_waveform_pair :: { ConditionalWaveformPair }
                          : waveform when condition else {ConditionalWaveformPair $1 $3}

selected_waveforms :: { [SelectedWaveformPair] }
                   : selected_waveform_pair                          {[$1]}
                   | selected_waveforms ',' selected_waveform_pair   {$3 : $1}

selected_waveform_pair :: { SelectedWaveformPair }
                       : waveform when choices {SelectedWaveformPair $1 $3}

component_instantiation_statement :: { ComponentInstantiationStatement }
                                  : identifier ':' name generic_map_aspect port_map_aspect   ';' {ComponentInstantiationStatement $1 $3 (Just $4) (Just $5)}
                                  | identifier ':' name generic_map_aspect                   ';' {ComponentInstantiationStatement $1 $3 (Just $4) Nothing}
                                  | identifier ':' name                    port_map_aspect   ';' {ComponentInstantiationStatement $1 $3 Nothing (Just $4)}
                                  | identifier ':' name                                      ';' {ComponentInstantiationStatement $1 $3 Nothing Nothing}

generate_statement :: { GenerateStatement }
                   : identifier ':' generation_scheme generate concurrent_statement_list end generate identifier  ';' {GenerateStatement $1 $3 $5 (Just $8)}
                   | identifier ':' generation_scheme generate concurrent_statement_list end generate             ';' {GenerateStatement $1 $3 $5 Nothing}

generation_scheme :: { GenerationScheme }
                  : for identifier ':' discrete_range {GenerationScheme_For $2 $4}
                  | if condition                      {GenerationScheme_If $2}

-- Boolean expression
condition :: { Expression }
          : expression {$1}

concurrent_statement_list :: { [ConcurrentStatement] }
                          : {- empty -}                                    {[]}
                          | concurrent_statement_list concurrent_statement {$2 : $1}

use_clause :: { UseClause }
           : use selected_name_list ';' {UseClause $2}

selected_name_list :: { [SelectedName] }
                   : selected_name                          {[$1]}
                   | selected_name_list ',' selected_name   {$3 : $1}

design_unit_list :: { [DesignUnit] }
                 : design_unit                  {[$1]}
                 | design_unit_list design_unit {$2 : $1}

design_unit :: { DesignUnit }
            : context_clause library_unit {DesignUnit $1 $2}

library_unit :: { LibraryUnit }
             : primary_unit   {Library_PrimaryUnit $1}
             | secondary_unit {Library_SecondaryUnit $1}

primary_unit :: { PrimaryUnit }
             : entity_declaration         {PrimaryUnit_EntityDeclaration $1}
             | configuration_declaration  {PrimaryUnit_ConfigurationDeclaration $1}
             | package_declaration        {PrimaryUnit_PackageDeclaration $1}

secondary_unit :: { SecondaryUnit }
               : architecture_body  {Secondary_ArchitectureBody $1}
               | package_body       {Secondary_PackageBody $1}

library_clause :: { LibraryClause }
               : library identifier_list ';' {$2}

context_clause :: { ContextClause }
               : {- empty -}                 {[]}
               | context_clause context_item {$2 : $1}

context_item :: { ContextItem }
             : library_clause {Context_LibraryClause $1}
             | use_clause {Context_UseClause $1}

{
v1987 :: Alex DesignFile
}
