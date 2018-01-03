{
module Parser.Parser where
import Parser.Happy.Types
import Parser.Alex.Types (Alex)
import Parser.Alex.Functions (alexError)
import qualified Parser.Lexer as Lex
}

%name 1987
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
   '\'            {Operator Apostrophe}
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
   decimal        {Literal Decimal $$}
   bitstr         {Literal BitStr $$ $$}
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

port_clause : port '(' post_list ')' ';' {PortClause $3}

generic_list : -- ?? generic_interface_list

port_list : -- ?? port_interface_list

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
formal_parameter_list : interface_list -- ?? Is this even needed

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

package_body_declarative_part : {- empty -} 
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
                          | record element_declaration_list end record

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
                | identifier_list ',' identifier   {$2 : $1}

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

--?? file_logical_name : string_expression {$1}

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
               | interface_list ';' interface_element {$2 : $1}

interface_element : interface_declaration {$1}
