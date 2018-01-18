{
module Parser.Parser where

import Parser.Happy.Types
import Parser.Happy.Functions
import Parser.Happy.ParseFunctions
import Parser.Alex.Monad (Alex)
import qualified Parser.Lexer as Lex
import qualified Parser.TokenTypes as Tokens
import Parser.PositionWrapper
}

%name v1987
%lexer{Lex.lexer}{PosnWrapper { getPos = _, unPos = Tokens.EOF }}
%tokentype{Tokens.WrappedToken}
%monad{Alex}
%error{parseError}

%token
   abs            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Abs }}
   access         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Access }}
   after          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.After }}
   alias          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Alias }}
   all            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.All }}
   and            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.And }}
   architecture   {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Architecture }}
   array          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Array }}
   assert         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Assert }}
   attribute      {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Attribute }}
   begin          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Begin }}
   block          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Block }}
   body           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Body }}
   buffer         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Buffer }}
   bus            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Bus }}
   case           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Case }}
   component      {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Component }}
   configuration  {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Configuration }}
   constant       {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Constant }}
   disconnect     {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Disconnect }}
   downto         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Downto }}
   else           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Else }}
   elsif          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Elsif }}
   end            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.End }}
   entity         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Entity }}
   exit           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Exit }}
   file           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.File }}
   for            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.For }}
   function       {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Function }}
   generate       {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Generate }}
   generic        {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Generic }}
   guarded        {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Guarded }}
   if             {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.If }}
   in             {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.In }}
   inout          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Inout }}
   is             {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Is }}
   label          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Label }}
   library        {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Library }}
   linkage        {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Linkage }}
   loop           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Loop }}
   map            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Map }}
   mod            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Mod }}
   nand           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Nand }}
   new            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.New }}
   next           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Next }}
   nor            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Nor }}
   not            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Not }}
   null           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Null }}
   of             {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Of }}
   on             {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.On }}
   open           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Open }}
   or             {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Or }}
   others         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Others }}
   out            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Out }}
   package        {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Package }}
   port           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Port }}
   procedure      {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Procedure }}
   process        {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Process }}
   range          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Range }}
   record         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Record }}
   register       {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Register }}
   rem            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Rem }}
   report         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Report }}
   return         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Return }}
   select         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Select }}
   severity       {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Severity }}
   signal         {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Signal }}
   subtype        {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Subtype }}
   then           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Then }}
   to             {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.To }}
   transport      {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Transport }}
   type           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Type }}
   units          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Units }}
   until          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Until }}
   use            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Use }}
   variable       {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Variable }}
   wait           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Wait }}
   when           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.When }}
   while          {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.While }}
   with           {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.With }}
   xor            {PosnWrapper { getPos = _, unPos = Tokens.Keyword Tokens.Xor }}
   '=>'           {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Arrow }}
   '**'           {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.DoubleStar }}
   ':='           {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.VarAssign }}
   '/='           {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Inequality }}
   '>='           {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.GreaterThanOrEqual }}
   '<='           {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.SignAssign }}
   '<>'           {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Box }}
   '&'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Ampersand }}
   '\''           {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Apostrophe }}
   '('            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.LeftParen }}
   ')'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.RightParen }}
   '*'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Star }}
   '+'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Plus }}
   ','            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Comma }}
   '-'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Hyphen }}
   '.'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Period }}
   '/'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Slash }}
   ':'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Colon }}
   ';'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Semicolon }}
   '<'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.LessThan }}
   '='            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Equal }}
   '>'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.GreaterThan }}
   '|'            {PosnWrapper { getPos = _, unPos = Tokens.Operator Tokens.Bar }}
   identifier     {PosnWrapper { getPos = _, unPos = Tokens.Identifier _ }}
   integer        {PosnWrapper { getPos = _, unPos = Tokens.Literal (Tokens.Univ_Int _) }}
   real           {PosnWrapper { getPos = _, unPos = Tokens.Literal (Tokens.Univ_Real _) }}
   bitstr         {PosnWrapper { getPos = _, unPos = Tokens.Literal (Tokens.BitStr _ _) }}
   str            {PosnWrapper { getPos = _, unPos = Tokens.Literal (Tokens.Str _) }}
   char           {PosnWrapper { getPos = _, unPos = Tokens.Literal (Tokens.Character _) }}

%%

------------------------------------------
-- Design Units
------------------------------------------

design_file :: { DesignFile }
            : design_unit_list {DesignFile $1}

design_unit_list :: { [WrappedDesignUnit] }
                 :                  context_clause library_unit {[newDesignUnit $1 $2]}
                 | design_unit_list context_clause library_unit {(newDesignUnit $2 $3) : $1}

library_unit :: { WrappedLibraryUnit }
             : entity identifier is generic '(' interface_list ')' ';'  port '(' interface_list ')' ';'  entity_declarative_part begin entity_statement_part   end identifier ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader (Just $ newGenericClause $4 $6)  (Just $ newPortClause $9 $11)) $14  (Just $16)  (Just $ newSimpleName $18)}
             | entity identifier is generic '(' interface_list ')' ';'                                   entity_declarative_part begin entity_statement_part   end identifier ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader (Just $ newGenericClause $4 $6)  Nothing)                       $9   (Just $11)  (Just $ newSimpleName $13)}
             | entity identifier is                                     port '(' interface_list ')' ';'  entity_declarative_part begin entity_statement_part   end identifier ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader Nothing                          (Just $ newPortClause $4 $6))  $9   (Just $11)  (Just $ newSimpleName $13)}
             | entity identifier is                                                                      entity_declarative_part begin entity_statement_part   end identifier ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader Nothing                          Nothing)                       $4   (Just $6)   (Just $ newSimpleName $8)}
             | entity identifier is generic '(' interface_list ')' ';'  port '(' interface_list ')' ';'  entity_declarative_part                               end identifier ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader (Just $ newGenericClause $4 $6)  (Just $ newPortClause $9 $11)) $14  Nothing     (Just $ newSimpleName $16)}
             | entity identifier is generic '(' interface_list ')' ';'                                   entity_declarative_part                               end identifier ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader (Just $ newGenericClause $4 $6)  Nothing)                       $9   Nothing     (Just $ newSimpleName $11)}
             | entity identifier is                                     port '(' interface_list ')' ';'  entity_declarative_part                               end identifier ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader Nothing                          (Just $ newPortClause $4 $6))  $9   Nothing     (Just $ newSimpleName $11)}
             | entity identifier is                                                                      entity_declarative_part                               end identifier ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader Nothing                          Nothing)                       $4   Nothing     (Just $ newSimpleName $6)}
             | entity identifier is generic '(' interface_list ')' ';'  port '(' interface_list ')' ';'  entity_declarative_part begin entity_statement_part   end            ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader (Just $ newGenericClause $4 $6)  (Just $ newPortClause $9 $11)) $14  (Just $16)  Nothing}
             | entity identifier is generic '(' interface_list ')' ';'                                   entity_declarative_part begin entity_statement_part   end            ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader (Just $ newGenericClause $4 $6)  Nothing)                       $9   (Just $11)  Nothing}
             | entity identifier is                                     port '(' interface_list ')' ';'  entity_declarative_part begin entity_statement_part   end            ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader Nothing                          (Just $ newPortClause $4 $6))  $9   (Just $11)  Nothing}
             | entity identifier is                                                                      entity_declarative_part begin entity_statement_part   end            ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader Nothing                          Nothing)                       $4   (Just $6)   Nothing}
             | entity identifier is generic '(' interface_list ')' ';'  port '(' interface_list ')' ';'  entity_declarative_part                               end            ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader (Just $ newGenericClause $4 $6)  (Just $ newPortClause $9 $11)) $14  Nothing     Nothing}
             | entity identifier is generic '(' interface_list ')' ';'                                   entity_declarative_part                               end            ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader (Just $ newGenericClause $4 $6)  Nothing)                       $9   Nothing     Nothing}
             | entity identifier is                                     port '(' interface_list ')' ';'  entity_declarative_part                               end            ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader Nothing                          (Just $ newPortClause $4 $6))  $9   Nothing     Nothing}
             | entity identifier is                                                                      entity_declarative_part                               end            ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_EntityDeclaration $ newEntityDeclaration $1 (newSimpleName $2) (EntityHeader Nothing                          Nothing)                       $4   Nothing     Nothing}
             | configuration identifier of name is configuration_declarative_part block_configuration end identifier                                                          ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_ConfigurationDeclaration $ newConfigurationDeclaration $1 (newSimpleName $2) $4 $6 $7 (Just $ newSimpleName $9)}
             | configuration identifier of name is configuration_declarative_part block_configuration end                                                                     ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_ConfigurationDeclaration $ newConfigurationDeclaration $1 (newSimpleName $2) $4 $6 $7 Nothing}
             | package identifier is package_declarative_part end identifier                                                                                                  ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_PackageDeclaration $ newPackageDeclaration $1 (newSimpleName $2) $4 (Just $ newSimpleName $6)}
             | package identifier is package_declarative_part end                                                                                                             ';' {raisePosition Library_PrimaryUnit $ raisePosition PrimaryUnit_PackageDeclaration $ newPackageDeclaration $1 (newSimpleName $2) $4 Nothing}
             | architecture identifier of name is architecture_declarative_part begin architecture_statement_part end identifier                                              ';' {raisePosition Library_SecondaryUnit $ raisePosition Secondary_ArchitectureBody $ newArchitectureBody $1 (newSimpleName $2) $4 $6 $8 (Just $ newSimpleName $10)}
             | architecture identifier of name is architecture_declarative_part begin architecture_statement_part end                                                         ';' {raisePosition Library_SecondaryUnit $ raisePosition Secondary_ArchitectureBody $ newArchitectureBody $1 (newSimpleName $2) $4 $6 $8 Nothing}
             | package body identifier is package_body_declarative_part end identifier                                                                                        ';' {raisePosition Library_SecondaryUnit $ raisePosition Secondary_PackageBody $ newPackageBody $1 (newSimpleName $3) $5 (Just $ newSimpleName $7)}
             | package body identifier is package_body_declarative_part end                                                                                                   ';' {raisePosition Library_SecondaryUnit $ raisePosition Secondary_PackageBody $ newPackageBody $1 (newSimpleName $3) $5 Nothing}

context_clause :: { ContextClause }
               : {- empty -}                                   {[]}
               | context_clause library identifier_list  ';'   {(raisePosition Context_LibraryClause $ newLibraryUnit $2 $3)  : $1}
               | context_clause use selected_name_list   ';'   {(raisePosition Context_UseClause $ newUseClause $2 $3)        : $1}

selected_name_list :: { [WrappedSelectedName] }
                   :                         name                            '.' identifier  {[newSelectedName (newPrefix_Name $1) (newSuffix_Identifier  $3)]}
                   |                         name                            '.' char        {[newSelectedName (newPrefix_Name $1) (newSuffix_Char        $3)]}
                   |                         name                            '.' str         {[newSelectedName (newPrefix_Name $1) (newSuffix_Operator    $3)]}
                   |                         name                            '.' all         {[newSelectedName (newPrefix_Name $1) (newSuffix_All         $3)]}
                   |                         name '(' association_list ')'   '.' identifier  {[newSelectedName (newPrefix_Function $1 $3) (newSuffix_Identifier $6)]}
                   |                         name '(' association_list ')'   '.' char        {[newSelectedName (newPrefix_Function $1 $3) (newSuffix_Char       $6)]}
                   |                         name '(' association_list ')'   '.' str         {[newSelectedName (newPrefix_Function $1 $3) (newSuffix_Operator   $6)]}
                   |                         name '(' association_list ')'   '.' all         {[newSelectedName (newPrefix_Function $1 $3) (newSuffix_All        $6)]}
                   | selected_name_list ','  name                            '.' identifier  {(newSelectedName (newPrefix_Name $3) (newSuffix_Identifier  $5))       : $1}
                   | selected_name_list ','  name                            '.' char        {(newSelectedName (newPrefix_Name $3) (newSuffix_Char        $5))       : $1}
                   | selected_name_list ','  name                            '.' str         {(newSelectedName (newPrefix_Name $3) (newSuffix_Operator    $5))       : $1}
                   | selected_name_list ','  name                            '.' all         {(newSelectedName (newPrefix_Name $3) (newSuffix_All         $5))       : $1}
                   | selected_name_list ','  name '(' association_list ')'   '.' identifier  {(newSelectedName (newPrefix_Function $3 $5) (newSuffix_Identifier $8)) : $1}
                   | selected_name_list ','  name '(' association_list ')'   '.' char        {(newSelectedName (newPrefix_Function $3 $5) (newSuffix_Char       $8)) : $1}
                   | selected_name_list ','  name '(' association_list ')'   '.' str         {(newSelectedName (newPrefix_Function $3 $5) (newSuffix_Operator   $8)) : $1}
                   | selected_name_list ','  name '(' association_list ')'   '.' all         {(newSelectedName (newPrefix_Function $3 $5) (newSuffix_All        $8)) : $1}

------------------------------------------
-- Design Entities and Configurations
------------------------------------------

-- entity_declaration ::=
--    entity identifier is
--       entity_header
--       entity_declarative_part
--  [ begin
--       entity_statement_part ]
--    end [ /entity/_simple_name ] ;
-- entity_header ::=
--    [ /formal/_generic_clause ]
--    [ /formal/_port_clause ]
-- generic_clause ::=
--    __generic__ ( generic_list ) ;
-- generic_list ::= /generic/_interface_list
-- port_clause ::=
--    __port__ ( port_list ) ;
-- port_list ::= /port/_interface_list

-- entity_declarative_part ::=
--    { entity_declarative_item }
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
-- subprogram_declaration ::=
--    subprogram_specification ;
-- subprogram_body ::=
--    subprogram_specification __is__
--       subprogram_declarative_part
--    __begin__
--       subprogram_statement_part
--    __end__ [ designator ] ;
entity_declarative_item :: { WrappedEntityDeclarativeItem }
                        : subprogram_specification ';'                                                                                {raisePosition EntityDeclaration_SubprogramDeclaration $ newSubprogramDeclaration $1}
                        | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end identifier ';'  {raisePosition EntityDeclaration_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Identifier $7)}
                        | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end str        ';'  {raisePosition EntityDeclaration_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Operator $7)}
                        | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end            ';'  {raisePosition EntityDeclaration_SubprogramBody $ newSubprogramBody $1 $3 $5 Nothing}
                        | type identifier is '(' enumeration_list ')' ';'                                                             {raisePosition EntityDeclaration_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newEnumerationType $4 $5}
                        | type identifier is range range_definition ';'                                                               {raisePosition EntityDeclaration_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUniversalType $4 $5}
                        | type identifier is range range_definition units identifier secondary_unit_declaration_list end units ';'    {raisePosition EntityDeclaration_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newPhysicalType $4 $5 (newSimpleName $7) $8}
                        | type identifier is array '(' index_subtype_definition_list ')'   of subtype_indication ';'                  {raisePosition EntityDeclaration_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUnconstrainedArrayType  $4 $6 $9}
                        | type identifier is array '(' discrete_range_list ')'             of subtype_indication ';'                  {raisePosition EntityDeclaration_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newConstrainedArrayType    $4 $6 $9}
                        | type identifier is record element_declaration_list end record ';'                                           {raisePosition EntityDeclaration_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newRecordType $4 $5}
                        | type identifier is access subtype_indication ';'                                                            {raisePosition EntityDeclaration_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newAccessType $4 $5}
                        | type identifier is file of name ';'                                                                         {raisePosition EntityDeclaration_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newFileType $4 $6}
                        | type identifier ';'                                                                                         {raisePosition EntityDeclaration_TypeDeclaration $ newIncompleteTypeDefinition $1 $ newSimpleName $2}
                        | subtype identifier is subtype_indication ';'                                                                {raisePosition EntityDeclaration_SubtypeDeclaration $ newSubtypeDeclaration $1 (newSimpleName $2) $4}
                        | constant identifier_list ':' subtype_indication ':=' expression ';'                                         {raisePosition EntityDeclaration_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 (Just $6)}
                        | constant identifier_list ':' subtype_indication ';'                                                         {raisePosition EntityDeclaration_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 Nothing}
                        | signal identifier_list ':' subtype_indication register   ':=' expression   ';'                              {raisePosition EntityDeclaration_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Register $5)  (Just $7)}
                        | signal identifier_list ':' subtype_indication bus        ':=' expression   ';'                              {raisePosition EntityDeclaration_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Bus $5)       (Just $7)}
                        | signal identifier_list ':' subtype_indication            ':=' expression   ';'                              {raisePosition EntityDeclaration_SignalDeclaration $ newSignalDeclaration $1 $2 $4 Nothing                            (Just $6)}
                        | signal identifier_list ':' subtype_indication register                     ';'                              {raisePosition EntityDeclaration_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Register $5)  Nothing}
                        | signal identifier_list ':' subtype_indication bus                          ';'                              {raisePosition EntityDeclaration_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Bus $5)       Nothing}
                        | signal identifier_list ':' subtype_indication                              ';'                              {raisePosition EntityDeclaration_SignalDeclaration $ newSignalDeclaration $1 $2 $4 Nothing                            Nothing}
                        | file identifier ':' subtype_indication is mode     expression ';'                                           {raisePosition EntityDeclaration_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 (Just $6)  $7}
                        | file identifier ':' subtype_indication is          expression ';'                                           {raisePosition EntityDeclaration_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 Nothing    $6}
                        | alias identifier ':' subtype_indication is name ';'                                                         {raisePosition EntityDeclaration_AliasDeclaration $ newAliasDeclaration $1 (newSimpleName $2) $4 $6}
                        | attribute identifier ':' name ';'                                                                           {raisePosition EntityDeclaration_AttributeDeclaration $ newAttributeDeclaration $1 (newSimpleName $2) $4}
                        | attribute identifier of entity_designator_list  ':' entity_class is expression ';'                          {raisePosition EntityDeclaration_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_List $4) $6 $8}
                        | attribute identifier of others                  ':' entity_class is expression ';'                          {raisePosition EntityDeclaration_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_Others $4) $6 $8}
                        | attribute identifier of all                     ':' entity_class is expression ';'                          {raisePosition EntityDeclaration_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_All $4) $6 $8}
                        | disconnect signal_list ':' name after expression ';'                                                        {raisePosition EntityDeclaration_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_List $2) $4 $6}
                        | disconnect others      ':' name after expression ';'                                                        {raisePosition EntityDeclaration_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_Others $2) $4 $6}
                        | disconnect all         ':' name after expression ';'                                                        {raisePosition EntityDeclaration_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_All $2) $4 $6}
                        | use selected_name_list ';'                                                                                  {raisePosition EntityDeclaration_UseClause $ newUseClause $1 $2}

-- entity_statement_part ::=
--    { entity_statement }
entity_statement_part :: { EntityStatementPart }
                      : {- empty -} {[]}
                      | entity_statement_part entity_statement {$2 : $1}

-- entity_statement ::=
--    concurrent_assertion_statement
--    | /passive/_concurrent_procedure_call
--    | /passive/_process_statement
entity_statement :: { WrappedEntityStatement }
                 : identifier ':'   assert expression report expression severity expression  ';' {raisePosition EntityStatement_ConcurrentAssertionStatement $ newConcurrentAssertionStatement (Just $ newSimpleName $1)   $ newAssertionStatement $3 $4 (Just $6)   (Just $8)}
                 | identifier ':'   assert expression                   severity expression  ';' {raisePosition EntityStatement_ConcurrentAssertionStatement $ newConcurrentAssertionStatement (Just $ newSimpleName $1)   $ newAssertionStatement $3 $4 Nothing     (Just $6)}
                 | identifier ':'   assert expression report expression                      ';' {raisePosition EntityStatement_ConcurrentAssertionStatement $ newConcurrentAssertionStatement (Just $ newSimpleName $1)   $ newAssertionStatement $3 $4 (Just $6)   Nothing}
                 | identifier ':'   assert expression                                        ';' {raisePosition EntityStatement_ConcurrentAssertionStatement $ newConcurrentAssertionStatement (Just $ newSimpleName $1)   $ newAssertionStatement $3 $4 Nothing     Nothing}
                 |                  assert expression report expression severity expression  ';' {raisePosition EntityStatement_ConcurrentAssertionStatement $ newConcurrentAssertionStatement Nothing                     $ newAssertionStatement $1 $2 (Just $4)   (Just $6)}
                 |                  assert expression                   severity expression  ';' {raisePosition EntityStatement_ConcurrentAssertionStatement $ newConcurrentAssertionStatement Nothing                     $ newAssertionStatement $1 $2 Nothing     (Just $4)}
                 |                  assert expression report expression                      ';' {raisePosition EntityStatement_ConcurrentAssertionStatement $ newConcurrentAssertionStatement Nothing                     $ newAssertionStatement $1 $2 (Just $4)   Nothing}
                 |                  assert expression                                        ';' {raisePosition EntityStatement_ConcurrentAssertionStatement $ newConcurrentAssertionStatement Nothing                     $ newAssertionStatement $1 $2 Nothing     Nothing}
                 | identifier ':'   name '(' association_list ')' ';' {raisePosition EntityStatement_ConcurrentProcedureCall $ newConcurrentProcedureCall (Just $ newSimpleName $1) $ newProcedureCallStatement $3 (Just $5)}
                 | identifier ':'   name                          ';' {raisePosition EntityStatement_ConcurrentProcedureCall $ newConcurrentProcedureCall (Just $ newSimpleName $1) $ newProcedureCallStatement $3 Nothing}
                 |                  name '(' association_list ')' ';' {raisePosition EntityStatement_ConcurrentProcedureCall $ newConcurrentProcedureCall Nothing                   $ newProcedureCallStatement $1 (Just $3)}
                 |                  name                          ';' {raisePosition EntityStatement_ConcurrentProcedureCall $ newConcurrentProcedureCall Nothing                   $ newProcedureCallStatement $1 Nothing}
                 | identifier ':'   process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process identifier  ';' {raisePosition EntityStatement_ProcessStatement $ newProcessStatement (Left $ newSimpleName $1)   (Just $5)   $7 $9 (Just $ newSimpleName $12)}
                 | identifier ':'   process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process             ';' {raisePosition EntityStatement_ProcessStatement $ newProcessStatement (Left $ newSimpleName $1)   (Just $5)   $7 $9 Nothing}
                 | identifier ':'   process                          process_declarative_part begin process_statement_part end process identifier  ';' {raisePosition EntityStatement_ProcessStatement $ newProcessStatement (Left $ newSimpleName $1)   Nothing     $4 $6 (Just $ newSimpleName $9)}
                 | identifier ':'   process                          process_declarative_part begin process_statement_part end process             ';' {raisePosition EntityStatement_ProcessStatement $ newProcessStatement (Left $ newSimpleName $1)   Nothing     $4 $6 Nothing}
                 |                  process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process identifier  ';' {raisePosition EntityStatement_ProcessStatement $ newProcessStatement (Right $1)                  (Just $3)   $5 $7 (Just $ newSimpleName $10)}
                 |                  process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process             ';' {raisePosition EntityStatement_ProcessStatement $ newProcessStatement (Right $1)                  (Just $3)   $5 $7 Nothing}
                 |                  process                          process_declarative_part begin process_statement_part end process identifier  ';' {raisePosition EntityStatement_ProcessStatement $ newProcessStatement (Right $1)                  Nothing     $2 $4 (Just $ newSimpleName $7)}
                 |                  process                          process_declarative_part begin process_statement_part end process             ';' {raisePosition EntityStatement_ProcessStatement $ newProcessStatement (Right $1)                  Nothing     $2 $4 Nothing}

-- architecture_body ::=
--    __architecture__ identifier __of__ /entity/_name __is__
--       architecture_declarative_part
--    __begin__
--       architecture_statement_part
--    __end__ [ /architecture/_simple_name ] ;

-- architecture_declarative_part ::=
--    { block_declarative_item }
architecture_declarative_part :: { ArchitectureDeclarativePart }
                              : {- empty -} {[]}
                              | architecture_declarative_part block_declarative_item {$2 : $1}

-- block_declarative_item ::=
--    subprogram_declaration
--    | subprogram_body
--    | type_declaration
--    | subtype_declaration
--    | constant_declaration
--    | signal_declaration
--    | file_declaration
--    | alias_declaration
--    | component_declaration
--    | attribute_declaration
--    | attribute_specification
--    | configuration_specification
--    | disconnection_specification
--    | use_clause
block_declarative_item :: { WrappedBlockDeclarativeItem }
                       : subprogram_specification ';'                                                                                                                      {raisePosition BlockDeclarativeItem_SubprogramDeclaration $ newSubprogramDeclaration $1}
                       | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end identifier ';'                                        {raisePosition BlockDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Identifier $7)}
                       | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end str        ';'                                        {raisePosition BlockDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Operator $7)}
                       | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end            ';'                                        {raisePosition BlockDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 Nothing}
                       | type identifier is '(' enumeration_list ')' ';'                                                                                                   {raisePosition BlockDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newEnumerationType $4 $5}
                       | type identifier is range range_definition ';'                                                                                                     {raisePosition BlockDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUniversalType $4 $5}
                       | type identifier is range range_definition units identifier secondary_unit_declaration_list end units ';'                                          {raisePosition BlockDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newPhysicalType $4 $5 (newSimpleName $7) $8}
                       | type identifier is array '(' index_subtype_definition_list ')'   of subtype_indication ';'                                                        {raisePosition BlockDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUnconstrainedArrayType  $4 $6 $9}
                       | type identifier is array '(' discrete_range_list ')'             of subtype_indication ';'                                                        {raisePosition BlockDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newConstrainedArrayType    $4 $6 $9}
                       | type identifier is record element_declaration_list end record ';'                                                                                 {raisePosition BlockDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newRecordType $4 $5}
                       | type identifier is access subtype_indication ';'                                                                                                  {raisePosition BlockDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newAccessType $4 $5}
                       | type identifier is file of name ';'                                                                                                               {raisePosition BlockDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newFileType $4 $6}
                       | type identifier ';'                                                                                                                               {raisePosition BlockDeclarativeItem_TypeDeclaration $ newIncompleteTypeDefinition $1 $ newSimpleName $2}
                       | subtype identifier is subtype_indication ';'                                                                                                      {raisePosition BlockDeclarativeItem_SubtypeDeclaration $ newSubtypeDeclaration $1 (newSimpleName $2) $4}
                       | constant identifier_list ':' subtype_indication ':=' expression ';'                                                                               {raisePosition BlockDeclarativeItem_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 (Just $6)}
                       | constant identifier_list ':' subtype_indication ';'                                                                                               {raisePosition BlockDeclarativeItem_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 Nothing}
                       | signal identifier_list ':' subtype_indication register   ':=' expression   ';'                                                                    {raisePosition BlockDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Register $5) (Just $7)}
                       | signal identifier_list ':' subtype_indication bus        ':=' expression   ';'                                                                    {raisePosition BlockDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Bus $5)      (Just $7)}
                       | signal identifier_list ':' subtype_indication            ':=' expression   ';'                                                                    {raisePosition BlockDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 Nothing                           (Just $6)}
                       | signal identifier_list ':' subtype_indication register                     ';'                                                                    {raisePosition BlockDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Register $5) Nothing}
                       | signal identifier_list ':' subtype_indication bus                          ';'                                                                    {raisePosition BlockDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Bus $5)      Nothing}
                       | signal identifier_list ':' subtype_indication                              ';'                                                                    {raisePosition BlockDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 Nothing                           Nothing}
                       | file identifier ':' subtype_indication is mode     expression ';'                                                                                 {raisePosition BlockDeclarativeItem_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 (Just $6)  $7}
                       | file identifier ':' subtype_indication is          expression ';'                                                                                 {raisePosition BlockDeclarativeItem_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 Nothing    $6}
                       | alias identifier ':' subtype_indication is name ';'                                                                                               {raisePosition BlockDeclarativeItem_AliasDeclaration $ newAliasDeclaration $1 (newSimpleName $2) $4 $6}
                       | component identifier generic '(' interface_list ')' ';' port '(' interface_list ')' ';'  end component ';'                                        {raisePosition BlockDeclarativeItem_ComponentDeclaration $ newComponentDeclaration $1 (newSimpleName $2) (Just $ newGenericClause $3 $5) (Just $ newPortClause $8 $10)}
                       | component identifier generic '(' interface_list ')' ';'                                  end component ';'                                        {raisePosition BlockDeclarativeItem_ComponentDeclaration $ newComponentDeclaration $1 (newSimpleName $2) (Just $ newGenericClause $3 $5) Nothing}
                       | component identifier                                    port '(' interface_list ')' ';'  end component ';'                                        {raisePosition BlockDeclarativeItem_ComponentDeclaration $ newComponentDeclaration $1 (newSimpleName $2) Nothing                         (Just $ newPortClause $3 $5)}
                       | component identifier                                                                     end component ';'                                        {raisePosition BlockDeclarativeItem_ComponentDeclaration $ newComponentDeclaration $1 (newSimpleName $2) Nothing                         Nothing}
                       | attribute identifier ':' name ';'                                                                                                                 {raisePosition BlockDeclarativeItem_AttributeDeclaration $ newAttributeDeclaration $1 (newSimpleName $2) $4}
                       | attribute identifier of entity_designator_list  ':' entity_class is expression ';'                                                                {raisePosition BlockDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_List $4) $6 $8}
                       | attribute identifier of others                  ':' entity_class is expression ';'                                                                {raisePosition BlockDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_Others $4) $6 $8}
                       | attribute identifier of all                     ':' entity_class is expression ';'                                                                {raisePosition BlockDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_All $4) $6 $8}
                       | for identifier_list ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)}
                       | for others          ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)}
                       | for all             ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)}
                       | for identifier_list ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing}
                       | for others          ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing}
                       | for all             ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing}
                       | for identifier_list ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)}
                       | for others          ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)}
                       | for all             ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)}
                       | for identifier_list ':' name use entity name '(' identifier ')'                                                                             ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing}
                       | for others          ':' name use entity name '(' identifier ')'                                                                             ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing}
                       | for all             ':' name use entity name '(' identifier ')'                                                                             ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing}
                       | for identifier_list ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)}
                       | for others          ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)}
                       | for all             ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)}
                       | for identifier_list ':' name use entity name                     generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing}
                       | for others          ':' name use entity name                     generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing}
                       | for all             ':' name use entity name                     generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing}
                       | for identifier_list ':' name use entity name                                                            port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)}
                       | for others          ':' name use entity name                                                            port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)}
                       | for all             ':' name use entity name                                                            port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)}
                       | for identifier_list ':' name use entity name                                                                                                ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing}
                       | for others          ':' name use entity name                                                                                                ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing}
                       | for all             ':' name use entity name                                                                                                ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing}
                       | for identifier_list ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)}
                       | for others          ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)}
                       | for all             ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)}
                       | for identifier_list ':' name use configuration name              generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing}
                       | for others          ':' name use configuration name              generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing}
                       | for all             ':' name use configuration name              generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing}
                       | for identifier_list ':' name use configuration name                                                     port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)}
                       | for others          ':' name use configuration name                                                     port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)}
                       | for all             ':' name use configuration name                                                     port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)}
                       | for identifier_list ':' name use configuration name                                                                                         ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing}
                       | for others          ':' name use configuration name                                                                                         ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing}
                       | for all             ':' name use configuration name                                                                                         ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing}
                       | for identifier_list ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)}
                       | for others          ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)}
                       | for all             ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)}
                       | for identifier_list ':' name use open                            generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing}
                       | for others          ':' name use open                            generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing}
                       | for all             ':' name use open                            generic map '(' association_list ')'                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing}
                       | for identifier_list ':' name use open                                                                   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)}
                       | for others          ':' name use open                                                                   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)}
                       | for all             ':' name use open                                                                   port map '(' association_list ')'   ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)}
                       | for identifier_list ':' name use open                                                                                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_List $2)     $4) $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing}
                       | for others          ':' name use open                                                                                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_Others $2)   $4) $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing}
                       | for all             ':' name use open                                                                                                       ';'   {raisePosition BlockDeclarativeItem_ConfigurationSpecification $ newConfigurationSpecification $1 (newComponentSpecification (newInstantiation_All $2)      $4) $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing}
                       | disconnect signal_list ':' name after expression ';'                                                                                              {raisePosition BlockDeclarativeItem_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_List $2) $4 $6}
                       | disconnect others      ':' name after expression ';'                                                                                              {raisePosition BlockDeclarativeItem_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_Others $2) $4 $6}
                       | disconnect all         ':' name after expression ';'                                                                                              {raisePosition BlockDeclarativeItem_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_All $2) $4 $6}
                       | use selected_name_list ';'                                                                                                                        {raisePosition BlockDeclarativeItem_UseClause $ newUseClause $1 $2}

-- architecture_statement_part ::=
--    { concurrent_statement }
architecture_statement_part :: { ArchitectureStatementPart }
                            : {- empty -} {[]}
                            | architecture_statement_part concurrent_statement {$2 : $1}

-- configuration_declaration ::=
--    __configuration__ identifier __of__ /entity/_name __is__
--       configuration_declarative_part
--       block_configuration
--    __end__ [ /configuration/_simple_name ] ;

-- configuration_declarative_part ::=
--    { configuration_declarative_item }
configuration_declarative_part :: { ConfigurationDeclarativePart }
                               : {- empty -} {[]}
                               | configuration_declarative_part configuration_declarative_item {$2 : $1}

-- configuration_declarative_item ::=
--    use_clause
--    | attribute_specification
configuration_declarative_item :: { WrappedConfigurationDeclarativeItem }
                               : use selected_name_list ';'                                                          {raisePosition ConfigurationDeclarativeItem_UseClause $ newUseClause $1 $2}
                               | attribute identifier of entity_designator_list  ':' entity_class is expression ';'  {raisePosition ConfigurationDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_List $4) $6 $8}
                               | attribute identifier of others                  ':' entity_class is expression ';'  {raisePosition ConfigurationDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_Others $4) $6 $8}
                               | attribute identifier of all                     ':' entity_class is expression ';'  {raisePosition ConfigurationDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_All $4) $6 $8}

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
block_configuration :: { WrappedBlockConfiguration }
                    : for identifier '(' discrete_range  ')'   use_clause_list configuration_item_list end for ';' {newBlockConfiguration $1 (newBlockSpecification_Generate (newSimpleName $2) $ raisePosition IndexSpecification_DiscreteRange $4)  $6 $7}
                    | for identifier '(' expression      ')'   use_clause_list configuration_item_list end for ';' {newBlockConfiguration $1 (newBlockSpecification_Generate (newSimpleName $2) $ raisePosition IndexSpecification_Expression $4)     $6 $7}
                    | for name                                 use_clause_list configuration_item_list end for ';' {newBlockConfiguration $1 (raisePosition BlockSpecification_Name $2)                                                               $3 $4}

use_clause_list :: { [WrappedUseClause] }
                : {- empty -}                {[]}
                | use_clause_list use selected_name_list ';' {(newUseClause $2 $3) : $1}

configuration_item_list :: { [WrappedConfigurationItem] }
                        : {- empty -}                                      {[]}
                        | configuration_item_list block_configuration      {(raisePosition ConfigurationItem_BlockConfiguration $2) : $1}
                        | configuration_item_list component_configuration  {(raisePosition ConfigurationItem_ComponentConfiguration $2) : $1}

-- component_configuration ::=
--    __for__ component_specification
--       [ __use__ binding_indication ; ]
--       [ block_configuration ]
--    __end__ __for__ ;
-- component_specification ::=
--    instantiation_list : /component/_name
--    binding_indication ::=
--       entity_aspect
--       [ generic_map_aspect ]
--       [ port_map_aspect ]
component_configuration :: { WrappedComponentConfiguration }
                        : for identifier_list ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)) (Just $22)}
                        | for others          ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)) (Just $22)}
                        | for all             ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)) (Just $22)}
                        | for identifier_list ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing)    (Just $17)}
                        | for others          ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing)    (Just $17)}
                        | for all             ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing)    (Just $17)}
                        | for identifier_list ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)) (Just $17)}
                        | for others          ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)) (Just $17)}
                        | for all             ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)) (Just $17)}
                        | for identifier_list ':' name use entity name '(' identifier ')'                                                                             ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing)    (Just $12)}
                        | for others          ':' name use entity name '(' identifier ')'                                                                             ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing)    (Just $12)}
                        | for all             ':' name use entity name '(' identifier ')'                                                                             ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing)    (Just $12)}
                        | for identifier_list ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)) (Just $19)}
                        | for others          ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)) (Just $19)}
                        | for all             ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)) (Just $19)}
                        | for identifier_list ':' name use entity name                     generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing)    (Just $14)}
                        | for others          ':' name use entity name                     generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing)    (Just $14)}
                        | for all             ':' name use entity name                     generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing)    (Just $14)}
                        | for identifier_list ':' name use entity name                                                            port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)) (Just $14)}
                        | for others          ':' name use entity name                                                            port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)) (Just $14)}
                        | for all             ':' name use entity name                                                            port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)) (Just $14)}
                        | for identifier_list ':' name use entity name                                                                                                ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing)    (Just $9)}
                        | for others          ':' name use entity name                                                                                                ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing)    (Just $9)}
                        | for all             ':' name use entity name                                                                                                ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing)    (Just $9)}
                        | for identifier_list ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)) (Just $19)}
                        | for others          ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)) (Just $19)}
                        | for all             ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)) (Just $19)}
                        | for identifier_list ':' name use configuration name              generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing)    (Just $14)}
                        | for others          ':' name use configuration name              generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing)    (Just $14)}
                        | for all             ':' name use configuration name              generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing)    (Just $14)}
                        | for identifier_list ':' name use configuration name                                                     port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)) (Just $14)}
                        | for others          ':' name use configuration name                                                     port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)) (Just $14)}
                        | for all             ':' name use configuration name                                                     port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)) (Just $14)}
                        | for identifier_list ':' name use configuration name                                                                                         ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing)    (Just $9)}
                        | for others          ':' name use configuration name                                                                                         ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing)    (Just $9)}
                        | for all             ':' name use configuration name                                                                                         ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing)    (Just $9)}
                        | for identifier_list ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)) (Just $18)}
                        | for others          ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)) (Just $18)}
                        | for all             ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)) (Just $18)}
                        | for identifier_list ':' name use open                            generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing)    (Just $13)}
                        | for others          ':' name use open                            generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing)    (Just $13)}
                        | for all             ':' name use open                            generic map '(' association_list ')'                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing)    (Just $13)}
                        | for identifier_list ':' name use open                                                                   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)) (Just $13)}
                        | for others          ':' name use open                                                                   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)) (Just $13)}
                        | for all             ':' name use open                                                                   port map '(' association_list ')'   ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)) (Just $13)}
                        | for identifier_list ':' name use open                                                                                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing)    (Just $8)}
                        | for others          ':' name use open                                                                                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing)    (Just $8)}
                        | for all             ':' name use open                                                                                                       ';'  block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing)    (Just $8)}
                        | for identifier_list ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)) Nothing}
                        | for others          ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)) Nothing}
                        | for all             ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  (Just $19)) Nothing}
                        | for identifier_list ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing)    Nothing}
                        | for others          ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing)    Nothing}
                        | for all             ':' name use entity name '(' identifier ')'  generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) (Just $14)  Nothing)    Nothing}
                        | for identifier_list ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)) Nothing}
                        | for others          ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)) Nothing}
                        | for all             ':' name use entity name '(' identifier ')'                                         port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     (Just $14)) Nothing}
                        | for identifier_list ':' name use entity name '(' identifier ')'                                                                             ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing)    Nothing}
                        | for others          ':' name use entity name '(' identifier ')'                                                                             ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing)    Nothing}
                        | for all             ':' name use entity name '(' identifier ')'                                                                             ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 $ Just $ newSimpleName $9) Nothing     Nothing)    Nothing}
                        | for identifier_list ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)) Nothing}
                        | for others          ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)) Nothing}
                        | for all             ':' name use entity name                     generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  (Just $16)) Nothing}
                        | for identifier_list ':' name use entity name                     generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing)    Nothing}
                        | for others          ':' name use entity name                     generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing)    Nothing}
                        | for all             ':' name use entity name                     generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   (Just $11)  Nothing)    Nothing}
                        | for identifier_list ':' name use entity name                                                            port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)) Nothing}
                        | for others          ':' name use entity name                                                            port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)) Nothing}
                        | for all             ':' name use entity name                                                            port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     (Just $11)) Nothing}
                        | for identifier_list ':' name use entity name                                                                                                ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing)    Nothing}
                        | for others          ':' name use entity name                                                                                                ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing)    Nothing}
                        | for all             ':' name use entity name                                                                                                ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Entity $6 $7 Nothing)                   Nothing     Nothing)    Nothing}
                        | for identifier_list ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)) Nothing}
                        | for others          ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)) Nothing}
                        | for all             ':' name use configuration name              generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  (Just $16)) Nothing}
                        | for identifier_list ':' name use configuration name              generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing)    Nothing}
                        | for others          ':' name use configuration name              generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing)    Nothing}
                        | for all             ':' name use configuration name              generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    (Just $11)  Nothing)    Nothing}
                        | for identifier_list ':' name use configuration name                                                     port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)) Nothing}
                        | for others          ':' name use configuration name                                                     port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)) Nothing}
                        | for all             ':' name use configuration name                                                     port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     (Just $11)) Nothing}
                        | for identifier_list ':' name use configuration name                                                                                         ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing)    Nothing}
                        | for others          ':' name use configuration name                                                                                         ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing)    Nothing}
                        | for all             ':' name use configuration name                                                                                         ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Configuration $6 $7)                    Nothing     Nothing)    Nothing}
                        | for identifier_list ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)) Nothing}
                        | for others          ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)) Nothing}
                        | for all             ':' name use open                            generic map '(' association_list ')'   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  (Just $15)) Nothing}
                        | for identifier_list ':' name use open                            generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing)    Nothing}
                        | for others          ':' name use open                            generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing)    Nothing}
                        | for all             ':' name use open                            generic map '(' association_list ')'                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                (Just $10)  Nothing)    Nothing}
                        | for identifier_list ':' name use open                                                                   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)) Nothing}
                        | for others          ':' name use open                                                                   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)) Nothing}
                        | for all             ':' name use open                                                                   port map '(' association_list ')'   ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     (Just $10)) Nothing}
                        | for identifier_list ':' name use open                                                                                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing)    Nothing}
                        | for others          ':' name use open                                                                                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing)    Nothing}
                        | for all             ':' name use open                                                                                                       ';'                       end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) (Just $ newBindingIndication (newEntityAspect_Open $6)                                Nothing     Nothing)    Nothing}
                        | for identifier_list ':' name                                                                                                                     block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) Nothing                                                                                                       (Just $5)}
                        | for others          ':' name                                                                                                                     block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) Nothing                                                                                                       (Just $5)}
                        | for all             ':' name                                                                                                                     block_configuration  end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) Nothing                                                                                                       (Just $5)}
                        | for identifier_list ':' name                                                                                                                                          end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_List $2)     $4) Nothing                                                                                                       Nothing}
                        | for others          ':' name                                                                                                                                          end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_Others $2)   $4) Nothing                                                                                                       Nothing}
                        | for all             ':' name                                                                                                                                          end for ';' {newComponentConfiguration $1 (newComponentSpecification (newInstantiation_All $2)      $4) Nothing                                                                                                       Nothing}

------------------------------------------
-- Subprograms and Packages
------------------------------------------

-- subprogram_specification ::=
--    __procedure__ designator [ ( formal_parameter_list ) ]
--    | __function__ designator [ ( formal_parameter_list ) ] __return__ type_mark
-- designator ::= identifier | operator_symbol
-- operator_symbol ::= string_literal
-- formal_parameter_list ::= /parameter/_interface_list
-- type_mark ::=
--    /type/_name
--    | /subtype/_name
subprogram_specification :: { WrappedSubprogramSpecification }
                         : procedure identifier '(' interface_list ')'              ';'  {newSubprogramSpecification_Procedure   $1 (newDesignator_Identifier $2) (Just $4)}
                         | procedure identifier                                     ';'  {newSubprogramSpecification_Procedure   $1 (newDesignator_Identifier $2) Nothing}
                         | procedure str        '(' interface_list ')'              ';'  {newSubprogramSpecification_Procedure   $1 (newDesignator_Operator $2)   (Just $4)}
                         | procedure str                                            ';'  {newSubprogramSpecification_Procedure   $1 (newDesignator_Operator $2)   Nothing}
                         | function  identifier '(' interface_list ')'  return name ';'  {newSubprogramSpecification_Function    $1 (newDesignator_Identifier $2) (Just $4)   $7}
                         | function  identifier                         return name ';'  {newSubprogramSpecification_Function    $1 (newDesignator_Identifier $2) Nothing     $4}
                         | function  str        '(' interface_list ')'  return name ';'  {newSubprogramSpecification_Function    $1 (newDesignator_Operator $2)   (Just $4)   $7}
                         | function  str                                return name ';'  {newSubprogramSpecification_Function    $1 (newDesignator_Operator $2)   Nothing     $4}


-- subprogram_declarative_part ::=
--    { subprogram_declarative_item }
subprogram_declarative_part :: { SubprogramDeclarativePart }
                            : {- empty -}                                              {[]}
                            | subprogram_declarative_part subprogram_declarative_item  {$2 : $1}

-- subprogram_declarative_item ::=
--    subprogram_declaration
--    | subprogram_body
--    | type_declaration
--    | subtype_declaration
--    | constant_declaration
--    | variable_declaration
--    | file_declaration
--    | alias_declaration
--    | attribute_declaration
--    | attribute_specification
--    | use_clause
subprogram_declarative_item :: { WrappedSubprogramDeclarativeItem }
                            : subprogram_specification ';'                                                                                {raisePosition SubprogramDeclarativeItem_SubprogramDeclaration $ newSubprogramDeclaration $1}
                            | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end identifier ';'  {raisePosition SubprogramDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Identifier $7)}
                            | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end str        ';'  {raisePosition SubprogramDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Operator $7)}
                            | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end            ';'  {raisePosition SubprogramDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 Nothing}
                            | type identifier is '(' enumeration_list ')' ';'                                                             {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newEnumerationType $4 $5}
                            | type identifier is range range_definition ';'                                                               {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUniversalType $4 $5}
                            | type identifier is range range_definition units identifier secondary_unit_declaration_list end units ';'    {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newPhysicalType $4 $5 (newSimpleName $7) $8}
                            | type identifier is array '(' index_subtype_definition_list ')'   of subtype_indication ';'                  {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUnconstrainedArrayType  $4 $6 $9}
                            | type identifier is array '(' discrete_range_list ')'             of subtype_indication ';'                  {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newConstrainedArrayType    $4 $6 $9}
                            | type identifier is record element_declaration_list end record ';'                                           {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newRecordType $4 $5}
                            | type identifier is access subtype_indication ';'                                                            {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newAccessType $4 $5}
                            | type identifier is file of name ';'                                                                         {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newFileType $4 $6}
                            | type identifier ';'                                                                                         {raisePosition SubprogramDeclarativeItem_TypeDeclaration $ newIncompleteTypeDefinition $1 $ newSimpleName $2}
                            | subtype identifier is subtype_indication ';'                                                                {raisePosition SubprogramDeclarativeItem_SubtypeDeclaration $ newSubtypeDeclaration $1 (newSimpleName $2) $4}
                            | constant identifier_list ':' subtype_indication ':=' expression ';'                                         {raisePosition SubprogramDeclarativeItem_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 (Just $6)}
                            | constant identifier_list ':' subtype_indication ';'                                                         {raisePosition SubprogramDeclarativeItem_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 Nothing}
                            | variable identifier_list ':' subtype_indication ':=' expression ';'                                         {raisePosition SubprogramDeclarativeItem_VariableDeclaration $ newVariableDeclaration $1 $2 $4 (Just $6)}
                            | variable identifier_list ':' subtype_indication ';'                                                         {raisePosition SubprogramDeclarativeItem_VariableDeclaration $ newVariableDeclaration $1 $2 $4 Nothing}
                            | file identifier ':' subtype_indication is mode     expression ';'                                           {raisePosition SubprogramDeclarativeItem_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 (Just $6)  $7}
                            | file identifier ':' subtype_indication is          expression ';'                                           {raisePosition SubprogramDeclarativeItem_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 Nothing    $6}
                            | alias identifier ':' subtype_indication is name ';'                                                         {raisePosition SubprogramDeclarativeItem_AliasDeclaration $ newAliasDeclaration $1 (newSimpleName $2) $4 $6}
                            | attribute identifier ':' name ';'                                                                           {raisePosition SubprogramDeclarativeItem_AttributeDeclaration $ newAttributeDeclaration $1 (newSimpleName $2) $4}
                            | attribute identifier of entity_designator_list  ':' entity_class is expression ';'                          {raisePosition SubprogramDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_List $4) $6 $8}
                            | attribute identifier of others                  ':' entity_class is expression ';'                          {raisePosition SubprogramDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_Others $4) $6 $8}
                            | attribute identifier of all                     ':' entity_class is expression ';'                          {raisePosition SubprogramDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_All $4) $6 $8}
                            | use selected_name_list ';'                                                                                  {raisePosition SubprogramDeclarativeItem_UseClause $ newUseClause $1 $2}

-- subprogram_statement_part ::=
--    { sequential_statement }
subprogram_statement_part :: { SubprogramStatementPart }
                          : {- empty -}                                    {[]}
                          | subprogram_statement_part sequential_statement {$2 : $1}

package_declarative_part :: { PackageDeclarativePart }
                         : {- empty -}                                        {[]}
                         | package_declarative_part package_declarative_item  {$2 : $1}

package_declarative_item :: { WrappedPackageDeclarativeItem }
                         : subprogram_specification ';'                                                                                {raisePosition PackageDeclarativeItem_SubprogramDeclaration $ newSubprogramDeclaration $1}
                         | type identifier is '(' enumeration_list ')' ';'                                                             {raisePosition PackageDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newEnumerationType $4 $5}
                         | type identifier is range range_definition ';'                                                               {raisePosition PackageDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUniversalType $4 $5}
                         | type identifier is range range_definition units identifier secondary_unit_declaration_list end units ';'    {raisePosition PackageDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newPhysicalType $4 $5 (newSimpleName $7) $8}
                         | type identifier is array '(' index_subtype_definition_list ')'   of subtype_indication ';'                  {raisePosition PackageDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUnconstrainedArrayType  $4 $6 $9}
                         | type identifier is array '(' discrete_range_list ')'             of subtype_indication ';'                  {raisePosition PackageDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newConstrainedArrayType    $4 $6 $9}
                         | type identifier is record element_declaration_list end record ';'                                           {raisePosition PackageDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newRecordType $4 $5}
                         | type identifier is access subtype_indication ';'                                                            {raisePosition PackageDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newAccessType $4 $5}
                         | type identifier is file of name ';'                                                                         {raisePosition PackageDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newFileType $4 $6}
                         | type identifier ';'                                                                                         {raisePosition PackageDeclarativeItem_TypeDeclaration $ newIncompleteTypeDefinition $1 $ newSimpleName $2}
                         | subtype identifier is subtype_indication ';'                                                                {raisePosition PackageDeclarativeItem_SubtypeDeclaration $ newSubtypeDeclaration $1 (newSimpleName $2) $4}
                         | constant identifier_list ':' subtype_indication ':=' expression ';'                                         {raisePosition PackageDeclarativeItem_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 (Just $6)}
                         | constant identifier_list ':' subtype_indication ';'                                                         {raisePosition PackageDeclarativeItem_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 Nothing}
                         | signal identifier_list ':' subtype_indication register   ':=' expression   ';'                              {raisePosition PackageDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Register $5)   (Just $7)}
                         | signal identifier_list ':' subtype_indication bus        ':=' expression   ';'                              {raisePosition PackageDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Bus $5)        (Just $7)}
                         | signal identifier_list ':' subtype_indication            ':=' expression   ';'                              {raisePosition PackageDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 Nothing                             (Just $6)}
                         | signal identifier_list ':' subtype_indication register                     ';'                              {raisePosition PackageDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Register $5)   Nothing}
                         | signal identifier_list ':' subtype_indication bus                          ';'                              {raisePosition PackageDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 (Just $ passPosition Bus $5)        Nothing}
                         | signal identifier_list ':' subtype_indication                              ';'                              {raisePosition PackageDeclarativeItem_SignalDeclaration $ newSignalDeclaration $1 $2 $4 Nothing                             Nothing}
                         | file identifier ':' subtype_indication is mode     expression ';'                                           {raisePosition PackageDeclarativeItem_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 (Just $6)  $7}
                         | file identifier ':' subtype_indication is          expression ';'                                           {raisePosition PackageDeclarativeItem_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 Nothing    $6}
                         | alias identifier ':' subtype_indication is name ';'                                                         {raisePosition PackageDeclarativeItem_AliasDeclaration $ newAliasDeclaration $1 (newSimpleName $2) $4 $6}
                         | component identifier generic '(' interface_list ')' ';' port '(' interface_list ')' ';'  end component ';'  {raisePosition PackageDeclarativeItem_ComponentDeclaration $ newComponentDeclaration $1 (newSimpleName $2) (Just $ newGenericClause $3 $5) (Just $ newPortClause $8 $10)}
                         | component identifier generic '(' interface_list ')' ';'                                  end component ';'  {raisePosition PackageDeclarativeItem_ComponentDeclaration $ newComponentDeclaration $1 (newSimpleName $2) (Just $ newGenericClause $3 $5) Nothing}
                         | component identifier                                    port '(' interface_list ')' ';'  end component ';'  {raisePosition PackageDeclarativeItem_ComponentDeclaration $ newComponentDeclaration $1 (newSimpleName $2) Nothing                         (Just $ newPortClause $3 $5)}
                         | component identifier                                                                     end component ';'  {raisePosition PackageDeclarativeItem_ComponentDeclaration $ newComponentDeclaration $1 (newSimpleName $2) Nothing                         Nothing}
                         | attribute identifier ':' name ';'                                                                           {raisePosition PackageDeclarativeItem_AttributeDeclaration $ newAttributeDeclaration $1 (newSimpleName $2) $4}
                         | attribute identifier of entity_designator_list  ':' entity_class is expression ';'                          {raisePosition PackageDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_List $4) $6 $8}
                         | attribute identifier of others                  ':' entity_class is expression ';'                          {raisePosition PackageDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_Others $4) $6 $8}
                         | attribute identifier of all                     ':' entity_class is expression ';'                          {raisePosition PackageDeclarativeItem_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_All $4) $6 $8}
                         | disconnect signal_list ':' name after expression ';'                                                        {raisePosition PackageDeclarativeItem_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_List $2) $4 $6}
                         | disconnect others      ':' name after expression ';'                                                        {raisePosition PackageDeclarativeItem_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_Others $2) $4 $6}
                         | disconnect all         ':' name after expression ';'                                                        {raisePosition PackageDeclarativeItem_DisconnectionSpecification $ newDisconnectionSpecification $1 (newGuardedSignal_All $2) $4 $6}
                         | use selected_name_list ';'                                                                                  {raisePosition PackageDeclarativeItem_UseClause $ newUseClause $1 $2}

package_body_declarative_part :: { PackageBodyDeclarativePart }
                              : {- empty -}                                                  {[]}
                              | package_body_declarative_part package_body_declarative_item  {$2 : $1}

package_body_declarative_item :: { WrappedPackageBodyDeclarativeItem }
                              : subprogram_specification ';'                                                                                {raisePosition PackageBodyDeclarativeItem_SubprogramDeclaration $ newSubprogramDeclaration $1}
                              | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end identifier ';'  {raisePosition PackageBodyDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Identifier $7)}
                              | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end str        ';'  {raisePosition PackageBodyDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Operator $7)}
                              | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end            ';'  {raisePosition PackageBodyDeclarativeItem_SubprogramBody $ newSubprogramBody $1 $3 $5 Nothing}
                              | type identifier is '(' enumeration_list ')' ';'                                                             {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newEnumerationType $4 $5}
                              | type identifier is range range_definition ';'                                                               {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUniversalType $4 $5}
                              | type identifier is range range_definition units identifier secondary_unit_declaration_list end units ';'    {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newPhysicalType $4 $5 (newSimpleName $7) $8}
                              | type identifier is array '(' index_subtype_definition_list ')'   of subtype_indication ';'                  {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUnconstrainedArrayType  $4 $6 $9}
                              | type identifier is array '(' discrete_range_list ')'             of subtype_indication ';'                  {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newConstrainedArrayType    $4 $6 $9}
                              | type identifier is record element_declaration_list end record ';'                                           {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newRecordType $4 $5}
                              | type identifier is access subtype_indication ';'                                                            {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newAccessType $4 $5}
                              | type identifier is file of name ';'                                                                         {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newFileType $4 $6}
                              | type identifier ';'                                                                                         {raisePosition PackageBodyDeclarativeItem_TypeDeclaration $ newIncompleteTypeDefinition $1 $ newSimpleName $2}
                              | subtype identifier is subtype_indication ';'                                                                {raisePosition PackageBodyDeclarativeItem_SubtypeDeclaration $ newSubtypeDeclaration $1 (newSimpleName $2) $4}
                              | constant identifier_list ':' subtype_indication ':=' expression ';'                                         {raisePosition PackageBodyDeclarativeItem_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 (Just $6)}
                              | constant identifier_list ':' subtype_indication ';'                                                         {raisePosition PackageBodyDeclarativeItem_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 Nothing}
                              | file identifier ':' subtype_indication is mode     expression ';'                                           {raisePosition PackageBodyDeclarativeItem_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 (Just $6)  $7}
                              | file identifier ':' subtype_indication is          expression ';'                                           {raisePosition PackageBodyDeclarativeItem_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 Nothing    $6}
                              | alias identifier ':' subtype_indication is name ';'                                                         {raisePosition PackageBodyDeclarativeItem_AliasDeclaration $ newAliasDeclaration $1 (newSimpleName $2) $4 $6}
                              | use selected_name_list ';'                                                                                  {raisePosition PackageBodyDeclarativeItem_UseClause $ newUseClause $1 $2}

------------------------------------------
-- Declarations
------------------------------------------

-- type_declaration ::=
--    full_type_declaration
--    | incomplete_type_declaration
-- full_type_declaration ::=
--    __type__ identifier __is__ type_definition ;
-- incomplete_type_declaration ::= __type__ identifier ;
-- type_definition ::=
--    scalar_type_definition
--    | composite_type_definition
--    | access_type_definition
--    | file_type_definition
-- scalar_type_definition ::=
--    enumeration_type_definition
--    | integer_type_definition
--    | floating_type_definition
--    | physical_type_definition
-- enumeration_type_definition ::=
--    ( enumeration_literal { , enumeration_literal } )
-- NOTE: Implemented as
--    enumeration_type_definition ::= ( enumeration_list )
-- integer_type_definition ::= range_constraint
-- floating_type_definition ::= range_constraint
-- physical_type_definition ::=
--    range_constraint
--       __units__
--          base_unit_declaration
--          { secondary_unit_declaration }
--       __end__ __units__
-- range_constraint ::= __range__ range_definition
-- base_unit_declaration ::= identifier ;
-- composite_type_definition ::=
--    array_type_definition
--    | record_type_definition
-- array_type_definition ::=
--    unconstrained_array_definition | constrained_array_definition
-- unconstrained_array_definition ::=
--    __array__ ( index_subtype_definition { , index_subtype_definition } )
--       __of__ /element/_subtype_indication
-- constrained_array_definition ::=
--    __array__ index_constraint __of__ /element/_subtype_indication
-- index_constraint ::= ( discrete_range { , discrete_range } )
-- record_type_definition ::=
--    __record__
--       element_declaration
--       { element_declaration }
--    __end__ __record__
-- access_type_definition ::= __access__ subtype_indication
-- file_type_definition ::= __file__ __of__ type_mark

subtype_indication :: { WrappedSubtypeIndication }
                   : name   name range range_definition         {newSubtypeIndication (Just $1)   $2 $ Just $ newConstraint_Range $ newRangeConstraint $3 $4}
                   |        name range range_definition         {newSubtypeIndication Nothing     $1 $ Just $ newConstraint_Range $ newRangeConstraint $2 $3}
                   | name   name '(' index_constraint_list ')'  {newSubtypeIndication (Just $1)   $2 $ Just $ newConstraint_Index $3 $4}
                   |        name '(' index_constraint_list ')'  {newSubtypeIndication Nothing     $1 $ Just $ newConstraint_Index $2 $3}
                   | name   name                                {newSubtypeIndication (Just $1)   $2 Nothing}
                   |        name                                {newSubtypeIndication Nothing     $1 Nothing}

-- interface_declaration ::=
--    interface_constant_declaration
--    | interface_signal_declaration
--    | interface_variable_declaration
-- interface_constant_declaration ::=
--    [ __constant__ ] identifier_list : [ __in__ ] subtype_indication [ := /static/_expression ]
-- interface_signal_declaration ::=
--    [ __signal__ ] identifier_list : [ mode ] subtype_indication [ __bus__ ] [ := /static/_expression ]
-- interface_variable_declaration ::=
--    [ __variable__ ] identifier_list : [ mode ] subtype_indication [ := /static/_expression ]
interface_declaration :: { WrappedInterfaceDeclaration }
                      : constant identifier_list ':' in     subtype_indication      ':=' expression   {newInterfaceDeclaration   (Just $ passPosition Constant $1)      $2 (Just $ passPosition In $4)   $5 (Just $7)}
                      | constant identifier_list ':' in     subtype_indication                        {newInterfaceDeclaration   (Just $ passPosition Constant $1)      $2 (Just $ passPosition In $4)   $5 Nothing}
                      | constant identifier_list ':'        subtype_indication      ':=' expression   {newInterfaceDeclaration   (Just $ passPosition Constant $1)      $2 Nothing                       $4 (Just $6)}
                      | constant identifier_list ':'        subtype_indication                        {newInterfaceDeclaration   (Just $ passPosition Constant $1)      $2 Nothing                       $4 Nothing}
                      | signal   identifier_list ':' mode   subtype_indication bus  ':=' expression   {newInterfaceDeclaration   (Just $ passPosition GuardedSignal $1) $2 (Just $4)                     $5 (Just $8)}
                      | signal   identifier_list ':' mode   subtype_indication bus                    {newInterfaceDeclaration   (Just $ passPosition GuardedSignal $1) $2 (Just $4)                     $5 Nothing}
                      | signal   identifier_list ':' mode   subtype_indication      ':=' expression   {newInterfaceDeclaration   (Just $ passPosition Signal $1)        $2 (Just $4)                     $5 (Just $7)}
                      | signal   identifier_list ':' mode   subtype_indication                        {newInterfaceDeclaration   (Just $ passPosition Signal $1)        $2 (Just $4)                     $5 Nothing}
                      | signal   identifier_list ':'        subtype_indication bus  ':=' expression   {newInterfaceDeclaration   (Just $ passPosition GuardedSignal $1) $2 Nothing                       $4 (Just $7)}
                      | signal   identifier_list ':'        subtype_indication bus                    {newInterfaceDeclaration   (Just $ passPosition GuardedSignal $1) $2 Nothing                       $4 Nothing}
                      | signal   identifier_list ':'        subtype_indication      ':=' expression   {newInterfaceDeclaration   (Just $ passPosition Signal $1)        $2 Nothing                       $4 (Just $6)}
                      | signal   identifier_list ':'        subtype_indication                        {newInterfaceDeclaration   (Just $ passPosition Signal $1)        $2 Nothing                       $4 Nothing}
                      | variable identifier_list ':' mode   subtype_indication      ':=' expression   {newInterfaceDeclaration   (Just $ passPosition Variable $1)      $2 (Just $4)                     $5 (Just $7)}
                      | variable identifier_list ':' mode   subtype_indication                        {newInterfaceDeclaration   (Just $ passPosition Variable $1)      $2 (Just $4)                     $5 Nothing}
                      | variable identifier_list ':'        subtype_indication      ':=' expression   {newInterfaceDeclaration   (Just $ passPosition Variable $1)      $2 Nothing                       $4 (Just $6)}
                      | variable identifier_list ':'        subtype_indication                        {newInterfaceDeclaration   (Just $ passPosition Variable $1)      $2 Nothing                       $4 Nothing}
                      |          identifier_list ':' mode   subtype_indication bus  ':=' expression   {newInterfaceDeclaration   (Just $ passPosition GuardedSignal $5) $1 (Just $3)                     $4 (Just $7)}
                      |          identifier_list ':' mode   subtype_indication bus                    {newInterfaceDeclaration   (Just $ passPosition GuardedSignal $5) $1 (Just $3)                     $4 Nothing}
                      |          identifier_list ':'        subtype_indication bus  ':=' expression   {newInterfaceDeclaration   (Just $ passPosition GuardedSignal $4) $1 Nothing                       $3 (Just $6)}
                      |          identifier_list ':'        subtype_indication bus                    {newInterfaceDeclaration   (Just $ passPosition GuardedSignal $4) $1 Nothing                       $3 Nothing}
                      |          identifier_list ':' mode   subtype_indication      ':=' expression   {newInterfaceDeclaration   Nothing                                $1 (Just $3)                     $4 (Just $6)}
                      |          identifier_list ':' mode   subtype_indication                        {newInterfaceDeclaration   Nothing                                $1 (Just $3)                     $4 Nothing}
                      |          identifier_list ':'        subtype_indication      ':=' expression   {newInterfaceDeclaration   Nothing                                $1 Nothing                       $3 (Just $5)}
                      |          identifier_list ':'        subtype_indication                        {newInterfaceDeclaration   Nothing                                $1 Nothing                       $3 Nothing}

-- mode ::= __in__ | __out__ | __inout__ | __buffer__ | __linkage__
mode :: { WrappedMode }
     : in      {passPosition In $1}
     | out     {passPosition Out $1}
     | inout   {passPosition Inout $1}
     | buffer  {passPosition Buffer $1}
     | linkage {passPosition Linkage $1}

-- interface_list ::=
--    interface_element { ; interface_element }
-- interface_element ::= interface_declaration
interface_list :: { InterfaceList }
               :                    interface_declaration {[$1]}
               | interface_list ';' interface_declaration {$3 : $1}

-- enumeration_list ::=
--    enumeration_literal { , enumeration_literal }
-- enumeration_literal ::= identifier | character_literal
enumeration_list :: { [WrappedEnumerationLiteral] }
                 : char                            {[newFromToken (EnumerationLiteral_Char) extractChar $1]}
                 | identifier                      {[newFromToken (EnumerationLiteral_Identifier) extractIdentifier $1]}
                 | enumeration_list ',' char       {(newFromToken (EnumerationLiteral_Char) extractChar $3) : $1}
                 | enumeration_list ',' identifier {(newFromToken (EnumerationLiteral_Identifier) extractIdentifier $3) : $1}

-- secondary_unit_declaration_list ::= { secondary_unit_declaration }
-- secondary_unit_declaration ::= identifier = physical_literal ;
-- physical_literal ::= [ abstract_literal ] /unit/_name
-- abstract_literal ::= decimal_literal | based_literal
-- NOTE: Abstract literals are lexed to integer or real universal types
secondary_unit_declaration_list :: { [WrappedSecondaryUnitDeclaration] }
                                : {- empty -}                                                      {[]}
                                | secondary_unit_declaration_list identifier '=' integer  name ';' {(newSecondaryUnitDeclaration (newSimpleName $2) $ newPhysicalLiteral (Just $ newAbstractLiteral_Integer $4)   $5) : $1}
                                | secondary_unit_declaration_list identifier '=' real     name ';' {(newSecondaryUnitDeclaration (newSimpleName $2) $ newPhysicalLiteral (Just $ newAbstractLiteral_Real    $4)   $5) : $1}
                                | secondary_unit_declaration_list identifier '='          name ';' {(newSecondaryUnitDeclaration (newSimpleName $2) $ newPhysicalLiteral Nothing                                  $4) : $1}

-- index_subtype_definition_list ::= index_subtype_definition { , index_subtype_definition }
-- index_subtype_definition ::= type_mark __range__ <>
index_subtype_definition_list :: { [WrappedName] }
                              : name range '<>'                                     {[$1]}
                              | index_subtype_definition_list ',' name range '<>'   {$3 : $1}

-- index_constraint ::= ( discrete_range { , discrete_range } )
discrete_range_list :: { IndexConstraint }
                    : discrete_range                     {[$1]}
                    | discrete_range_list discrete_range {$2 : $1}

-- element_declaration_list ::= element_declaration { element_declaration }
-- element_declaration ::=
--    identifier_list : element_subtype_definition ;
-- element_subtype_definition ::= subtype_indication
element_declaration_list :: { [WrappedElementDeclaration] }
                         :                            identifier_list ':' subtype_indication ';' {[newElementDeclaration $1 $3]}
                         | element_declaration_list   identifier_list ':' subtype_indication ';' {(newElementDeclaration $2 $4) : $1}

-- identifier_list ::= identifier { , identifier }
identifier_list :: { [WrappedSimpleName] }
                : identifier_list ','  identifier  {(newSimpleName $3) : $1}
                |                      identifier  {[newSimpleName $1]}

------------------------------------------
-- Specifications
------------------------------------------

-- entity_class ::=
--    __entity__
--    | __architecture__
--    | __configuration__
--    | __procedure__
--    | __function__
--    | __package__
--    | __type__
--    | __subtype__
--    | __constant__
--    | __signal__
--    | __variable__
--    | __component__
--    | __label__
entity_class :: { WrappedEntityClass }
             : entity         {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Entity }}
             | architecture   {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Architecture }}
             | configuration  {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Configuration }}
             | procedure      {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Procedure }}
             | function       {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Function }}
             | package        {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Package }}
             | type           {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Type }}
             | subtype        {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Subtype }}
             | constant       {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Constant }}
             | signal         {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Signal }}
             | variable       {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Variable }}
             | component      {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Component }}
             | label          {PosnWrapper { getPos = getPos $1, unPos = EntityClass_Label }}

-- entity_name_list ::=
--    entity_designator { , entity_designator }
entity_designator_list :: { [WrappedEntityDesignator] }
                       : identifier                            {[newFromToken EntityDesignator_Name      extractIdentifier $1]}
                       | str                                   {[newFromToken EntityDesignator_Operator  extractString     $1]}
                       | entity_designator_list ',' identifier {(newFromToken EntityDesignator_Name      extractIdentifier $3) : $1}
                       | entity_designator_list ',' str        {(newFromToken EntityDesignator_Operator  extractString     $3) : $1}

-- signal_list ::=
--    /signal/_name { , /signal/_name }
signal_list :: { [WrappedName] }
            :                 name  {[$1]}
            | signal_list ',' name  {$3 : $1}

------------------------------------------
-- Sequential Statements
------------------------------------------

sequence_of_statements :: { [WrappedSequentialStatement] }
                       : {- empty -}                                 {[]}
                       | sequence_of_statements sequential_statement {$2 : $1}

-- sequential_statement ::=
--    wait_statement
--    | assertion_statement
--    | signal_assignment_statement
--    | variable_assignment_statement
--    | procedure_call_statement
--    | if_statement
--    | case_statement
--    | loop_statement
--    | next_statement
--    | exit_statement
--    | return_statement
--    | null_statement
sequential_statement :: { WrappedSequentialStatement }
                     : wait   on sensitivity_list  until expression  for expression                                        ';' {raisePosition SequentialStatement_Wait $ newWaitStatement $1 (Just $3)   (Just $5)   (Just $7)}
                     | wait   on sensitivity_list  until expression                                                        ';' {raisePosition SequentialStatement_Wait $ newWaitStatement $1 (Just $3)   (Just $5)   Nothing}
                     | wait   on sensitivity_list                    for expression                                        ';' {raisePosition SequentialStatement_Wait $ newWaitStatement $1 (Just $3)   Nothing     (Just $5)}
                     | wait   on sensitivity_list                                                                          ';' {raisePosition SequentialStatement_Wait $ newWaitStatement $1 (Just $3)   Nothing     Nothing}
                     | wait                        until expression  for expression                                        ';' {raisePosition SequentialStatement_Wait $ newWaitStatement $1 Nothing     (Just $3)   (Just $5)}
                     | wait                        until expression                                                        ';' {raisePosition SequentialStatement_Wait $ newWaitStatement $1 Nothing     (Just $3)   Nothing}
                     | wait                                          for expression                                        ';' {raisePosition SequentialStatement_Wait $ newWaitStatement $1 Nothing     Nothing     (Just $3)}
                     | wait                                                                                                ';' {raisePosition SequentialStatement_Wait $ newWaitStatement $1 Nothing     Nothing     Nothing}
                     | assert expression report expression severity expression                                             ';' {raisePosition SequentialStatement_Assertion $ newAssertionStatement $1 $2 (Just $4)   (Just $6)}
                     | assert expression report expression                                                                 ';' {raisePosition SequentialStatement_Assertion $ newAssertionStatement $1 $2 (Just $4)   Nothing}
                     | assert expression                   severity expression                                             ';' {raisePosition SequentialStatement_Assertion $ newAssertionStatement $1 $2 Nothing     (Just $4)}
                     | assert expression                                                                                   ';' {raisePosition SequentialStatement_Assertion $ newAssertionStatement $1 $2 Nothing     Nothing}
                     | name                              '<=' transport waveform                                           ';' {raisePosition SequentialStatement_SignalAssignment $ newSignalAssignmentStatement (newTarget_Name $1)           (Just $ passPosition SignalAssignmentTransport $3) $4}
                     | name                              '<='           waveform                                           ';' {raisePosition SequentialStatement_SignalAssignment $ newSignalAssignmentStatement (newTarget_Name $1)           Nothing                                            $3}
                     | '(' element_association_list ')'  '<=' transport waveform                                           ';' {raisePosition SequentialStatement_SignalAssignment $ newSignalAssignmentStatement (newTarget_Aggregate $1 $2)   (Just $ passPosition SignalAssignmentTransport $5) $6}
                     | '(' element_association_list ')'  '<='           waveform                                           ';' {raisePosition SequentialStatement_SignalAssignment $ newSignalAssignmentStatement (newTarget_Aggregate $1 $2)   Nothing                                            $5}
                     | name                              ':=' expression                                                   ';' {raisePosition SequentialStatement_VariableAssignment $ newVariableAssignmentStatement (newTarget_Name $1)           $3}
                     | '(' element_association_list ')'  ':=' expression                                                   ';' {raisePosition SequentialStatement_VariableAssignment $ newVariableAssignmentStatement (newTarget_Aggregate $1 $2)   $5}
                     | name '(' association_list ')'                                                                       ';' {raisePosition SequentialStatement_ProcedureCall $ newProcedureCallStatement $1 (Just $3)}
                     | name                                                                                                ';' {raisePosition SequentialStatement_ProcedureCall $ newProcedureCallStatement $1 Nothing}
                     | if expression then sequence_of_statements elsif_statement_list else sequence_of_statements end if   ';' {raisePosition SequentialStatement_If $ newIfStatement $1 $2 $4 $5 (Just $7)}
                     | if expression then sequence_of_statements elsif_statement_list                             end if   ';' {raisePosition SequentialStatement_If $ newIfStatement $1 $2 $4 $5 Nothing}
                     | case expression is case_statement_alternative_list end case                                         ';' {raisePosition SequentialStatement_Case $ newCaseStatement $1 $2 $4}
                     | identifier ':'  while expression                 loop sequence_of_statements end loop identifier    ';' {raisePosition SequentialStatement_Loop $ newLoopStatement (Just $ newSimpleName $1)  (Right $ newIterationScheme_While $3 $4)                   $6 (Just $ newSimpleName $9)}
                     | identifier ':'  for identifier in discrete_range loop sequence_of_statements end loop identifier    ';' {raisePosition SequentialStatement_Loop $ newLoopStatement (Just $ newSimpleName $1)  (Right $ newIterationScheme_For $3 (newSimpleName $4) $6)  $8 (Just $ newSimpleName $11)}
                     | identifier ':'  while expression                 loop sequence_of_statements end loop               ';' {raisePosition SequentialStatement_Loop $ newLoopStatement (Just $ newSimpleName $1)  (Right $ newIterationScheme_While $3 $4)                   $6 Nothing}
                     | identifier ':'  for identifier in discrete_range loop sequence_of_statements end loop               ';' {raisePosition SequentialStatement_Loop $ newLoopStatement (Just $ newSimpleName $1)  (Right $ newIterationScheme_For $3 (newSimpleName $4) $6)  $8 Nothing}
                     |                 while expression                 loop sequence_of_statements end loop identifier    ';' {raisePosition SequentialStatement_Loop $ newLoopStatement Nothing                    (Right $ newIterationScheme_While $1 $2)                   $4 (Just $ newSimpleName $7)}
                     |                 for identifier in discrete_range loop sequence_of_statements end loop identifier    ';' {raisePosition SequentialStatement_Loop $ newLoopStatement Nothing                    (Right $ newIterationScheme_For $1 (newSimpleName $2) $4)  $6 (Just $ newSimpleName $9)}
                     |                 while expression                 loop sequence_of_statements end loop               ';' {raisePosition SequentialStatement_Loop $ newLoopStatement Nothing                    (Right $ newIterationScheme_While $1 $2)                   $4 Nothing}
                     |                 for identifier in discrete_range loop sequence_of_statements end loop               ';' {raisePosition SequentialStatement_Loop $ newLoopStatement Nothing                    (Right $ newIterationScheme_For $1 (newSimpleName $2) $4)  $6 Nothing}
                     | identifier ':'                                   loop sequence_of_statements end loop identifier    ';' {raisePosition SequentialStatement_Loop $ newLoopStatement (Just $ newSimpleName $1)  (Left $3)                                                  $4 (Just $ newSimpleName $7)}
                     | identifier ':'                                   loop sequence_of_statements end loop               ';' {raisePosition SequentialStatement_Loop $ newLoopStatement (Just $ newSimpleName $1)  (Left $3)                                                  $4 Nothing}
                     |                                                  loop sequence_of_statements end loop identifier    ';' {raisePosition SequentialStatement_Loop $ newLoopStatement Nothing                    (Left $1)                                                  $2 (Just $ newSimpleName $5)}
                     |                                                  loop sequence_of_statements end loop               ';' {raisePosition SequentialStatement_Loop $ newLoopStatement Nothing                    (Left $1)                                                  $2 Nothing}
                     | next identifier when expression                                                                     ';' {raisePosition SequentialStatement_Next $ newNextStatement $1 (Just $ newSimpleName $2)  (Just $4)}
                     | next identifier                                                                                     ';' {raisePosition SequentialStatement_Next $ newNextStatement $1 (Just $ newSimpleName $2)  Nothing}
                     | next            when expression                                                                     ';' {raisePosition SequentialStatement_Next $ newNextStatement $1 Nothing                    (Just $3)}
                     | next                                                                                                ';' {raisePosition SequentialStatement_Next $ newNextStatement $1 Nothing                    Nothing}
                     | exit identifier when expression                                                                     ';' {raisePosition SequentialStatement_Exit $ newExitStatement $1 (Just $ newSimpleName $2)  (Just $4)}
                     | exit identifier                                                                                     ';' {raisePosition SequentialStatement_Exit $ newExitStatement $1 (Just $ newSimpleName $2)  Nothing}
                     | exit            when expression                                                                     ';' {raisePosition SequentialStatement_Exit $ newExitStatement $1 Nothing                    (Just $3)}
                     | exit                                                                                                ';' {raisePosition SequentialStatement_Exit $ newExitStatement $1 Nothing                    Nothing}
                     | return expression                                                                                   ';' {raisePosition SequentialStatement_Return $ newReturnStatement $1 (Just $2)}
                     | return                                                                                              ';' {raisePosition SequentialStatement_Return $ newReturnStatement $1 Nothing}
                     | null                                                                                                ';' {passPosition SequentialStatement_Null $1}

-- sensitivity_list ::= /signal/_name { , /signal/_name }
sensitivity_list :: { [WrappedName] }
                 :                        name {[$1]}
                 | sensitivity_list ','   name {$3 : $1}

waveform :: { Waveform }
         :              expression  after expression  {[newWaveform_Expression $1 (Just $3)]}
         |              expression                    {[newWaveform_Expression $1 Nothing]}
         |              null        after expression  {[newWaveform_Null $1 (Just $3)]}
         |              null                          {[newWaveform_Null $1 Nothing]}
         | waveform ',' expression  after expression  {(newWaveform_Expression $3 (Just $5)) : $1}
         | waveform ',' expression                    {(newWaveform_Expression $3 Nothing)   : $1}
         | waveform ',' null        after expression  {(newWaveform_Null $3 (Just $5))       : $1}
         | waveform ',' null                          {(newWaveform_Null $3 Nothing)         : $1}

elsif_statement_list :: { [WrappedElsifStatement] }
                     : {- empty -}                                                        {[]}
                     | elsif_statement_list elsif expression then sequence_of_statements  {(newElsifStatement $2 $3 $5) : $1}

case_statement_alternative_list :: { [WrappedCaseStatementAlternative] }
                                :                                 when choices '=>' sequence_of_statements {[newCaseStatementAlternative $1 $2 $4]}
                                | case_statement_alternative_list when choices '=>' sequence_of_statements {(newCaseStatementAlternative $2 $3 $5) : $1}

------------------------------------------
-- Concurrent Statements
------------------------------------------

concurrent_statement :: { WrappedConcurrentStatement }
                     : identifier ':'  block '(' expression ')'   block_header block_declarative_part begin block_statement_part end block identifier  ';' {raisePosition Concurrent_BlockStatement $ newBlockStatement (newSimpleName $1) (Just $5)  $7 $8 $10   (Just $ newSimpleName $13)}
                     | identifier ':'  block '(' expression ')'   block_header block_declarative_part begin block_statement_part end block             ';' {raisePosition Concurrent_BlockStatement $ newBlockStatement (newSimpleName $1) (Just $5)  $7 $8 $10   Nothing}
                     | identifier ':'  block                      block_header block_declarative_part begin block_statement_part end block identifier  ';' {raisePosition Concurrent_BlockStatement $ newBlockStatement (newSimpleName $1) Nothing    $4 $5 $7    (Just $ newSimpleName $10)}
                     | identifier ':'  block                      block_header block_declarative_part begin block_statement_part end block             ';' {raisePosition Concurrent_BlockStatement $ newBlockStatement (newSimpleName $1) Nothing    $4 $5 $7    Nothing}
                     | identifier ':'  process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process identifier   ';' {raisePosition Concurrent_ProcessStatement $ newProcessStatement (Left $ newSimpleName $1)   (Just $5)   $7 $9 (Just $ newSimpleName $12)}
                     | identifier ':'  process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process              ';' {raisePosition Concurrent_ProcessStatement $ newProcessStatement (Left $ newSimpleName $1)   (Just $5)   $7 $9 Nothing}
                     | identifier ':'  process                          process_declarative_part begin process_statement_part end process identifier   ';' {raisePosition Concurrent_ProcessStatement $ newProcessStatement (Left $ newSimpleName $1)   Nothing     $4 $6 (Just $ newSimpleName $9)}
                     | identifier ':'  process                          process_declarative_part begin process_statement_part end process              ';' {raisePosition Concurrent_ProcessStatement $ newProcessStatement (Left $ newSimpleName $1)   Nothing     $4 $6 Nothing}
                     |                 process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process identifier   ';' {raisePosition Concurrent_ProcessStatement $ newProcessStatement (Right $1)                  (Just $3)   $5 $7 (Just $ newSimpleName $10)}
                     |                 process '(' sensitivity_list ')' process_declarative_part begin process_statement_part end process              ';' {raisePosition Concurrent_ProcessStatement $ newProcessStatement (Right $1)                  (Just $3)   $5 $7 Nothing}
                     |                 process                          process_declarative_part begin process_statement_part end process identifier   ';' {raisePosition Concurrent_ProcessStatement $ newProcessStatement (Right $1)                  Nothing     $2 $4 (Just $ newSimpleName $7)}
                     |                 process                          process_declarative_part begin process_statement_part end process              ';' {raisePosition Concurrent_ProcessStatement $ newProcessStatement (Right $1)                  Nothing     $2 $4 Nothing}
                     | identifier ':'  name '(' association_list ')'                                                                                   ';' {raisePosition Concurrent_ProcedureCall $ newConcurrentProcedureCall (Just $ newSimpleName $1)   $ newProcedureCallStatement $3 (Just $5)}
                     | identifier ':'  name                                                                                                            ';' {raisePosition Concurrent_ProcedureCall $ newConcurrentProcedureCall (Just $ newSimpleName $1)   $ newProcedureCallStatement $3 Nothing}
                     |                 name '(' association_list ')'                                                                                   ';' {raisePosition Concurrent_ProcedureCall $ newConcurrentProcedureCall Nothing                     $ newProcedureCallStatement $1 (Just $3)}
                     |                 name                                                                                                            ';' {raisePosition Concurrent_ProcedureCall $ newConcurrentProcedureCall Nothing                     $ newProcedureCallStatement $1 Nothing}
                     | identifier ':'  assert expression report expression severity expression                                                         ';' {raisePosition Concurrent_AssertionStatement $ newConcurrentAssertionStatement (Just $ newSimpleName $1)   $ newAssertionStatement $3 $4 (Just $6)   (Just $8)}
                     | identifier ':'  assert expression                   severity expression                                                         ';' {raisePosition Concurrent_AssertionStatement $ newConcurrentAssertionStatement (Just $ newSimpleName $1)   $ newAssertionStatement $3 $4 Nothing     (Just $6)}
                     | identifier ':'  assert expression report expression                                                                             ';' {raisePosition Concurrent_AssertionStatement $ newConcurrentAssertionStatement (Just $ newSimpleName $1)   $ newAssertionStatement $3 $4 (Just $6)   Nothing}
                     | identifier ':'  assert expression                                                                                               ';' {raisePosition Concurrent_AssertionStatement $ newConcurrentAssertionStatement (Just $ newSimpleName $1)   $ newAssertionStatement $3 $4 Nothing     Nothing}
                     |                 assert expression report expression severity expression                                                         ';' {raisePosition Concurrent_AssertionStatement $ newConcurrentAssertionStatement Nothing                     $ newAssertionStatement $1 $2 (Just $4)   (Just $6)}
                     |                 assert expression                   severity expression                                                         ';' {raisePosition Concurrent_AssertionStatement $ newConcurrentAssertionStatement Nothing                     $ newAssertionStatement $1 $2 Nothing     (Just $4)}
                     |                 assert expression report expression                                                                             ';' {raisePosition Concurrent_AssertionStatement $ newConcurrentAssertionStatement Nothing                     $ newAssertionStatement $1 $2 (Just $4)   Nothing}
                     |                 assert expression                                                                                               ';' {raisePosition Concurrent_AssertionStatement $ newConcurrentAssertionStatement Nothing                     $ newAssertionStatement $1 $2 Nothing     Nothing}
                     | identifier ':'  name                             '<=' guarded   transport   conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional (Just $ newSimpleName $1) (newTarget_Name $3)          (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $5)  (Just $ passPosition SignalAssignment_Transport $6))  $7 $8}
                     | identifier ':'  name                             '<=' guarded               conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional (Just $ newSimpleName $1) (newTarget_Name $3)          (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $5)  Nothing)                                              $6 $7}
                     | identifier ':'  name                             '<='           transport   conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional (Just $ newSimpleName $1) (newTarget_Name $3)          (SignalAssignmentOptions Nothing                                            (Just $ passPosition SignalAssignment_Transport $5))  $6 $7}
                     | identifier ':'  name                             '<='                       conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional (Just $ newSimpleName $1) (newTarget_Name $3)          (SignalAssignmentOptions Nothing                                            Nothing)                                              $5 $6}
                     |                 name                             '<=' guarded   transport   conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional Nothing                   (newTarget_Name $1)          (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $3)  (Just $ passPosition SignalAssignment_Transport $4))  $5 $6}
                     |                 name                             '<=' guarded               conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional Nothing                   (newTarget_Name $1)          (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $3)  Nothing)                                              $4 $5}
                     |                 name                             '<='           transport   conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional Nothing                   (newTarget_Name $1)          (SignalAssignmentOptions Nothing                                            (Just $ passPosition SignalAssignment_Transport $3))  $4 $5}
                     |                 name                             '<='                       conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional Nothing                   (newTarget_Name $1)          (SignalAssignmentOptions Nothing                                            Nothing)                                              $3 $4}
                     | identifier ':'  '(' element_association_list ')' '<=' guarded   transport   conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional (Just $ newSimpleName $1) (newTarget_Aggregate $3 $4)  (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $7)  (Just $ passPosition SignalAssignment_Transport $8))  $9 $10}
                     | identifier ':'  '(' element_association_list ')' '<=' guarded               conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional (Just $ newSimpleName $1) (newTarget_Aggregate $3 $4)  (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $7)  Nothing)                                              $8 $9}
                     | identifier ':'  '(' element_association_list ')' '<='           transport   conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional (Just $ newSimpleName $1) (newTarget_Aggregate $3 $4)  (SignalAssignmentOptions Nothing                                            (Just $ passPosition SignalAssignment_Transport $7))  $8 $9}
                     | identifier ':'  '(' element_association_list ')' '<='                       conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional (Just $ newSimpleName $1) (newTarget_Aggregate $3 $4)  (SignalAssignmentOptions Nothing                                            Nothing)                                              $7 $8}
                     |                 '(' element_association_list ')' '<=' guarded   transport   conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional Nothing                   (newTarget_Aggregate $1 $2)  (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $5)  (Just $ passPosition SignalAssignment_Transport $6))  $7 $8}
                     |                 '(' element_association_list ')' '<=' guarded               conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional Nothing                   (newTarget_Aggregate $1 $2)  (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $5)  Nothing)                                              $6 $7}
                     |                 '(' element_association_list ')' '<='           transport   conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional Nothing                   (newTarget_Aggregate $1 $2)  (SignalAssignmentOptions Nothing                                            (Just $ passPosition SignalAssignment_Transport $5))  $6 $7}
                     |                 '(' element_association_list ')' '<='                       conditional_waveform_pairs waveform                 ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Conditional Nothing                   (newTarget_Aggregate $1 $2)  (SignalAssignmentOptions Nothing                                            Nothing)                                              $5 $6}
                     | identifier ':'  with expression select name                              '<=' guarded   transport   selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected (Just $ newSimpleName $1) $4 (newTarget_Name $6)          (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $8)  (Just $ passPosition SignalAssignment_Transport $9))  $10}
                     | identifier ':'  with expression select name                              '<=' guarded               selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected (Just $ newSimpleName $1) $4 (newTarget_Name $6)          (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $8)  Nothing)                                              $9}
                     | identifier ':'  with expression select name                              '<='           transport   selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected (Just $ newSimpleName $1) $4 (newTarget_Name $6)          (SignalAssignmentOptions Nothing                                            (Just $ passPosition SignalAssignment_Transport $8))  $9}
                     | identifier ':'  with expression select name                              '<='                       selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected (Just $ newSimpleName $1) $4 (newTarget_Name $6)          (SignalAssignmentOptions Nothing                                            Nothing)                                              $8}
                     |                 with expression select name                              '<=' guarded   transport   selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected Nothing                   $2 (newTarget_Name $4)          (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $6)  (Just $ passPosition SignalAssignment_Transport $7))  $8}
                     |                 with expression select name                              '<=' guarded               selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected Nothing                   $2 (newTarget_Name $4)          (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $6)  Nothing)                                              $7}
                     |                 with expression select name                              '<='           transport   selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected Nothing                   $2 (newTarget_Name $4)          (SignalAssignmentOptions Nothing                                            (Just $ passPosition SignalAssignment_Transport $6))  $7}
                     |                 with expression select name                              '<='                       selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected Nothing                   $2 (newTarget_Name $4)          (SignalAssignmentOptions Nothing                                            Nothing)                                              $6}
                     | identifier ':'  with expression select '(' element_association_list ')'  '<=' guarded   transport   selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected (Just $ newSimpleName $1) $4 (newTarget_Aggregate $6 $7)  (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $10) (Just $ passPosition SignalAssignment_Transport $11)) $12}
                     | identifier ':'  with expression select '(' element_association_list ')'  '<=' guarded               selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected (Just $ newSimpleName $1) $4 (newTarget_Aggregate $6 $7)  (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $10) Nothing)                                              $11}
                     | identifier ':'  with expression select '(' element_association_list ')'  '<='           transport   selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected (Just $ newSimpleName $1) $4 (newTarget_Aggregate $6 $7)  (SignalAssignmentOptions Nothing                                            (Just $ passPosition SignalAssignment_Transport $10)) $11}
                     | identifier ':'  with expression select '(' element_association_list ')'  '<='                       selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected (Just $ newSimpleName $1) $4 (newTarget_Aggregate $6 $7)  (SignalAssignmentOptions Nothing                                            Nothing)                                              $10}
                     |                 with expression select '(' element_association_list ')'  '<=' guarded   transport   selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected Nothing                   $2 (newTarget_Aggregate $4 $5)  (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $8)  (Just $ passPosition SignalAssignment_Transport $9))  $10}
                     |                 with expression select '(' element_association_list ')'  '<=' guarded               selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected Nothing                   $2 (newTarget_Aggregate $4 $5)  (SignalAssignmentOptions (Just $ passPosition SignalAssignment_Guarded $8)  Nothing)                                              $9}
                     |                 with expression select '(' element_association_list ')'  '<='           transport   selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected Nothing                   $2 (newTarget_Aggregate $4 $5)  (SignalAssignmentOptions Nothing                                            (Just $ passPosition SignalAssignment_Transport $8))  $9}
                     |                 with expression select '(' element_association_list ')'  '<='                       selected_waveforms          ';' {raisePosition Concurrent_SignalAssignmentStatement $ newConcurrentSignalAssignmentStatement_Selected Nothing                   $2 (newTarget_Aggregate $4 $5)  (SignalAssignmentOptions Nothing                                            Nothing)                                              $8}
                     | identifier ':'  name generic map '(' association_list ')' port map '(' association_list ')'                                     ';' {raisePosition Concurrent_ComponentInstantiationStatement $ newComponentInstantiationStatement (newSimpleName $1) $3 (Just $ newGenericMapAspect $4 $7)   (Just $ newPortMapAspect $9 $12)}
                     | identifier ':'  name generic map '(' association_list ')'                                                                       ';' {raisePosition Concurrent_ComponentInstantiationStatement $ newComponentInstantiationStatement (newSimpleName $1) $3 (Just $ newGenericMapAspect $4 $7)   Nothing}
                     | identifier ':'  name                                      port map '(' association_list ')'                                     ';' {raisePosition Concurrent_ComponentInstantiationStatement $ newComponentInstantiationStatement (newSimpleName $1) $3 Nothing                              (Just $ newPortMapAspect $4 $7)}
                     | identifier ':'  name                                                                                                            ';' {raisePosition Concurrent_ComponentInstantiationStatement $ newComponentInstantiationStatement (newSimpleName $1) $3 Nothing                              Nothing}
                     | identifier ':'  for identifier ':' discrete_range   generate concurrent_statement_list end generate identifier                  ';' {raisePosition Concurrent_GenerateStatement $ newGenerateStatement (newSimpleName $1) (newGenerationScheme_For $3 (newSimpleName $4) $6)   $8 (Just $ newSimpleName $11)}
                     | identifier ':'  if expression                       generate concurrent_statement_list end generate identifier                  ';' {raisePosition Concurrent_GenerateStatement $ newGenerateStatement (newSimpleName $1) (newGenerationScheme_If $3 $4)                       $6 (Just $ newSimpleName $9)}
                     | identifier ':'  for identifier ':' discrete_range   generate concurrent_statement_list end generate                             ';' {raisePosition Concurrent_GenerateStatement $ newGenerateStatement (newSimpleName $1) (newGenerationScheme_For $3 (newSimpleName $4) $6)   $8 Nothing}
                     | identifier ':'  if expression                       generate concurrent_statement_list end generate                             ';' {raisePosition Concurrent_GenerateStatement $ newGenerateStatement (newSimpleName $1) (newGenerationScheme_If $3 $4)                       $6 Nothing}

block_header :: { BlockHeader }
             : generic '(' interface_list ')' ';' generic map '(' association_list ')' ';'   port '(' interface_list ')' ';'  port map '(' association_list ')' ';'  {BlockHeader (Just $ newBlockHeader_Generic (newGenericClause $1 $3) (Just $ newGenericMapAspect $6 $9)) (Just $ newBlockHeader_Port (newPortClause $12 $14)   (Just $ newPortMapAspect $17 $20))}
             | generic '(' interface_list ')' ';' generic map '(' association_list ')' ';'   port '(' interface_list ')' ';'                                         {BlockHeader (Just $ newBlockHeader_Generic (newGenericClause $1 $3) (Just $ newGenericMapAspect $6 $9)) (Just $ newBlockHeader_Port (newPortClause $12 $14)   Nothing)}
             | generic '(' interface_list ')' ';'                                            port '(' interface_list ')' ';'  port map '(' association_list ')' ';'  {BlockHeader (Just $ newBlockHeader_Generic (newGenericClause $1 $3) Nothing)                            (Just $ newBlockHeader_Port (newPortClause $6 $8)     (Just $ newPortMapAspect $11 $14))}
             | generic '(' interface_list ')' ';'                                            port '(' interface_list ')' ';'                                         {BlockHeader (Just $ newBlockHeader_Generic (newGenericClause $1 $3) Nothing)                            (Just $ newBlockHeader_Port (newPortClause $6 $8)     Nothing)}
             | generic '(' interface_list ')' ';' generic map '(' association_list ')' ';'                                                                           {BlockHeader (Just $ newBlockHeader_Generic (newGenericClause $1 $3) (Just $ newGenericMapAspect $6 $9)) Nothing}
             | generic '(' interface_list ')' ';'                                                                                                                    {BlockHeader (Just $ newBlockHeader_Generic (newGenericClause $1 $3) Nothing)                            Nothing}
             |                                                                               port '(' interface_list ')' ';'  port map '(' association_list ')' ';'  {BlockHeader Nothing                                                                                     (Just $ newBlockHeader_Port (newPortClause $1 $3)     (Just $ newPortMapAspect $6 $9))}
             |                                                                               port '(' interface_list ')' ';'                                         {BlockHeader Nothing                                                                                     (Just $ newBlockHeader_Port (newPortClause $1 $3)     Nothing)}
             | {- empty -}                                                                                                                                           {BlockHeader Nothing                                                                                     Nothing}

block_declarative_part :: { BlockDeclarativePart }
                       : {- empty -}                                    {[]}
                       | block_declarative_part block_declarative_item  {$2 : $1}

block_statement_part :: { BlockStatementPart }
                     : {- empty -}                                {[]}
                     | block_statement_part concurrent_statement  {$2 : $1}

process_declarative_part :: { ProcessDeclarativePart }
                         : {- empty -}                                        {[]}
                         | process_declarative_part process_declarative_item  {$2 : $1}

process_declarative_item :: { WrappedProcessDeclarativeItem }
                        : subprogram_specification ';'                                                                                {raisePosition ProcessDeclarative_SubprogramDeclaration $ newSubprogramDeclaration $1}
                        | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end identifier ';'  {raisePosition ProcessDeclarative_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Identifier $7)}
                        | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end str        ';'  {raisePosition ProcessDeclarative_SubprogramBody $ newSubprogramBody $1 $3 $5 (Just $ newDesignator_Operator $7)}
                        | subprogram_specification is subprogram_declarative_part begin subprogram_statement_part end            ';'  {raisePosition ProcessDeclarative_SubprogramBody $ newSubprogramBody $1 $3 $5 Nothing}
                        | type identifier is '(' enumeration_list ')' ';'                                                             {raisePosition ProcessDeclarative_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newEnumerationType $4 $5}
                        | type identifier is range range_definition ';'                                                               {raisePosition ProcessDeclarative_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUniversalType $4 $5}
                        | type identifier is range range_definition units identifier secondary_unit_declaration_list end units ';'    {raisePosition ProcessDeclarative_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newPhysicalType $4 $5 (newSimpleName $7) $8}
                        | type identifier is array '(' index_subtype_definition_list ')'   of subtype_indication ';'                  {raisePosition ProcessDeclarative_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newUnconstrainedArrayType  $4 $6 $9}
                        | type identifier is array '(' discrete_range_list ')'             of subtype_indication ';'                  {raisePosition ProcessDeclarative_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newConstrainedArrayType    $4 $6 $9}
                        | type identifier is record element_declaration_list end record ';'                                           {raisePosition ProcessDeclarative_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newRecordType $4 $5}
                        | type identifier is access subtype_indication ';'                                                            {raisePosition ProcessDeclarative_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newAccessType $4 $5}
                        | type identifier is file of name ';'                                                                         {raisePosition ProcessDeclarative_TypeDeclaration $ newFullTypeDeclaration $1 (newSimpleName $2) $ newFileType $4 $6}
                        | type identifier ';'                                                                                         {raisePosition ProcessDeclarative_TypeDeclaration $ newIncompleteTypeDefinition $1 $ newSimpleName $2}
                        | subtype identifier is subtype_indication ';'                                                                {raisePosition ProcessDeclarative_SubtypeDeclaration $ newSubtypeDeclaration $1 (newSimpleName $2) $4}
                        | constant identifier_list ':' subtype_indication ':=' expression ';'                                         {raisePosition ProcessDeclarative_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 (Just $6)}
                        | constant identifier_list ':' subtype_indication ';'                                                         {raisePosition ProcessDeclarative_ConstantDeclaration $ newConstantDeclaration $1 $2 $4 Nothing}
                        | variable identifier_list ':' subtype_indication ':=' expression ';'                                         {raisePosition ProcessDeclarative_VariableDeclaration $ newVariableDeclaration $1 $2 $4 (Just $6)}
                        | variable identifier_list ':' subtype_indication ';'                                                         {raisePosition ProcessDeclarative_VariableDeclaration $ newVariableDeclaration $1 $2 $4 Nothing}
                        | file identifier ':' subtype_indication is mode     expression ';'                                           {raisePosition ProcessDeclarative_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 (Just $6)  $7}
                        | file identifier ':' subtype_indication is          expression ';'                                           {raisePosition ProcessDeclarative_FileDeclaration $ newFileDeclaration $1 (newSimpleName $2) $4 Nothing    $6}
                        | alias identifier ':' subtype_indication is name ';'                                                         {raisePosition ProcessDeclarative_AliasDeclaration $ newAliasDeclaration $1 (newSimpleName $2) $4 $6}
                        | attribute identifier ':' name ';'                                                                           {raisePosition ProcessDeclarative_AttributeDeclaration $ newAttributeDeclaration $1 (newSimpleName $2) $4}
                        | attribute identifier of entity_designator_list  ':' entity_class is expression ';'                          {raisePosition ProcessDeclarative_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_List $4) $6 $8}
                        | attribute identifier of others                  ':' entity_class is expression ';'                          {raisePosition ProcessDeclarative_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_Others $4) $6 $8}
                        | attribute identifier of all                     ':' entity_class is expression ';'                          {raisePosition ProcessDeclarative_AttributeSpecification $ newAttributeSpecification $1 (newSimpleName $2) (newEntityNameList_All $4) $6 $8}
                        | use selected_name_list ';'                                                                                  {raisePosition ProcessDeclarative_UseClause $ newUseClause $1 $2}

process_statement_part :: { ProcessStatementPart }
                       : {- empty -}                                 {[]}
                       | process_statement_part sequential_statement {$2 : $1}

conditional_waveform_pairs :: { [WrappedConditionalWaveformPair] }
                           :                             waveform when expression else {[newConditionalWaveformPair $1 $3]}
                           | conditional_waveform_pairs  waveform when expression else {(newConditionalWaveformPair $2 $4) : $1}

selected_waveforms :: { [WrappedSelectedWaveformPair] }
                   :                         waveform when choices   {[newSelectedWaveformPair $1 $3]}
                   | selected_waveforms ','  waveform when choices   {(newSelectedWaveformPair $3 $5) : $1}

concurrent_statement_list :: { [WrappedConcurrentStatement] }
                          : {- empty -}                                    {[]}
                          | concurrent_statement_list concurrent_statement {$2 : $1}

------------------------------------------
-- Names and Expressions
------------------------------------------

-- name ::=
--    simple_name
--    | operator_symbol
--    | selected_name
--    | indexed_name
--    | slice_name
--    | attribute_name
-- simple_name ::= identifier
-- operator_symbol ::= string_literal
-- NOTE: Operator symbols are referred to by string literals in language
--       Symbol in quotes must match existing operator
--       ?? Add check for this in lexer
-- selected_name ::= prefix . suffix
-- indexed_name ::= prefix ( expression_list )
-- slice_name ::= prefix ( discrete_range )
-- attribute_name ::=
--    prefix ' attribute_designator [ ( /static/_expression ) ]
-- prefix ::=
--    name
--    | function_call
-- NOTE: One case of function_call is caught by 'name' parser
-- function_call ::=
--    /function/_name [ ( actual_parameter_part ) ]
-- NOTE: Only matches full case of this
-- actual_parameter_part ::= /parameter/_association_list
-- suffix ::=
--    simple_name
--    | character_literal
--    | operator_symbol
--    | __all__
-- attribute_designator ::= /attribute/_simple_name
name :: { WrappedName }
     : identifier                                                          {newName_Simple   $1}
     | str                                                                 {newName_Operator $1}
     | name                            '.' identifier                      {newName_Selected (newPrefix_Name $1) (newSuffix_Identifier $3)}
     | name                            '.' char                            {newName_Selected (newPrefix_Name $1) (newSuffix_Char       $3)}
     | name                            '.' str                             {newName_Selected (newPrefix_Name $1) (newSuffix_Operator   $3)}
     | name                            '.' all                             {newName_Selected (newPrefix_Name $1) (newSuffix_All        $3)}
     | name '(' association_list ')'   '.' identifier                      {newName_Selected (newPrefix_Function $1 $3) (newSuffix_Identifier   $6)}
     | name '(' association_list ')'   '.' char                            {newName_Selected (newPrefix_Function $1 $3) (newSuffix_Char         $6)}
     | name '(' association_list ')'   '.' str                             {newName_Selected (newPrefix_Function $1 $3) (newSuffix_Operator     $6)}
     | name '(' association_list ')'   '.' all                             {newName_Selected (newPrefix_Function $1 $3) (newSuffix_All          $6)}
     | name                            '(' expression_list ')'             {newName_Indexed (newPrefix_Name $1)         $3}
     | name '(' association_list ')'   '(' expression_list ')'             {newName_Indexed (newPrefix_Function $1 $3)  $6}
     | name                            '(' discrete_range ')'              {newName_Slice (newPrefix_Name $1)        $3}
     | name '(' association_list ')'   '(' discrete_range ')'              {newName_Slice (newPrefix_Function $1 $3) $6}
     | name                            '\'' identifier '(' expression ')'  {newName_Attribute $ newAttributeName (newPrefix_Name $1)         (newSimpleName $3) (Just $5)}
     | name '(' association_list ')'   '\'' identifier '(' expression ')'  {newName_Attribute $ newAttributeName (newPrefix_Function $1 $3)  (newSimpleName $6) (Just $8)}
     | name                            '\'' identifier                     {newName_Attribute $ newAttributeName (newPrefix_Name $1)         (newSimpleName $3) Nothing}
     | name '(' association_list ')'   '\'' identifier                     {newName_Attribute $ newAttributeName (newPrefix_Function $1 $3)  (newSimpleName $6) Nothing}

-- assocation_list ::= association_element { , association_element }
association_list :: { AssociationList }
                 : association_element                      {[$1]}
                 | association_list ',' association_element {$3 : $1}

-- association_element ::=
--    [ formal_part => ] actual_part
-- formal_part ::=
--    formal_designator
--    | /function/_name ( formal_designator )
-- actual_part ::=
--    actual_designator
--    | /function/_name ( actual_designator )
-- formal_designator ::=
--    /generic/_name
--    | /port/_name
--    | /parameter/_name
-- actual_designator ::=
--    expression
--    | /signal/_name
--    | /variable/_name
--    | __open__
association_element :: { WrappedAssociationElement }
                    : name '(' name ')' '=>' expression              {newAssociationElement (Just $ newFormalPart_Function $1 $3)   (newActualPart_Designator $ newActualDesignator_Expression $6)}
                    | name '(' name ')' '=>' name                    {newAssociationElement (Just $ newFormalPart_Function $1 $3)   (newActualPart_Designator $ newActualDesignator_Name $6)}
                    | name '(' name ')' '=>' open                    {newAssociationElement (Just $ newFormalPart_Function $1 $3)   (newActualPart_Designator $ newActualDesignator_Open $6)}
                    | name '(' name ')' '=>' name '(' expression ')' {newAssociationElement (Just $ newFormalPart_Function $1 $3)   (newActualPart_Function $6 $ newActualDesignator_Expression $8)}
                    | name '(' name ')' '=>' name '(' name ')'       {newAssociationElement (Just $ newFormalPart_Function $1 $3)   (newActualPart_Function $6 $ newActualDesignator_Name $8)}
                    | name '(' name ')' '=>' name '(' open ')'       {newAssociationElement (Just $ newFormalPart_Function $1 $3)   (newActualPart_Function $6 $ newActualDesignator_Open $8)}
                    | name              '=>' expression              {newAssociationElement (Just $ newFormalPart_Name $1)          (newActualPart_Designator $ newActualDesignator_Expression $3)}
                    | name              '=>' name                    {newAssociationElement (Just $ newFormalPart_Name $1)          (newActualPart_Designator $ newActualDesignator_Name $3)}
                    | name              '=>' open                    {newAssociationElement (Just $ newFormalPart_Name $1)          (newActualPart_Designator $ newActualDesignator_Open $3)}
                    | name              '=>' name '(' expression ')' {newAssociationElement (Just $ newFormalPart_Name $1)          (newActualPart_Function $3 $ newActualDesignator_Expression $5)}
                    | name              '=>' name '(' name ')'       {newAssociationElement (Just $ newFormalPart_Name $1)          (newActualPart_Function $3 $ newActualDesignator_Name $5)}
                    | name              '=>' name '(' open ')'       {newAssociationElement (Just $ newFormalPart_Name $1)          (newActualPart_Function $3 $ newActualDesignator_Open $5)}
                    |                        expression              {newAssociationElement Nothing                                 (newActualPart_Designator $ newActualDesignator_Expression $1)}
                    |                        name                    {newAssociationElement Nothing                                 (newActualPart_Designator $ newActualDesignator_Name $1)}
                    |                        open                    {newAssociationElement Nothing                                 (newActualPart_Designator $ newActualDesignator_Open $1)}
                    |                        name '(' expression ')' {newAssociationElement Nothing                                 (newActualPart_Function $1 $ newActualDesignator_Expression $3)}
                    |                        name '(' name ')'       {newAssociationElement Nothing                                 (newActualPart_Function $1 $ newActualDesignator_Name $3)}
                    |                        name '(' open ')'       {newAssociationElement Nothing                                 (newActualPart_Function $1 $ newActualDesignator_Open $3)}

-- expression_list ::= expression { , expression }
expression_list :: { [WrappedExpression] }
                :                     expression   {[$1]}
                | expression_list ',' expression   {$3 : $1}

-- expression ::=
--      relation { __and__ relation }
--    | relation { __or__ relation }
--    | relation { __xor__ relation }
--    | relation [ __nand__ relation ]
--    | relation [ __nor__ relation ]
expression :: { WrappedExpression }
           : relation                     {newExpression_Relation $1}
           | and_expression and relation  {newExpression_And   ($3:$1)}
           | or_expression or relation    {newExpression_Or    ($3:$1)}
           | xor_expression xor relation  {newExpression_Xor   ($3:$1)}
           | relation nand relation       {newExpression_Nand  $1 $3}
           | relation nor relation        {newExpression_Nor   $1 $3}

-- expression ::= relation { __and__ relation }
and_expression :: { [WrappedRelation] }
               : relation                    {[$1]}
               | and_expression and relation {$3 : $1}

-- expression ::= relation { __or__ relation }
or_expression :: { [WrappedRelation] }
               : relation                    {[$1]}
               | or_expression or relation {$3 : $1}

-- expression ::= relation { __xor__ relation }
xor_expression :: { [WrappedRelation] }
               : relation                    {[$1]}
               | xor_expression xor relation {$3 : $1}

-- relation ::=
--    simple_expression [ relational_operator simple_expression ]
relation :: { WrappedRelation }
         : simple_expression relational_operator simple_expression   {newRelation_Compare $1 $2 $3}
         | simple_expression                                         {newRelation_Term $1}

-- simple_expression ::=
--    [ sign ] term { adding_operator term }
-- NOTE: Implemented as
-- simple_expression ::= [ sign ] term adding_operation_list
simple_expression :: { WrappedSimpleExpression }
                  : '+' term adding_operation_list {newSimpleExpression (Just $ newSign $1 Positive) $2 $3}
                  | '-' term adding_operation_list {newSimpleExpression (Just $ newSign $1 Negative) $2 $3}
                  |     term adding_operation_list {newSimpleExpression Nothing $1 $2}

-- relational_operator ::= = | /= | < | <= | > | >=
relational_operator :: { WrappedRelationalOperator }
                    : '='  {newRelationalOperator $1 Relation_Equals}
                    | '/=' {newRelationalOperator $1 Relation_NotEquals}
                    | '<'  {newRelationalOperator $1 Relation_LessThan}
                    | '<=' {newRelationalOperator $1 Relation_LessThanOrEqual}
                    | '>'  {newRelationalOperator $1 Relation_GreaterThan}
                    | '>=' {newRelationalOperator $1 Relation_GreaterThanOrEqual}

-- term ::=
--    factor { multiplying_operator factor }
-- NOTE: Implemented as
-- term ::= factor multiplying_operation_list
term :: { WrappedTerm }
     : factor multiplying_operation_list {newTerm $1 $2}

-- adding_operation_list ::= { adding_operator term }
adding_operation_list :: { [WrappedAddingOperation] }
                      : {- empty -}                            {[]}
                      | adding_operation_list adding_operation {$2 : $1}

-- adding_operation ::= adding_operator term
-- adding_operator ::= + | - | &
adding_operation :: { WrappedAddingOperation }
                 : '+' term {newAddingOperation (newAddingOperator $1 Add) $2}
                 | '-' term {newAddingOperation (newAddingOperator $1 Minus) $2}
                 | '&' term {newAddingOperation (newAddingOperator $1 Concat) $2}

-- multiplying_operation_list ::= { multiplying_operator factor }
multiplying_operation_list :: { [WrappedMultiplyingOperation] }
                           : {- empty -}                                      {[]}
                           | multiplying_operation_list multiplying_operation {$2 : $1}

-- multiplying_operation ::= multiplying_operator factor
-- multiplying_operator ::= * | / | __mod__ | __rem__
multiplying_operation :: { WrappedMultiplyingOperation }
                      : '*' factor {newMultiplyingOperation (newMultiplyingOperator $1 Multiply) $2}
                      | '/' factor {newMultiplyingOperation (newMultiplyingOperator $1 Divide) $2}
                      | mod factor {newMultiplyingOperation (newMultiplyingOperator $1 Mod) $2}
                      | rem factor {newMultiplyingOperation (newMultiplyingOperator $1 Rem) $2}

-- factor ::=
--    primary [ ** primary ]
--    | __abs__ primary
--    | __not__ primary
factor :: { WrappedFactor }
       : primary '**' primary {newFactor_Pow $1 $3}
       | primary              {newFactor_Value $1}
       | abs primary          {newFactor_Abs $1 $2}
       | not primary          {newFactor_Not $1 $2}

-- primary ::=
--    name
--    | literal
--    | aggregate
--    | function_call
--    | qualified_expression
--    | type_conversion
--    | allocator
--    | ( expression )
-- aggregate ::=
--    ( element_assocation { , element_assocation } )
-- NOTE: One case of function_call is caught by 'name' parser
-- function_call ::=
--    /function/_name [ ( actual_parameter_part ) ]
-- NOTE: Only matches full case of this
-- actual_parameter_part ::= /parameter/_association_list
-- qualified_expression ::=
--    type_mark ' ( expresssion )
--    | type_mark ' aggregate
-- type_mark ::=
--    /type/_name
--    | /subtype/_name
-- type_conversion ::= type_mark ( expression )
-- allocator ::=
--    __new__ subtype_indication
--    | __new__ qualified_expression
-- subtype_indication ::=
--    [ /resolution_function/_name ] type_mark [ constraint ]
-- constraint ::=
--    range_constraint
--    | index_constraint
-- range_constraint ::= __range__ range_definition
-- index_constraint ::= ( discrete_range { , discrete_range } )
primary :: { WrappedPrimary }
        : name                                           {newPrimary_Name $1}
        | literal                                        {newPrimary_Literal $1}
        | '(' element_association_list ')'               {newPrimary_Aggregate $1 $2}
        | name '(' association_list ')'                  {newPrimary_FunctionCall $ newFunctionCall $1 $3}
        | name '\'' '(' expression ')'                   {newPrimary_QualifiedExpression $ newQualifiedExpression_Expression $1 $4}
        | name '\'' '(' element_association_list ')'     {newPrimary_QualifiedExpression $ newQualifiedExpression_Aggregate $1 $4}
        | name '(' expression ')'                        {newPrimary_TypeConversion $1 $3}
        | new name   name range range_definition         {newPrimary_Allocator $ newAllocator_SubtypeIndication $1 $ newSubtypeIndication (Just $2)   $3 $ Just $ newConstraint_Range $ newRangeConstraint $4 $5}
        | new        name range range_definition         {newPrimary_Allocator $ newAllocator_SubtypeIndication $1 $ newSubtypeIndication Nothing     $2 $ Just $ newConstraint_Range $ newRangeConstraint $3 $4}
        | new name   name '(' index_constraint_list ')'  {newPrimary_Allocator $ newAllocator_SubtypeIndication $1 $ newSubtypeIndication (Just $2)   $3 $ Just $ newConstraint_Index $4 $5}
        | new        name '(' index_constraint_list ')'  {newPrimary_Allocator $ newAllocator_SubtypeIndication $1 $ newSubtypeIndication Nothing     $2 $ Just $ newConstraint_Index $3 $4}
        | new name   name                                {newPrimary_Allocator $ newAllocator_SubtypeIndication $1 $ newSubtypeIndication (Just $2)   $3 Nothing}
        | new        name                                {newPrimary_Allocator $ newAllocator_SubtypeIndication $1 $ newSubtypeIndication Nothing     $2 Nothing}
        | new name '\'' '(' expression ')'               {newPrimary_Allocator $ newAllocator_Expression $1 $ newQualifiedExpression_Expression $2 $5}
        | new name '\'' '(' element_association_list ')' {newPrimary_Allocator $ newAllocator_Expression $1 $ newQualifiedExpression_Aggregate $2 $5}
        | '(' expression ')'                             {newPrimary_Expression $1 $2}

-- element_association_list ::= element_assocation { , element_assocation }
element_association_list :: { [WrappedElementAssociation] }
                         : element_association                                {[$1]}
                         | element_association_list ',' element_association   {$3 : $1}

-- element_association ::=
--    [ choices => ] expression
element_association :: { WrappedElementAssociation }
                    : choices '=>'  expression {newElementAssociation (Just $1) $3}
                    |               expression {newElementAssociation Nothing $1}

-- choices ::= choice { | choice }
choices :: { [WrappedChoice] }
        : choice              {[$1]}
        | choices '|' choice  {$3 : $1}

-- choice ::=
--    simple_expression
--    | discrete_range
--    | /element/_simple_name
--    | __others__
choice :: { WrappedChoice }
       : simple_expression {newChoice_Expression $1}
       | discrete_range    {newChoice_DiscreteRange $1}
       | identifier        {newChoice_ElementName $ newSimpleName $1}
       | others            {newChoice_Others $1}

-- index_constraint_list ::= discrete_range { , discrete_range }
index_constraint_list :: { [WrappedDiscreteRange] }
                      : discrete_range                            {[$1]}
                      | index_constraint_list ',' discrete_range  {$3 : $1}

-- range_definition ::=
--    /range/_attribute_name
--    | simple_expression direction simple_expression
-- direction ::=
--    __to__
--    | __downto__
-- attribute_name ::=
--    prefix ' attribute_designator [ ( /static/_expression ) ]
-- prefix ::=
--    name
--    | function_call
-- NOTE: One case of function_call is caught by 'name' parser
-- function_call ::=
--    /function/_name [ ( actual_parameter_part ) ]
-- NOTE: Only matches full case of this
range_definition :: { WrappedRange }
                 : name                            '\'' identifier '(' expression ')'  {newRange_AttributeName $ newAttributeName (newPrefix_Name $1)         (newSimpleName $3) (Just $5)}
                 | name '(' association_list ')'   '\'' identifier '(' expression ')'  {newRange_AttributeName $ newAttributeName (newPrefix_Function $1 $3)  (newSimpleName $6) (Just $8)}
                 | name                            '\'' identifier                     {newRange_AttributeName $ newAttributeName (newPrefix_Name $1)         (newSimpleName $3) Nothing}
                 | name '(' association_list ')'   '\'' identifier                     {newRange_AttributeName $ newAttributeName (newPrefix_Function $1 $3)  (newSimpleName $6) Nothing}
                 | simple_expression to simple_expression                              {newRange_Expression $1 (newDirection $2 To) $3}
                 | simple_expression downto simple_expression                          {newRange_Expression $1 (newDirection $2 Downto) $3}

-- discrete_range ::=
--    /discrete/_subtype_indication
--    | range_definition
-- subtype_indication ::=
--    [ /resolution_function/_name ] type_mark [ constraint ]
discrete_range :: { WrappedDiscreteRange }
               : name   name range range_definition         {newDiscreteRange_SubtypeIndication $ newSubtypeIndication (Just $1)   $2 $ Just $ newConstraint_Range $ newRangeConstraint $3 $4}
               |        name range range_definition         {newDiscreteRange_SubtypeIndication $ newSubtypeIndication Nothing     $1 $ Just $ newConstraint_Range $ newRangeConstraint $2 $3}
               | name   name '(' index_constraint_list ')'  {newDiscreteRange_SubtypeIndication $ newSubtypeIndication (Just $1)   $2 $ Just $ newConstraint_Index $3 $4}
               |        name '(' index_constraint_list ')'  {newDiscreteRange_SubtypeIndication $ newSubtypeIndication Nothing     $1 $ Just $ newConstraint_Index $2 $3}
               | name   name                                {newDiscreteRange_SubtypeIndication $ newSubtypeIndication (Just $1)   $2 Nothing}
               |        name                                {newDiscreteRange_SubtypeIndication $ newSubtypeIndication Nothing     $1 Nothing}
               | range_definition                           {newDiscreteRange_Range $1}

------------------------------------------
-- Literals
------------------------------------------

-- literal ::=
--    numeric_literal
--    | enumeration_literal
--    | string_literal
--    | bit_string_literal
--    | __null__
-- numeric_literal ::=
--    abstract_literal
--    | physical_literal
-- abstract_literal ::= decimal_literal | based_literal
-- NOTE: Abstract literals are lexed to integer or real universal types
-- physical_literal ::= [ abstract_literal ] /unit/_name
-- enumeration_literal ::= identifier | character_literal
literal :: { WrappedLiteral }
        : integer             {newLiteral_Numeric_Abstract_Integer   $1}
        | real                {newLiteral_Numeric_Abstract_Real      $1}
        | integer name        {newLiteral_Numeric_Physical $ newPhysicalLiteral (Just $ newAbstractLiteral_Integer $1) $2}
        | real name           {newLiteral_Numeric_Physical $ newPhysicalLiteral (Just $ newAbstractLiteral_Real $1) $2}
        | name                {newLiteral_Numeric_Physical $ newPhysicalLiteral Nothing $1}
        | identifier          {newLiteral_Enumeration_Identifier     $1}
        | char                {newLiteral_Enumeration_Char           $1}
        | str                 {newLiteral_String                     $1}
        | bitstr              {newLiteral_BitStr                     $1}
        | null                {newLiteral_Null                       $1}

------------------------------------------
-- Type Signature
------------------------------------------

{
v1987 :: Alex DesignFile
}
