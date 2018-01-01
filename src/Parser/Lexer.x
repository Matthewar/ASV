{
module Parser.Lexer where
import Data.List.Split (splitOn)
import Parser.TokenTypes
import Data.Function ((&))
import Parser.Alex.Types
import Parser.Alex.Functions
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as ByteString (pack)
}

-- %wrapper "monad"

$upper_case_letter = [A-Z]
$lower_case_letter = [a-z]
@letter = $upper_case_letter | $lower_case_letter
$digit = [0-9]
@letter_or_digit = $digit | @letter
@underline_letter_or_digit = "_" | @letter_or_digit
@underline_digit = "_"? $digit
@integer = $digit @underline_digit*
@exponent = [Ee] [\+\-]? @integer
$binary = [01]
@underline_binary = "_"? $binary
@binary_value = $binary @underline_binary*
$octal = [0-7]
@underline_octal = "_"? $octal
@octal_value = $octal @underline_octal*
$hex = [0-9a-fA-F]
@underline_hex = "_"? $hex
@hex_value = $hex @underline_hex*
$special_character = [\# \& \  \' \( \) \* \, \- \. \/ \: \; \< \= \> \_ \| \+]
@special_character_w_quote = \" | $special_character
$other_special_character = [ \! \$ \% \@ \? \[ \] \\ \^ \` \{ \} \~]
$space_character = \ 
@basic_graphic_character = $upper_case_letter | $digit | $special_character | $space_character
@basic_character = @basic_graphic_character | [\ \t] -- other format effectors?
@graphic_character = @basic_graphic_character | $lower_case_letter | $other_special_character
@compound_delimiter = "=>" | "**" | ":=" | "/=" | ">=" | "<=" | "<>"
$delimiter = [\& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \| \n \ ]

tokens :-
-- Keywords
<0,nondelim>               [Aa][Bb][Ss]                                          { makeReserved Abs                        `andBegin`  separator   }
<0,nondelim>               [Aa][Cc][Cc][Ee][Ss][Ss]                              { makeReserved Access                     `andBegin`  separator   }
<0,nondelim>               [Aa][Ff][Tt][Ee][Rr]                                  { makeReserved After                      `andBegin`  separator   }
<0,nondelim>               [Aa][Ll][Ii][Aa][Ss]                                  { makeReserved Alias                      `andBegin`  separator   }
<0,nondelim>               [Aa][Ll][Ll]                                          { makeReserved All                        `andBegin`  separator   }
<0,nondelim>               [Aa][Nn][Dd]                                          { makeReserved And                        `andBegin`  separator   }
<0,nondelim>               [Aa][Rr][Cc][Hh][Ii][Tt][Ee][Cc][Tt][Uu][Rr][Ee]      { makeReserved Architecture               `andBegin`  separator   }
<0,nondelim>               [Aa][Rr][Rr][Aa][Yy]                                  { makeReserved Array                      `andBegin`  separator   }
<0,nondelim>               [Aa][Ss][Ss][Ee][Rr][Tt]                              { makeReserved Assert                     `andBegin`  separator   }
<0,nondelim>               [Aa][Tt][Tt][Rr][Ii][Bb][Uu][Tt][Ee]                  { makeReserved Attribute                  `andBegin`  separator   }

<0,nondelim>               [Bb][Ee][Gg][Ii][Nn]                                  { makeReserved Begin                      `andBegin`  separator   }
<0,nondelim>               [Bb][Ll][Oo][Cc][Kk]                                  { makeReserved Block                      `andBegin`  separator   }
<0,nondelim>               [Bb][Oo][Dd][Yy]                                      { makeReserved Body                       `andBegin`  separator   }
<0,nondelim>               [Bb][Uu][Ff][Ff][Ee][Rr]                              { makeReserved Buffer                     `andBegin`  separator   }
<0,nondelim>               [Bb][Uu][Ss]                                          { makeReserved Bus                        `andBegin`  separator   }

<0,nondelim>               [Cc][Aa][Ss][Ee]                                      { makeReserved Case                       `andBegin`  separator   }
<0,nondelim>               [Cc][Oo][Mm][Pp][Oo][Nn][Ee][Nn][Tt]                  { makeReserved Component                  `andBegin`  separator   }
<0,nondelim>               [Cc][Oo][Nn][Ff][Ii][Gg][Uu][Rr][Aa][Tt][Ii][Oo][Nn]  { makeReserved Configuration              `andBegin`  separator   }
<0,nondelim>               [Cc][Oo][Nn][Ss][Tt][Aa][Nn][Tt]                      { makeReserved Constant                   `andBegin`  separator   }

<0,nondelim>               [Dd][Ii][Ss][Cc][Oo][Nn][Nn][Ee][Cc][Tt]              { makeReserved Disconnect                 `andBegin`  separator   }
<0,nondelim>               [Dd][Oo][Ww][Nn][Tt][Oo]                              { makeReserved Downto                     `andBegin`  separator   }

<0,nondelim>               [Ee][Ll][Ss][Ee]                                      { makeReserved Else                       `andBegin`  separator   }
<0,nondelim>               [Ee][Ll][Ss][Ii][Ff]                                  { makeReserved Elsif                      `andBegin`  separator   }
<0,nondelim>               [Ee][Nn][Dd]                                          { makeReserved End                        `andBegin`  separator   }
<0,nondelim>               [Ee][Nn][Tt][Ii][Tt][Yy]                              { makeReserved Entity                     `andBegin`  separator   }
<0,nondelim>               [Ee][Xx][Ii][Tt]                                      { makeReserved Exit                       `andBegin`  separator   }

<0,nondelim>               [Ff][Ii][Ll][Ee]                                      { makeReserved File                       `andBegin`  separator   }
<0,nondelim>               [Ff][Oo][Rr]                                          { makeReserved For                        `andBegin`  separator   }
<0,nondelim>               [Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]                      { makeReserved Function                   `andBegin`  separator   }

<0,nondelim>               [Gg][Ee][Nn][Ee][Rr][Aa][Tt][Ee]                      { makeReserved Generate                   `andBegin`  separator   }
<0,nondelim>               [Gg][Ee][Nn][Ee][Rr][Ii][Cc]                          { makeReserved Generic                    `andBegin`  separator   }
<0,nondelim>               [Gg][Uu][Aa][Rr][Dd][Ee][Dd]                          { makeReserved Guarded                    `andBegin`  separator   }

<0,nondelim>               [Ii][Ff]                                              { makeReserved If                         `andBegin`  separator   }
<0,nondelim>               [Ii][Nn]                                              { makeReserved In                         `andBegin`  separator   }
<0,nondelim>               [Ii][Nn][Oo][Uu][Tt]                                  { makeReserved Inout                      `andBegin`  separator   }
<0,nondelim>               [Ii][Ss]                                              { makeReserved Is                         `andBegin`  separator   }

<0,nondelim>               [Ll][Aa][Bb][Ee][Ll]                                  { makeReserved Label                      `andBegin`  separator   }
<0,nondelim>               [Ll][Ii][Bb][Rr][Aa][Rr][Yy]                          { makeReserved Library                    `andBegin`  separator   }
<0,nondelim>               [Ll][Ii][Nn][Kk][Aa][Gg][Ee]                          { makeReserved Linkage                    `andBegin`  separator   }
<0,nondelim>               [Ll][Oo][Oo][Pp]                                      { makeReserved Loop                       `andBegin`  separator   }

<0,nondelim>               [Mm][Aa][Pp]                                          { makeReserved Map                        `andBegin`  separator   }
<0,nondelim>               [Mm][Oo][Dd]                                          { makeReserved Mod                        `andBegin`  separator   }

<0,nondelim>               [Nn][Aa][Nn][Dd]                                      { makeReserved Nand                       `andBegin`  separator   }
<0,nondelim>               [Nn][Ee][Ww]                                          { makeReserved New                        `andBegin`  separator   }
<0,nondelim>               [Nn][Ee][Xx][Tt]                                      { makeReserved Next                       `andBegin`  separator   }
<0,nondelim>               [Nn][Oo][Rr]                                          { makeReserved Nor                        `andBegin`  separator   }
<0,nondelim>               [Nn][Oo][Tt]                                          { makeReserved Not                        `andBegin`  separator   }
<0,nondelim>               [Nn][Uu][Ll][Ll]                                      { makeReserved Null                       `andBegin`  separator   }

<0,nondelim>               [Oo][Ff]                                              { makeReserved Of                         `andBegin`  separator   }
<0,nondelim>               [Oo][Nn]                                              { makeReserved On                         `andBegin`  separator   }
<0,nondelim>               [Oo][Pp][Ee][Nn]                                      { makeReserved Open                       `andBegin`  separator   }
<0,nondelim>               [Oo][Rr]                                              { makeReserved Or                         `andBegin`  separator   }
<0,nondelim>               [Oo][Tt][Hh][Ee][Rr][Ss]                              { makeReserved Others                     `andBegin`  separator   }
<0,nondelim>               [Oo][Uu][Tt]                                          { makeReserved Out                        `andBegin`  separator   }

<0,nondelim>               [Pp][Aa][Cc][Kk][Aa][Gg][Ee]                          { makeReserved Package                    `andBegin`  separator   }
<0,nondelim>               [Pp][Oo][Rr][Tt]                                      { makeReserved Port                       `andBegin`  separator   }
<0,nondelim>               [Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]                  { makeReserved Procedure                  `andBegin`  separator   }
<0,nondelim>               [Pp][Rr][Oo][Cc][Ee][Ss][Ss]                          { makeReserved Process                    `andBegin`  separator   }

<0,nondelim>               [Rr][Aa][Nn][Gg][Ee]                                  { makeReserved Range                      `andBegin`  separator   }
<0,nondelim>               [Rr][Ee][Cc][Oo][Rr][Dd]                              { makeReserved Record                     `andBegin`  separator   }
<0,nondelim>               [Rr][Ee][Gg][Ii][Ss][Tt][Ee][Rr]                      { makeReserved Register                   `andBegin`  separator   }
<0,nondelim>               [Rr][Ee][Mm]                                          { makeReserved Rem                        `andBegin`  separator   }
<0,nondelim>               [Rr][Ee][Pp][Oo][Rr][Tt]                              { makeReserved Report                     `andBegin`  separator   }
<0,nondelim>               [Rr][Ee][Tt][Uu][Rr][Nn]                              { makeReserved Return                     `andBegin`  separator   }

<0,nondelim>               [Ss][Ee][Ll][Ee][Cc][Tt]                              { makeReserved Select                     `andBegin`  separator   }
<0,nondelim>               [Ss][Ee][Vv][Ee][Rr][Ii][Tt][Yy]                      { makeReserved Severity                   `andBegin`  separator   }
<0,nondelim>               [Ss][Ii][Gg][Nn][Aa][Ll]                              { makeReserved Signal                     `andBegin`  separator   }
<0,nondelim>               [Ss][Uu][Bb][Tt][Yy][Pp][Ee]                          { makeReserved Subtype                    `andBegin`  separator   }

<0,nondelim>               [Tt][Hh][Ee][Nn]                                      { makeReserved Then                       `andBegin`  separator   }
<0,nondelim>               [Tt][Oo]                                              { makeReserved To                         `andBegin`  separator   }
<0,nondelim>               [Tt][Rr][Aa][Nn][Ss][Pp][Oo][Rr][Tt]                  { makeReserved Transport                  `andBegin`  separator   }
<0,nondelim>               [Tt][Yy][Pp][Ee]                                      { makeReserved Type                       `andBegin`  separator   }

<0,nondelim>               [Uu][Nn][Ii][Tt][Ss]                                  { makeReserved Units                      `andBegin`  separator   }
<0,nondelim>               [Uu][Nn][Tt][Ii][Ll]                                  { makeReserved Until                      `andBegin`  separator   }
<0,nondelim>               [Uu][Ss][Ee]                                          { makeReserved Use                        `andBegin`  separator   }

<0,nondelim>               [Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee]                      { makeReserved Variable                   `andBegin`  separator   }

<0,nondelim>               [Ww][Aa][Ii][Tt]                                      { makeReserved Wait                       `andBegin`  separator   }
<0,nondelim>               [Ww][Hh][Ee][Nn]                                      { makeReserved When                       `andBegin`  separator   }
<0,nondelim>               [Ww][Hh][Ii][Ll][Ee]                                  { makeReserved While                      `andBegin`  separator   }
<0,nondelim>               [Ww][Ii][Tt][Hh]                                      { makeReserved With                       `andBegin`  separator   }

<0,nondelim>               [Xx][Oo][Rr]                                          { makeReserved Xor                        `andBegin`  separator   }

-- Valid Identifier
<0,nondelim>               @letter @underline_letter_or_digit *                  { (\alexIn -> makeIdentifier alexIn)      `andBegin`  separator   }
-- Invalid Identifier
-- <0,nondelim>               "_" @underline_letter_or_digit *                      {

-- Valid Decimal
<0,nondelim>               @integer (\. @integer)? @exponent?                    { (\alexIn -> makeDecimalLiteral alexIn)  `andBegin`  separator   }
-- Invalid Decimal
-- - Underscores in incorrect positions
-- -- Underscore at the start or end of value components
<0,nondelim>               "_" ($digit | "_")+ (\. ($digit | "_")+)? ([Ee] [\+\-]? ($digit | "_")+)?                       { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ "_" (\. ($digit | "_")+)? ([Ee] [\+\-]? ($digit | "_")+)?                       { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ \. "_" ($digit | "_")+ ([Ee] [\+\-]? ($digit | "_")+)?                          { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ \. ($digit | "_")+ "_" ([Ee] [\+\-]? ($digit | "_")+)?                          { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ (\. ($digit | "_")+)? [Ee] "_"+ [\+\-]? ($digit | "_")+                         { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ (\. ($digit | "_")+)? [Ee] [\+\-]? "_" ($digit | "_")+                          { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ (\. ($digit | "_")+)? [Ee] [\+\-]? ($digit | "_")+ "_"                          { errorDecimalLiteral }
-- -- Double underscores in the centre of value components
<0,nondelim>               ($digit | "_")+ "__" ($digit | "_")+ (\. ($digit | "_")+)? ([Ee] [\+\-]? ($digit | "_")+)?      { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ \. ($digit | "_")+ "__" ($digit | "_")+ ([Ee] [\+\-]? ($digit | "_")+)?         { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ (\. ($digit | "_")+)? [Ee] [\+\-]? ($digit | "_")+ "__" ($digit | "_")+         { errorDecimalLiteral }
-- - Empty components of values ?? Improve regex of these?
<0,nondelim>               \. ($digit | "_")+ [Ee] [\+\-]? ($digit | "_")+                                                 { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ \. [Ee] [\+\-]? ($digit | "_")+                                                 { errorDecimalLiteral }
<0,nondelim>               ($digit | "_")+ \. ($digit | "_")+ [Ee] [\+\-]?                                                 { errorDecimalLiteral }

-- Valid Based
<0,nondelim>               [0-9]+ "#" @hex_value (\. @hex_value)? "#" @exponent?   { (\alexIn -> makeBasedLiteral '#' alexIn)   `andBegin`  separator   }
<0,nondelim>               [0-9]+ ":" @hex_value (\. @hex_value)? ":" @exponent?   { (\alexIn -> makeBasedLiteral ':' alexIn)   `andBegin`  separator   }

-- Valid Str-esque types
<0,nondelim>               \' (@graphic_character | \") \'                       { (\alexIn -> makeCharLiteral alexIn)     `andBegin`  separator   }
<0,nondelim>               \" (@graphic_character | [\"]{2})* \"                 { (\alexIn -> makeStrLiteral alexIn)      `andBegin`  separator   }
<0,nondelim>               \% (@graphic_character | \" | [\%]{2})* \%            { (\alexIn -> makeStrLiteral alexIn)      `andBegin`  separator   }

-- Valid Bit Strings
<0,nondelim>               [Bb] \" @binary_value \"                              { (\alexIn -> makeBitStrLiteral BinBased alexIn)   `andBegin`  separator   }
<0,nondelim>               [Bb] "%" @binary_value "%"                            { (\alexIn -> makeBitStrLiteral BinBased alexIn)   `andBegin`  separator   }
<0,nondelim>               [Oo] \" @octal_value \"                               { (\alexIn -> makeBitStrLiteral OctBased alexIn)   `andBegin`  separator   }
<0,nondelim>               [Oo] "%" @octal_value "%"                             { (\alexIn -> makeBitStrLiteral OctBased alexIn)   `andBegin`  separator   }
<0,nondelim>               [Xx] \" @hex_value \"                                 { (\alexIn -> makeBitStrLiteral HexBased alexIn)   `andBegin`  separator   }
<0,nondelim>               [Xx] "%" @hex_value "%"                               { (\alexIn -> makeBitStrLiteral HexBased alexIn)   `andBegin`  separator   }
-- Invalid Bit Strings
-- - Invalid characters
<0,nondelim>               [Bb] \" ( $binary | "_" )* ~ $binary ( $binary | "_" )* \"     { errorBitStrLiteral }
<0,nondelim>               [Bb] \" ( $binary | "_" )* ~ $binary \"                        { errorBitStrLiteral }
<0,nondelim>               [Bb] \" ~ $binary ( $binary | "_" )* \"                        { errorBitStrLiteral }
<0,nondelim>               [Oo] \" ( $octal | "_" )* ~ $octal ( $octal | "_" )* \"        { errorBitStrLiteral }
<0,nondelim>               [Oo] \" ( $octal | "_" )* ~ $octal \"                          { errorBitStrLiteral }
<0,nondelim>               [Oo] \" ~ $octal ( $octal | "_" )* \"                          { errorBitStrLiteral }
<0,nondelim>               [Xx] \" ( $hex | "_" )* ~ $hex ( $hex | "_" )* \"              { errorBitStrLiteral }
<0,nondelim>               [Xx] \" ( $hex | "_" )* ~ $hex \"                              { errorBitStrLiteral }
<0,nondelim>               [Xx] \" ~ $hex ( $hex | "_" )* \"                              { errorBitStrLiteral }
<0,nondelim>               [Bb] "%" ( $binary | "_" )* ~ $binary ( $binary | "_" )* "%"   { errorBitStrLiteral }
<0,nondelim>               [Bb] "%" ( $binary | "_" )* ~ $binary "%"                      { errorBitStrLiteral }
<0,nondelim>               [Bb] "%" ~ $binary ( $binary | "_" )* "%"                      { errorBitStrLiteral }
<0,nondelim>               [Oo] "%" ( $octal | "_" )* ~ $octal ( $octal | "_" )* "%"      { errorBitStrLiteral }
<0,nondelim>               [Oo] "%" ( $octal | "_" )* ~ $octal "%"                        { errorBitStrLiteral }
<0,nondelim>               [Oo] "%" ~ $octal ( $octal | "_" )* "%"                        { errorBitStrLiteral }
<0,nondelim>               [Xx] "%" ( $hex | "_" )* ~ $hex ( $hex | "_" )* "%"            { errorBitStrLiteral }
<0,nondelim>               [Xx] "%" ( $hex | "_" )* ~ $hex "%"                            { errorBitStrLiteral }
<0,nondelim>               [Xx] "%" ~ $hex ( $hex | "_" )* "%"                            { errorBitStrLiteral }
-- - Incorrect underscores
<0,nondelim>               [Bb] \" "_" ( $binary | "_" )* \"                              { errorBitStrLiteral }
<0,nondelim>               [Bb] \" ( $binary | "_" )* "_" \"                              { errorBitStrLiteral }
<0,nondelim>               [Bb] \" ( $binary | "_" )+ "__" ( $binary | "_" )+ \"          { errorBitStrLiteral }
<0,nondelim>               [Oo] \" "_" ( $octal | "_" )* \"                               { errorBitStrLiteral }
<0,nondelim>               [Oo] \" ( $octal | "_" )* "_" \"                               { errorBitStrLiteral }
<0,nondelim>               [Oo] \" ( $octal | "_" )+ "__" ( $octal | "_" )+ \"            { errorBitStrLiteral }
<0,nondelim>               [Xx] \" "_" ( $hex | "_" )* \"                                 { errorBitStrLiteral }
<0,nondelim>               [Xx] \" ( $hex | "_" )* "_" \"                                 { errorBitStrLiteral }
<0,nondelim>               [Xx] \" ( $hex | "_" )+ "__" ( $hex | "_" )+ \"                { errorBitStrLiteral }
<0,nondelim>               [Bb] "%" "_" ( $binary | "_" )* "%"                            { errorBitStrLiteral }
<0,nondelim>               [Bb] "%" ( $binary | "_" )* "_" "%"                            { errorBitStrLiteral }
<0,nondelim>               [Bb] "%" ( $binary | "_" )+ "__" ( $binary | "_" )+ "%"        { errorBitStrLiteral }
<0,nondelim>               [Oo] "%" "_" ( $octal | "_" )* "%"                             { errorBitStrLiteral }
<0,nondelim>               [Oo] "%" ( $octal | "_" )* "_" "%"                             { errorBitStrLiteral }
<0,nondelim>               [Oo] "%" ( $octal | "_" )+ "__" ( $octal | "_" )+ "%"          { errorBitStrLiteral }
<0,nondelim>               [Xx] "%" "_" ( $hex | "_" )* "%"                               { errorBitStrLiteral }
<0,nondelim>               [Xx] "%" ( $hex | "_" )* "_" "%"                               { errorBitStrLiteral }
<0,nondelim>               [Xx] "%" ( $hex | "_" )+ "__" ( $hex | "_" )+ "%"              { errorBitStrLiteral }
-- - Invalid bitstring base
<0,nondelim>               [^BbOoXx] \" .* \"                                             { errorBitStrLiteralBase }
<0,nondelim>               [^BbOoXx] "%" .* "%"                                           { errorBitStrLiteralBase }
-- - Empty bitstring
<0,nondelim>               [BbOoXx] \" \"                                                 { errorBitStrLiteralEmpty }
<0,nondelim>               [BbOoXx] "%" "%"                                               { errorBitStrLiteralEmpty }

<0,separator,identifier>   "--".*                                                ;
<0,separator,identifier>   $white+                                               {                                         begin       0           }
<0,separator>              "=>"                                                  { makeOperator Arrow                      `andBegin`  identifier  }
<0,separator>              "**"                                                  { makeOperator DoubleStar                 `andBegin`  identifier  }
<0,separator>              ":="                                                  { makeOperator VarAssign                  `andBegin`  identifier  }
<0,separator>              "/="                                                  { makeOperator Inequality                 `andBegin`  identifier  }
<0,separator>              ">="                                                  { makeOperator GreaterThanOrEqual         `andBegin`  identifier  }
<0,separator>              "<="                                                  { makeOperator SignAssign                 `andBegin`  identifier  }
<0,separator>              "<>"                                                  { makeOperator Box                        `andBegin`  identifier  }
<0,separator>              "&"                                                   { makeOperator Ampersand                  `andBegin`  identifier  }
<0,separator>              "'"                                                   { makeOperator Apostrophe                 `andBegin`  identifier  }
<0,separator>              "("                                                   { makeOperator LeftParen                  `andBegin`  identifier  }
<0,separator>              ")"                                                   { makeOperator RightParen                 `andBegin`  identifier  }
<0,separator>              "*"                                                   { makeOperator Star                       `andBegin`  identifier  }
<0,separator>              "+"                                                   { makeOperator Plus                       `andBegin`  identifier  }
<0,separator>              ","                                                   { makeOperator Comma                      `andBegin`  identifier  }
<0,separator>              "-"                                                   { makeOperator Hyphen                     `andBegin`  identifier  }
<0,separator>              "."                                                   { makeOperator Period                     `andBegin`  identifier  }
<0,separator>              "/"                                                   { makeOperator Slash                      `andBegin`  identifier  }
<0,separator>              ":"                                                   { makeOperator Colon                      `andBegin`  identifier  }
<0,separator>              ";"                                                   { makeOperator Semicolon                  `andBegin`  identifier  }
<0,separator>              "<"                                                   { makeOperator LessThan                   `andBegin`  identifier  }
<0,separator>              "="                                                   { makeOperator Equal                      `andBegin`  identifier  }
<0,separator>              ">"                                                   { makeOperator GreaterThan                `andBegin`  identifier  }
<0,separator>              "|"                                                   { makeOperator Bar                        `andBegin`  identifier  }
-- Need to sort replacement characters

{
-- The token type:
data Token = Keyword ReservedWord
                   | Operator OperatorType
                   | Identifier String
                   | Literal LitType
                   | EOF
                   deriving (Eq,Show)

makeReserved :: ReservedWord -> AlexInput -> Int -> Alex Token
makeReserved keyword (position, _, _, _) _ =
   let keyType = Keyword keyword
   in return $ keyType

makeIdentifier :: AlexInput -> Int -> Alex Token
makeIdentifier (position, _, _, str) length =
   let identifier = take length str
   in return $ Identifier identifier

makeDecimalLiteral :: AlexInput -> Int -> Alex Token
makeDecimalLiteral (position, _, _, str) length =
   let extractedStr = take length str
       formattedStr = filter (\char -> char /= '_') extractedStr
       isReal = elem '.' formattedStr
       convertToLiteralType =
         if isReal then Univ_Real
         else Univ_Int . floor
       checkError Nothing =
         let errorType =
               if isReal then LexErr_UniversalReal_OutOfBounds
               else LexErr_UniversalInt_OutOfBounds
         in alexError $ errorType extractedStr position
       checkError (Just value) =
         value
         & convertToLiteralType
         & Literal
         & return
   in formattedStr
      & readMaybe
      & checkError

errorDecimalLiteral :: AlexInput -> Int -> Alex Token
errorDecimalLiteral (position, _, _, str) length =
   take length str
   & \err -> LexErr_DecimalLiteral_InvalidFormat err position
   & alexError

makeBasedLiteral :: Char -> AlexInput -> Int -> Alex Token
makeBasedLiteral separator (position, _, _, str) length = do
   let basedStr = take length str
   (base,value,exponent) <- case splitOn [separator] basedStr of
      (base:value:('E':exponent):[]) -> return (base,value,exponent)
      (base:value:"":[]) -> return (base,value,"0")
      _ -> alexError $ LexErr_BasedLiteral_InvalidValue basedStr position -- ?? This error is incorrect
   baseInt <- return $ read base
   exponentInt <- return $ read exponent
   let convertBasedUnits ans iter (unit:units) =
         (read [unit]) * (baseInt ** iter)
         & \currentDigitWeight -> currentDigitWeight + ans
         & \newAns -> convertBasedUnits newAns (iter+1) units
       convertBasedUnits ans _ [] = ans
       convertBasedDecimals ans iter (decimal:decimals) =
         (read [decimal]) * (baseInt ** iter)
         & \currentDigitWeight -> currentDigitWeight + ans
         & \newAns -> convertBasedDecimals newAns (iter-1) decimals
       convertBasedDecimals ans _ [] = ans
       convertBasedFloat number =
         case splitOn "." number of
            (units:decimals:[]) -> (convertBasedUnits 0 0 $ reverse units) + (convertBasedDecimals 0 (-1) decimals)
            (units:[]) -> convertBasedUnits 0 0 $ reverse units
   if elem baseInt [2..16] then
      convertBasedFloat value
      & \numericValue -> numericValue * (baseInt ^ exponentInt)
--      & Decimal
      & Univ_Real
      & Literal
      & return
   else alexError $ LexErr_BasedLiteral_InvalidBaseValue (floor baseInt) basedStr position

makeCharLiteral :: AlexInput -> Int -> Alex Token
makeCharLiteral (position, _, _, ('\'':char:'\'':[])) _ =
   return $ Literal $ Character char

makeStrLiteral :: AlexInput -> Int -> Alex Token
makeStrLiteral (position, _, _, str) length =
   let completeStr = take length str
       container :: Char
       container = head completeStr
       strContents :: String
       strContents = -- Remove first and last elements
         completeStr & tail & init
       filterContainer :: String -> String -> String
       filterContainer (item1:item2:rest) outputStr =
         if item1 == item2 && item1 == container then
            filterContainer rest (item1:outputStr)
         else filterContainer (item2:rest) (item1:outputStr)
       filterContainer (item:rest) outputStr =
         filterContainer rest (item:outputStr)
       filterContainer [] outputStr = reverse outputStr
   in filterContainer strContents []
      & Str
      & Literal
      & return

makeBitStrLiteral :: LiteralBase -> AlexInput -> Int -> Alex Token
makeBitStrLiteral base (position, _, _, str) length =
   take length str
   & init
   & \(_:_:bitString) ->
      filter (\c -> c /= '_') bitString
      & ByteString.pack
      & BitStr base
      & Literal
      & return

errorBitStrLiteral :: AlexInput -> Int -> Alex Token
errorBitStrLiteral (position, _, _, str) length =
   let bitStr = take length str
   in alexError $ LexErr_BitStrLiteral_InvalidStr bitStr position

errorBitStrLiteralBase :: AlexInput -> Int -> Alex Token
errorBitStrLiteralBase (position, _, _, str) length =
   let bitStr = take length str
       (base:_) = bitStr
   in alexError $ LexErr_BitStrLiteral_InvalidBase base bitStr position

errorBitStrLiteralEmpty :: AlexInput -> Int -> Alex Token
errorBitStrLiteralEmpty (position, _, _, str) length =
   let bitStr = take length str
   in alexError $ LexErr_BitStrLiteral_EmptyStr bitStr position

makeOperator :: OperatorType -> AlexInput -> Int -> Alex Token
makeOperator op (position, _, _, _) _ =
   return $ Operator op

-- | Monadic call to lexer (requires continuation monad)
-- Used by happy to call the alex lexer
lexer :: (Token -> Alex a) -> Alex a
lexer cont = do
   token <- alexMonadScan
   cont token

-- | Basic call to lexer
-- Can be used for debug
-- Returns either error or list of tokens
lexerList :: String -> Either LexerError [Token]
lexerList str = runAlex str $ do
   let loop tknLst = do token <- alexMonadScan
                        case token of
                           EOF -> return $ reverse tknLst
                           token -> loop (token:tknLst)
   loop []

-- | Lexer scan
alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> return EOF
    AlexError (pos,_,_,_) -> alexError $ GenericLexError pos
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__' len action -> do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len

-- | Ignore this token and scan another one
--skip :: AlexAction result
skip _input _len = alexMonadScan

-- | Ignore this token, but set the start code to a new value
--begin :: Int -> AlexAction result
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- | Perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

-- | Return token
token :: (AlexInput -> Int -> Token) -> AlexAction Token
token t input__ len = return (t input__ len)
}
