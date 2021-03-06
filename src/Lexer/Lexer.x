{
module Lexer.Lexer where

import Data.Function ((&))
import Control.Monad.Except
         ( ExceptT
         , runExceptT
         )
import Control.Monad.Trans.State
         ( StateT
         , evalStateT
         )

import Lexer.Types.Token
import Lexer.Types.Error
import Lexer.Types.Monad
import Lexer.Types.PositionWrapper
import Lexer.Functions.Lex
import Lexer.Alex.Types
import Lexer.Alex.Functions
import Parser.Netlist.Types.Stores
         ( NetlistStore
         , emptyNetlistStore
         )
import Manager.Types.Error (ConverterError)
}

$upper_case_letter = [A-Z]
$lower_case_letter = [a-z]
@letter = $upper_case_letter | $lower_case_letter
$digit = [0-9]
@letter_or_digit = $digit | @letter
@underline_letter_or_digit = "_" | @letter_or_digit
@underline_digit = "_"? $digit
@integer = $digit @underline_digit*
@exponent = [Ee] [\+\-]? @integer
$special_character = [\# \& \  \' \( \) \* \, \- \. \/ \: \; \< \= \> \_ \| \+]
@special_character_w_quote = \" | $special_character
$other_special_character = [ \! \$ \% \@ \? \[ \] \\ \^ \` \{ \} \~]
$space_character = \ 
@basic_graphic_character = $upper_case_letter | $digit | $special_character | $space_character
@basic_character = @basic_graphic_character | [\ \t] -- other format effectors?
@graphic_character = @basic_graphic_character | $lower_case_letter | $other_special_character
@compound_delimiter = "=>" | "**" | ":=" | "/=" | ">=" | "<=" | "<>"
$delimiter = [\& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \| \n \ ]
-- Values
$binary = [01]
@underline_binary = "_"? $binary
@binary_value = $binary @underline_binary*
$base_three = [0-2]
@underline_base_three = "_"? $base_three
@base_three_value = $base_three @underline_base_three*
$base_four = [0-3]
@underline_base_four = "_"? $base_four
@base_four_value = $base_four @underline_base_four*
$base_five = [0-4]
@underline_base_five = "_"? $base_five
@base_five_value = $base_five @underline_base_five*
$base_six = [0-5]
@underline_base_six = "_"? $base_six
@base_six_value = $base_six @underline_base_six*
$base_seven = [0-6]
@underline_base_seven = "_"? $base_seven
@base_seven_value = $base_seven @underline_base_seven*
$octal = [0-7]
@underline_octal = "_"? $octal
@octal_value = $octal @underline_octal*
$base_nine = [0-8]
@underline_base_nine = "_"? $base_nine
@base_nine_value = $base_nine @underline_base_nine*
$base_ten = [0-9]
@underline_base_ten = "_"? $base_ten
@base_ten_value = $base_ten @underline_base_ten*
$base_eleven = [0-9aA]
@underline_base_eleven = "_"? $base_eleven
@base_eleven_value = $base_eleven @underline_base_eleven*
$base_twelve = [0-9abAB]
@underline_base_twelve = "_"? $base_twelve
@base_twelve_value = $base_twelve @underline_base_twelve*
$base_thirteen = [0-9a-cA-C]
@underline_base_thirteen = "_"? $base_thirteen
@base_thirteen_value = $base_thirteen @underline_base_thirteen*
$base_fourteen = [0-9a-dA-D]
@underline_base_fourteen = "_"? $base_fourteen
@base_fourteen_value = $base_fourteen @underline_base_fourteen*
$base_fifteen = [0-9a-eA-E]
@underline_base_fifteen = "_"? $base_fifteen
@base_fifteen_value = $base_fifteen @underline_base_fifteen*
$hex = [0-9a-fA-F]
@underline_hex = "_"? $hex
@hex_value = $hex @underline_hex*

tokens :-
-- Keywords
<0,identifier>               [Aa][Bb][Ss]                                          { makeToken (makeReserved Abs)            separator   }
<0,identifier>               [Aa][Cc][Cc][Ee][Ss][Ss]                              { makeToken (makeReserved Access)         separator   }
<0,identifier>               [Aa][Ff][Tt][Ee][Rr]                                  { makeToken (makeReserved After)          separator   }
<0,identifier>               [Aa][Ll][Ii][Aa][Ss]                                  { makeToken (makeReserved Alias)          separator   }
<0,identifier>               [Aa][Ll][Ll]                                          { makeToken (makeReserved All)            separator   }
<0,identifier>               [Aa][Nn][Dd]                                          { makeToken (makeReserved And)            separator   }
<0,identifier>               [Aa][Rr][Cc][Hh][Ii][Tt][Ee][Cc][Tt][Uu][Rr][Ee]      { makeToken (makeReserved Architecture)   separator   }
<0,identifier>               [Aa][Rr][Rr][Aa][Yy]                                  { makeToken (makeReserved Array)          separator   }
<0,identifier>               [Aa][Ss][Ss][Ee][Rr][Tt]                              { makeToken (makeReserved Assert)         separator   }
<0,identifier>               [Aa][Tt][Tt][Rr][Ii][Bb][Uu][Tt][Ee]                  { makeToken (makeReserved Attribute)      separator   }

<0,identifier>               [Bb][Ee][Gg][Ii][Nn]                                  { makeToken (makeReserved Begin)          separator   }
<0,identifier>               [Bb][Ll][Oo][Cc][Kk]                                  { makeToken (makeReserved Block)          separator   }
<0,identifier>               [Bb][Oo][Dd][Yy]                                      { makeToken (makeReserved Body)           separator   }
<0,identifier>               [Bb][Uu][Ff][Ff][Ee][Rr]                              { makeToken (makeReserved Buffer)         separator   }
<0,identifier>               [Bb][Uu][Ss]                                          { makeToken (makeReserved Bus)            separator   }

<0,identifier>               [Cc][Aa][Ss][Ee]                                      { makeToken (makeReserved Case)           separator   }
<0,identifier>               [Cc][Oo][Mm][Pp][Oo][Nn][Ee][Nn][Tt]                  { makeToken (makeReserved Component)      separator   }
<0,identifier>               [Cc][Oo][Nn][Ff][Ii][Gg][Uu][Rr][Aa][Tt][Ii][Oo][Nn]  { makeToken (makeReserved Configuration)  separator   }
<0,identifier>               [Cc][Oo][Nn][Ss][Tt][Aa][Nn][Tt]                      { makeToken (makeReserved Constant)       separator   }

<0,identifier>               [Dd][Ii][Ss][Cc][Oo][Nn][Nn][Ee][Cc][Tt]              { makeToken (makeReserved Disconnect)     separator   }
<0,identifier>               [Dd][Oo][Ww][Nn][Tt][Oo]                              { makeToken (makeReserved Downto)         separator   }

<0,identifier>               [Ee][Ll][Ss][Ee]                                      { makeToken (makeReserved Else)           separator   }
<0,identifier>               [Ee][Ll][Ss][Ii][Ff]                                  { makeToken (makeReserved Elsif)          separator   }
<0,identifier>               [Ee][Nn][Dd]                                          { makeToken (makeReserved End)            separator   }
<0,identifier>               [Ee][Nn][Tt][Ii][Tt][Yy]                              { makeToken (makeReserved Entity)         separator   }
<0,identifier>               [Ee][Xx][Ii][Tt]                                      { makeToken (makeReserved Exit)           separator   }

<0,identifier>               [Ff][Ii][Ll][Ee]                                      { makeToken (makeReserved File)           separator   }
<0,identifier>               [Ff][Oo][Rr]                                          { makeToken (makeReserved For)            separator   }
<0,identifier>               [Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]                      { makeToken (makeReserved Function)       separator   }

<0,identifier>               [Gg][Ee][Nn][Ee][Rr][Aa][Tt][Ee]                      { makeToken (makeReserved Generate)       separator   }
<0,identifier>               [Gg][Ee][Nn][Ee][Rr][Ii][Cc]                          { makeToken (makeReserved Generic)        separator   }
<0,identifier>               [Gg][Uu][Aa][Rr][Dd][Ee][Dd]                          { makeToken (makeReserved Guarded)        separator   }

<0,identifier>               [Ii][Ff]                                              { makeToken (makeReserved If)             separator   }
<0,identifier>               [Ii][Nn]                                              { makeToken (makeReserved In)             separator   }
<0,identifier>               [Ii][Nn][Oo][Uu][Tt]                                  { makeToken (makeReserved Inout)          separator   }
<0,identifier>               [Ii][Ss]                                              { makeToken (makeReserved Is)             separator   }

<0,identifier>               [Ll][Aa][Bb][Ee][Ll]                                  { makeToken (makeReserved Label)          separator   }
<0,identifier>               [Ll][Ii][Bb][Rr][Aa][Rr][Yy]                          { makeToken (makeReserved Library)        separator   }
<0,identifier>               [Ll][Ii][Nn][Kk][Aa][Gg][Ee]                          { makeToken (makeReserved Linkage)        separator   }
<0,identifier>               [Ll][Oo][Oo][Pp]                                      { makeToken (makeReserved Loop)           separator   }

<0,identifier>               [Mm][Aa][Pp]                                          { makeToken (makeReserved Map)            separator   }
<0,identifier>               [Mm][Oo][Dd]                                          { makeToken (makeReserved Mod)            separator   }

<0,identifier>               [Nn][Aa][Nn][Dd]                                      { makeToken (makeReserved Nand)           separator   }
<0,identifier>               [Nn][Ee][Ww]                                          { makeToken (makeReserved New)            separator   }
<0,identifier>               [Nn][Ee][Xx][Tt]                                      { makeToken (makeReserved Next)           separator   }
<0,identifier>               [Nn][Oo][Rr]                                          { makeToken (makeReserved Nor)            separator   }
<0,identifier>               [Nn][Oo][Tt]                                          { makeToken (makeReserved Not)            separator   }
<0,identifier>               [Nn][Uu][Ll][Ll]                                      { makeToken (makeReserved Null)           separator   }

<0,identifier>               [Oo][Ff]                                              { makeToken (makeReserved Of)             separator   }
<0,identifier>               [Oo][Nn]                                              { makeToken (makeReserved On)             separator   }
<0,identifier>               [Oo][Pp][Ee][Nn]                                      { makeToken (makeReserved Open)           separator   }
<0,identifier>               [Oo][Rr]                                              { makeToken (makeReserved Or)             separator   }
<0,identifier>               [Oo][Tt][Hh][Ee][Rr][Ss]                              { makeToken (makeReserved Others)         separator   }
<0,identifier>               [Oo][Uu][Tt]                                          { makeToken (makeReserved Out)            separator   }

<0,identifier>               [Pp][Aa][Cc][Kk][Aa][Gg][Ee]                          { makeToken (makeReserved Package)        separator   }
<0,identifier>               [Pp][Oo][Rr][Tt]                                      { makeToken (makeReserved Port)           separator   }
<0,identifier>               [Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee]                  { makeToken (makeReserved Procedure)      separator   }
<0,identifier>               [Pp][Rr][Oo][Cc][Ee][Ss][Ss]                          { makeToken (makeReserved Process)        separator   }

<0,identifier>               [Rr][Aa][Nn][Gg][Ee]                                  { makeToken (makeReserved Range)          separator   }
<0,identifier>               [Rr][Ee][Cc][Oo][Rr][Dd]                              { makeToken (makeReserved Record)         separator   }
<0,identifier>               [Rr][Ee][Gg][Ii][Ss][Tt][Ee][Rr]                      { makeToken (makeReserved Register)       separator   }
<0,identifier>               [Rr][Ee][Mm]                                          { makeToken (makeReserved Rem)            separator   }
<0,identifier>               [Rr][Ee][Pp][Oo][Rr][Tt]                              { makeToken (makeReserved Report)         separator   }
<0,identifier>               [Rr][Ee][Tt][Uu][Rr][Nn]                              { makeToken (makeReserved Return)         separator   }

<0,identifier>               [Ss][Ee][Ll][Ee][Cc][Tt]                              { makeToken (makeReserved Select)         separator   }
<0,identifier>               [Ss][Ee][Vv][Ee][Rr][Ii][Tt][Yy]                      { makeToken (makeReserved Severity)       separator   }
<0,identifier>               [Ss][Ii][Gg][Nn][Aa][Ll]                              { makeToken (makeReserved Signal)         separator   }
<0,identifier>               [Ss][Uu][Bb][Tt][Yy][Pp][Ee]                          { makeToken (makeReserved Subtype)        separator   }

<0,identifier>               [Tt][Hh][Ee][Nn]                                      { makeToken (makeReserved Then)           separator   }
<0,identifier>               [Tt][Oo]                                              { makeToken (makeReserved To)             separator   }
<0,identifier>               [Tt][Rr][Aa][Nn][Ss][Pp][Oo][Rr][Tt]                  { makeToken (makeReserved Transport)      separator   }
<0,identifier>               [Tt][Yy][Pp][Ee]                                      { makeToken (makeReserved Type)           separator   }

<0,identifier>               [Uu][Nn][Ii][Tt][Ss]                                  { makeToken (makeReserved Units)          separator   }
<0,identifier>               [Uu][Nn][Tt][Ii][Ll]                                  { makeToken (makeReserved Until)          separator   }
<0,identifier>               [Uu][Ss][Ee]                                          { makeToken (makeReserved Use)            separator   }

<0,identifier>               [Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee]                      { makeToken (makeReserved Variable)       separator   }

<0,identifier>               [Ww][Aa][Ii][Tt]                                      { makeToken (makeReserved Wait)           separator   }
<0,identifier>               [Ww][Hh][Ee][Nn]                                      { makeToken (makeReserved When)           separator   }
<0,identifier>               [Ww][Hh][Ii][Ll][Ee]                                  { makeToken (makeReserved While)          separator   }
<0,identifier>               [Ww][Ii][Tt][Hh]                                      { makeToken (makeReserved With)           separator   }

<0,identifier>               [Xx][Oo][Rr]                                          { makeToken (makeReserved Xor)            separator   }

-- Valid Identifier
<0,identifier>               @letter @underline_letter_or_digit *                  { makeToken makeIdentifier                separator   }
-- Invalid Identifier
-- <0,identifier>               "_" @underline_letter_or_digit *                      {

-- Valid Decimal
<0,identifier>               @integer (\. @integer)? @exponent?                    { makeToken makeDecimalLiteral            separator   }
-- Invalid Decimal
-- - Underscores in incorrect positions
-- -- Underscore at the start or end of value components
<0,identifier>               "_" ($digit | "_")+ (\. ($digit | "_")+)? ([Ee] [\+\-]? ($digit | "_")+)?                       { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ "_" (\. ($digit | "_")+)? ([Ee] [\+\-]? ($digit | "_")+)?                       { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ \. "_" ($digit | "_")+ ([Ee] [\+\-]? ($digit | "_")+)?                          { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ \. ($digit | "_")+ "_" ([Ee] [\+\-]? ($digit | "_")+)?                          { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ (\. ($digit | "_")+)? [Ee] "_"+ [\+\-]? ($digit | "_")+                         { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ (\. ($digit | "_")+)? [Ee] [\+\-]? "_" ($digit | "_")+                          { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ (\. ($digit | "_")+)? [Ee] [\+\-]? ($digit | "_")+ "_"                          { makeError errorDecimalLiteral }
-- -- Double underscores in the centre of value components
<0,identifier>               ($digit | "_")+ "__" ($digit | "_")+ (\. ($digit | "_")+)? ([Ee] [\+\-]? ($digit | "_")+)?      { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ \. ($digit | "_")+ "__" ($digit | "_")+ ([Ee] [\+\-]? ($digit | "_")+)?         { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ (\. ($digit | "_")+)? [Ee] [\+\-]? ($digit | "_")+ "__" ($digit | "_")+         { makeError errorDecimalLiteral }
-- - Empty components of values ?? Improve regex of these?
<0,identifier>               \. ($digit | "_")+ [Ee] [\+\-]? ($digit | "_")+                                                 { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ \. [Ee] [\+\-]? ($digit | "_")+                                                 { makeError errorDecimalLiteral }
<0,identifier>               ($digit | "_")+ \. ($digit | "_")+ [Ee] [\+\-]?                                                 { makeError errorDecimalLiteral }

-- Valid Based
<0,identifier>               "2#"  @binary_value        (\. @binary_value)?        "#" @exponent? { makeToken (makeBasedLiteral '#' 2.0)   separator   }
<0,identifier>               "3#"  @base_three_value    (\. @base_three_value)?    "#" @exponent? { makeToken (makeBasedLiteral '#' 3.0)   separator   }
<0,identifier>               "4#"  @base_four_value     (\. @base_four_value)?     "#" @exponent? { makeToken (makeBasedLiteral '#' 4.0)   separator   }
<0,identifier>               "5#"  @base_five_value     (\. @base_five_value)?     "#" @exponent? { makeToken (makeBasedLiteral '#' 5.0)   separator   }
<0,identifier>               "6#"  @base_six_value      (\. @base_six_value)?      "#" @exponent? { makeToken (makeBasedLiteral '#' 6.0)   separator   }
<0,identifier>               "7#"  @base_seven_value    (\. @base_seven_value)?    "#" @exponent? { makeToken (makeBasedLiteral '#' 7.0)   separator   }
<0,identifier>               "8#"  @octal_value         (\. @octal_value)?         "#" @exponent? { makeToken (makeBasedLiteral '#' 8.0)   separator   }
<0,identifier>               "9#"  @base_nine_value     (\. @base_nine_value)?     "#" @exponent? { makeToken (makeBasedLiteral '#' 9.0)   separator   }
<0,identifier>               "10#" @base_ten_value      (\. @base_ten_value)?      "#" @exponent? { makeToken (makeBasedLiteral '#' 10.0)  separator   }
<0,identifier>               "11#" @base_eleven_value   (\. @base_eleven_value)?   "#" @exponent? { makeToken (makeBasedLiteral '#' 11.0)  separator   }
<0,identifier>               "12#" @base_twelve_value   (\. @base_twelve_value)?   "#" @exponent? { makeToken (makeBasedLiteral '#' 12.0)  separator   }
<0,identifier>               "13#" @base_thirteen_value (\. @base_thirteen_value)? "#" @exponent? { makeToken (makeBasedLiteral '#' 13.0)  separator   }
<0,identifier>               "14#" @base_fourteen_value (\. @base_fourteen_value)? "#" @exponent? { makeToken (makeBasedLiteral '#' 14.0)  separator   }
<0,identifier>               "15#" @base_fifteen_value  (\. @base_fifteen_value)?  "#" @exponent? { makeToken (makeBasedLiteral '#' 15.0)  separator   }
<0,identifier>               "16#" @hex_value           (\. @hex_value)?           "#" @exponent? { makeToken (makeBasedLiteral '#' 16.0)  separator   }
<0,identifier>               "2:"  @binary_value        (\. @binary_value)?        ":" @exponent? { makeToken (makeBasedLiteral ':' 2.0)   separator   }
<0,identifier>               "3:"  @base_three_value    (\. @base_three_value)?    ":" @exponent? { makeToken (makeBasedLiteral ':' 3.0)   separator   }
<0,identifier>               "4:"  @base_four_value     (\. @base_four_value)?     ":" @exponent? { makeToken (makeBasedLiteral ':' 4.0)   separator   }
<0,identifier>               "5:"  @base_five_value     (\. @base_five_value)?     ":" @exponent? { makeToken (makeBasedLiteral ':' 5.0)   separator   }
<0,identifier>               "6:"  @base_six_value      (\. @base_six_value)?      ":" @exponent? { makeToken (makeBasedLiteral ':' 6.0)   separator   }
<0,identifier>               "7:"  @base_seven_value    (\. @base_seven_value)?    ":" @exponent? { makeToken (makeBasedLiteral ':' 7.0)   separator   }
<0,identifier>               "8:"  @octal_value         (\. @octal_value)?         ":" @exponent? { makeToken (makeBasedLiteral ':' 8.0)   separator   }
<0,identifier>               "9:"  @base_nine_value     (\. @base_nine_value)?     ":" @exponent? { makeToken (makeBasedLiteral ':' 9.0)   separator   }
<0,identifier>               "10:" @base_ten_value      (\. @base_ten_value)?      ":" @exponent? { makeToken (makeBasedLiteral ':' 10.0)  separator   }
<0,identifier>               "11:" @base_eleven_value   (\. @base_eleven_value)?   ":" @exponent? { makeToken (makeBasedLiteral ':' 11.0)  separator   }
<0,identifier>               "12:" @base_twelve_value   (\. @base_twelve_value)?   ":" @exponent? { makeToken (makeBasedLiteral ':' 12.0)  separator   }
<0,identifier>               "13:" @base_thirteen_value (\. @base_thirteen_value)? ":" @exponent? { makeToken (makeBasedLiteral ':' 13.0)  separator   }
<0,identifier>               "14:" @base_fourteen_value (\. @base_fourteen_value)? ":" @exponent? { makeToken (makeBasedLiteral ':' 14.0)  separator   }
<0,identifier>               "15:" @base_fifteen_value  (\. @base_fifteen_value)?  ":" @exponent? { makeToken (makeBasedLiteral ':' 15.0)  separator   }
<0,identifier>               "16:" @hex_value           (\. @hex_value)?           ":" @exponent? { makeToken (makeBasedLiteral ':' 16.0)  separator   }

-- Invalid based
-- - Underscores in incorrect positions
-- -- Underscore at the start or end of value components
-- -- Double underscores in the centre of value components
-- - Invalid characters
-- -- Invalid value
-- -- Invalid base
-- -- Invalid exponent
-- - Empty components of values
-- -- Empty Value
--<0,identifier>               ([0-9] | "10" | "11" | "12" | "13" | "14" | "15" | "16") "#"                              "#" @exponent?    { makeError errorBasedLiteral_ValueEmpty }
--<0,identifier>               ([0-9] | "10" | "11" | "12" | "13" | "14" | "15" | "16") ":"                              ":" @exponent?    { makeError errorBasedLiteral_ValueEmpty }
-- -- Missing Base
--<0,identifier>                                                                        "#" @hex_value (\. @hex_value)?  "#" @exponent?    { makeError errorBasedLiteral_BaseEmpty }
--<0,identifier>                                                                        ":" @hex_value (\. @hex_value)?  ":" @exponent?    { makeError errorBasedLiteral_BaseEmpty }
-- -- Missing Exponent
--<0,identifier>               ([0-9] | "10" | "11" | "12" | "13" | "14" | "15" | "16") "#" @hex_value (\. @hex_value)?  "#" [Ee] [\+\-]?  { makeError errorBasedLiteral_ExponentEmpty }
--<0,identifier>               ([0-9] | "10" | "11" | "12" | "13" | "14" | "15" | "16") ":" @hex_value (\. @hex_value)?  ":" [Ee] [\+\-]?  { makeError errorBasedLiteral_ExponentEmpty }

-- Valid Str-esque types
<0,identifier>               \' (@graphic_character | \") \'                       { makeToken makeCharLiteral               separator   }
<0,identifier>               \" (@graphic_character | [\"]{2})* \"                 { makeToken makeStrLiteral                separator   }
<0,identifier>               \% (@graphic_character | \" | [\%]{2})* \%            { makeToken makeStrLiteral                separator   }

-- Valid Bit Strings
<0,identifier>               [Bb] \" @binary_value \"                              { makeToken (makeBitStrLiteral BinBased)  separator   }
<0,identifier>               [Bb] "%" @binary_value "%"                            { makeToken (makeBitStrLiteral BinBased)  separator   }
<0,identifier>               [Oo] \" @octal_value \"                               { makeToken (makeBitStrLiteral OctBased)  separator   }
<0,identifier>               [Oo] "%" @octal_value "%"                             { makeToken (makeBitStrLiteral OctBased)  separator   }
<0,identifier>               [Xx] \" @hex_value \"                                 { makeToken (makeBitStrLiteral HexBased)  separator   }
<0,identifier>               [Xx] "%" @hex_value "%"                               { makeToken (makeBitStrLiteral HexBased)  separator   }
-- Invalid Bit Strings
-- - Invalid characters
<0,identifier>               [Bb] \" ( $binary | "_" )* ~ $binary ( $binary | "_" )* \"     { makeError errorBitStrLiteral }
<0,identifier>               [Bb] \" ( $binary | "_" )* ~ $binary \"                        { makeError errorBitStrLiteral }
<0,identifier>               [Bb] \" ~ $binary ( $binary | "_" )* \"                        { makeError errorBitStrLiteral }
<0,identifier>               [Oo] \" ( $octal | "_" )* ~ $octal ( $octal | "_" )* \"        { makeError errorBitStrLiteral }
<0,identifier>               [Oo] \" ( $octal | "_" )* ~ $octal \"                          { makeError errorBitStrLiteral }
<0,identifier>               [Oo] \" ~ $octal ( $octal | "_" )* \"                          { makeError errorBitStrLiteral }
<0,identifier>               [Xx] \" ( $hex | "_" )* ~ $hex ( $hex | "_" )* \"              { makeError errorBitStrLiteral }
<0,identifier>               [Xx] \" ( $hex | "_" )* ~ $hex \"                              { makeError errorBitStrLiteral }
<0,identifier>               [Xx] \" ~ $hex ( $hex | "_" )* \"                              { makeError errorBitStrLiteral }
<0,identifier>               [Bb] "%" ( $binary | "_" )* ~ $binary ( $binary | "_" )* "%"   { makeError errorBitStrLiteral }
<0,identifier>               [Bb] "%" ( $binary | "_" )* ~ $binary "%"                      { makeError errorBitStrLiteral }
<0,identifier>               [Bb] "%" ~ $binary ( $binary | "_" )* "%"                      { makeError errorBitStrLiteral }
<0,identifier>               [Oo] "%" ( $octal | "_" )* ~ $octal ( $octal | "_" )* "%"      { makeError errorBitStrLiteral }
<0,identifier>               [Oo] "%" ( $octal | "_" )* ~ $octal "%"                        { makeError errorBitStrLiteral }
<0,identifier>               [Oo] "%" ~ $octal ( $octal | "_" )* "%"                        { makeError errorBitStrLiteral }
<0,identifier>               [Xx] "%" ( $hex | "_" )* ~ $hex ( $hex | "_" )* "%"            { makeError errorBitStrLiteral }
<0,identifier>               [Xx] "%" ( $hex | "_" )* ~ $hex "%"                            { makeError errorBitStrLiteral }
<0,identifier>               [Xx] "%" ~ $hex ( $hex | "_" )* "%"                            { makeError errorBitStrLiteral }
-- - Incorrect underscores
<0,identifier>               [Bb] \" "_" ( $binary | "_" )* \"                              { makeError errorBitStrLiteral }
<0,identifier>               [Bb] \" ( $binary | "_" )* "_" \"                              { makeError errorBitStrLiteral }
<0,identifier>               [Bb] \" ( $binary | "_" )+ "__" ( $binary | "_" )+ \"          { makeError errorBitStrLiteral }
<0,identifier>               [Oo] \" "_" ( $octal | "_" )* \"                               { makeError errorBitStrLiteral }
<0,identifier>               [Oo] \" ( $octal | "_" )* "_" \"                               { makeError errorBitStrLiteral }
<0,identifier>               [Oo] \" ( $octal | "_" )+ "__" ( $octal | "_" )+ \"            { makeError errorBitStrLiteral }
<0,identifier>               [Xx] \" "_" ( $hex | "_" )* \"                                 { makeError errorBitStrLiteral }
<0,identifier>               [Xx] \" ( $hex | "_" )* "_" \"                                 { makeError errorBitStrLiteral }
<0,identifier>               [Xx] \" ( $hex | "_" )+ "__" ( $hex | "_" )+ \"                { makeError errorBitStrLiteral }
<0,identifier>               [Bb] "%" "_" ( $binary | "_" )* "%"                            { makeError errorBitStrLiteral }
<0,identifier>               [Bb] "%" ( $binary | "_" )* "_" "%"                            { makeError errorBitStrLiteral }
<0,identifier>               [Bb] "%" ( $binary | "_" )+ "__" ( $binary | "_" )+ "%"        { makeError errorBitStrLiteral }
<0,identifier>               [Oo] "%" "_" ( $octal | "_" )* "%"                             { makeError errorBitStrLiteral }
<0,identifier>               [Oo] "%" ( $octal | "_" )* "_" "%"                             { makeError errorBitStrLiteral }
<0,identifier>               [Oo] "%" ( $octal | "_" )+ "__" ( $octal | "_" )+ "%"          { makeError errorBitStrLiteral }
<0,identifier>               [Xx] "%" "_" ( $hex | "_" )* "%"                               { makeError errorBitStrLiteral }
<0,identifier>               [Xx] "%" ( $hex | "_" )* "_" "%"                               { makeError errorBitStrLiteral }
<0,identifier>               [Xx] "%" ( $hex | "_" )+ "__" ( $hex | "_" )+ "%"              { makeError errorBitStrLiteral }
-- - Invalid bitstring base
<0,identifier>               [^BbOoXx] \" .* \"                                             { makeError errorBitStrLiteralBase }
<0,identifier>               [^BbOoXx] "%" .* "%"                                           { makeError errorBitStrLiteralBase }
-- - Empty bitstring
<0,identifier>               [BbOoXx] \" \"                                                 { makeError errorBitStrLiteralEmpty }
<0,identifier>               [BbOoXx] "%" "%"                                               { makeError errorBitStrLiteralEmpty }

<0,separator,identifier>   "--".*                                                ;
<0,separator,identifier>   $white+                                               {                                               begin 0     }
<0,separator>              "=>"                                                  { makeToken (makeOperator Arrow)                identifier  }
<0,separator>              "**"                                                  { makeToken (makeOperator DoubleStar)           identifier  }
<0,separator>              ":="                                                  { makeToken (makeOperator VarAssign)            identifier  }
<0,separator>              "/="                                                  { makeToken (makeOperator Inequality)           identifier  }
<0,separator>              ">="                                                  { makeToken (makeOperator GreaterThanOrEqual)   identifier  }
<0,separator>              "<="                                                  { makeToken (makeOperator SignAssign)           identifier  }
<0,separator>              "<>"                                                  { makeToken (makeOperator Box)                  identifier  }
<0,separator>              "&"                                                   { makeToken (makeOperator Ampersand)            identifier  }
<0,separator>              "'"                                                   { makeToken (makeOperator Apostrophe)           identifier  }
<0,separator>              "("                                                   { makeToken (makeOperator LeftParen)            identifier  }
<0,separator>              ")"                                                   { makeToken (makeOperator RightParen)           identifier  }
<0,separator>              "*"                                                   { makeToken (makeOperator Star)                 identifier  }
<0,separator>              "+"                                                   { makeToken (makeOperator Plus)                 identifier  }
<0,separator>              ","                                                   { makeToken (makeOperator Comma)                identifier  }
<0,separator>              "-"                                                   { makeToken (makeOperator Hyphen)               identifier  }
<0,separator>              "."                                                   { makeToken (makeOperator Period)               identifier  }
<0,separator>              "/"                                                   { makeToken (makeOperator Slash)                identifier  }
<0,separator>              ":"                                                   { makeToken (makeOperator Colon)                identifier  }
<0,separator>              ";"                                                   { makeToken (makeOperator Semicolon)            identifier  }
<0,separator>              "<"                                                   { makeToken (makeOperator LessThan)             identifier  }
<0,separator>              "="                                                   { makeToken (makeOperator Equal)                identifier  }
<0,separator>              ">"                                                   { makeToken (makeOperator GreaterThan)          identifier  }
<0,separator>              "|"                                                   { makeToken (makeOperator Bar)                  identifier  }
-- Need to sort replacement characters

{
-- |General wrapper function to create a new token
makeToken :: CreateToken -> Int -> AlexInput -> Int -> Alex WrappedToken
makeToken newToken code (position, _, _, str) length = do
   let extractedStr = take length str
   alexSetStartCode code
   case newToken extractedStr of
     Right token ->
         return $ PosnWrapper { getPos = position, unPos = token }
     Left err ->
         alexError $ PosnWrapper { getPos = position, unPos = err }

-- |General wrapper function for errors picked up in regex
makeError :: CreateToken -> AlexInput -> Int -> Alex WrappedToken
makeError errorToken (position, _, _, str) length = do
   let extractedStr = take length str
   case errorToken extractedStr of
      Left err ->
         alexError $ PosnWrapper { getPos = position, unPos = err }

-- | Monadic call to lexer (requires continuation monad)
-- Used by happy to call the alex lexer
lexer :: (WrappedToken -> Alex a) -> Alex a
lexer cont = do
   token <- alexMonadScan
   cont token

-- | Basic call to lexer
-- Can be used for debug
-- Returns either error or list of tokens
lexerList :: String -> IO (Either ConverterError [Token])
lexerList str =
   let loop tknLst = do token <- alexMonadScan
                        case unPos token of
                           EOF -> return $ reverse tknLst
                           unPosToken -> loop (unPosToken:tknLst)
   in runExceptT $ evalStateT (runAlex str $ loop []) emptyNetlistStore

-- |Lexer scan
alexMonadScan :: Alex WrappedToken
alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> return $ PosnWrapper { getPos = AlexPn 0 1 0, unPos = EOF }
    AlexError (pos,_,_,_) -> alexError $ PosnWrapper { getPos = pos, unPos = GenericLexError }
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
token :: (AlexInput -> Int -> WrappedToken) -> AlexAction WrappedToken
token t input__ len = return (t input__ len)
}
