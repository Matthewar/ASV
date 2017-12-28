{
module Parser.Lexer where
import Data.List.Split (splitOn)
import Parser.TokenTypes
import Data.Function ((&))
import Parser.Alex.Types
import Parser.Alex.Functions
}

-- %wrapper "monad"

$upper_case_letter = [A-Z]
$lower_case_letter = [a-z]
@letter = $upper_case_letter | $lower_case_letter
$digit = [0-9]
@letter_or_digit = $digit | @letter
@underline_letter_or_digit = "_" | @letter_or_digit
@underline_digit = "_" | $digit
@integer = $digit @underline_digit*
@exponent = [Ee] [\+\-]? @integer
$binary = [01]
@underline_binary = "_" | $binary
@binary_value = $binary @underline_binary*
$octal = [0-7]
@underline_octal = "_" | $octal
@octal_value = $octal @underline_octal*
$hex = [0-9a-fA-F]
@underline_hex = "_" | $hex
@hex_value = $hex @underline_hex*
$special_character = [\# \& \  \' \( \) \* \, \- \. \/ \: \; \< \= \> \_ \|]
@special_character_w_quote = \" | $special_character
$other_special_character = [ \! \$ \% \@ \? \[ \] \\ \^ \` \{ \} \~]
$space_character = \ 
@basic_graphic_character = $upper_case_letter | $digit | $special_character | $space_character
@basic_character = @basic_graphic_character | [\ \t] -- other format effectors?
@graphic_character = @basic_graphic_character | $lower_case_letter | $other_special_character
@compound_delimiter = "=>" | "**" | ":=" | "/=" | ">=" | "<=" | "<>"
$delimiter = [\& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \| \n \ ]

tokens :-
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

<0,nondelim>               @letter @underline_letter_or_digit *                  { (\alexIn -> makeIdentifier alexIn)              `andBegin`  separator   }

<0,nondelim>               @integer (\. @integer)? @exponent?                    { (\alexIn -> makeDecimalLiteral alexIn)          `andBegin`  separator   }

<0,nondelim>               [0-9]+ "#" @hex_value (\. @hex_value)? "#" @exponent?   { (\alexIn -> makeBasedLiteral '#' alexIn)   `andBegin`  separator   }
<0,nondelim>               [0-9]+ ":" @hex_value (\. @hex_value)? ":" @exponent?   { (\alexIn -> makeBasedLiteral ':' alexIn)   `andBegin`  separator   }

<0,nondelim>               \' @graphic_character \'                              { (\alexIn -> makeCharLiteral alexIn)             `andBegin`  separator   }
<0,nondelim>               \" (@graphic_character | [\"]{2})* \"                 { (\alexIn -> makeStrLiteral alexIn)              `andBegin`  separator   }
--<0,nondelim>               \% (@graphic_character | [\%]{2})* \%                 { (\alexIn -> makeStrLiteral alexIn)              `andBegin`  separator   }

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
   in return $ Token keyType

makeIdentifier :: AlexInput -> Int -> Alex Token
makeIdentifier (position, _, _, str) length =
   let identifier = take length str
   in return $ Token Identifier identifier

makeDecimalLiteral :: AlexInput -> Int -> Alex Token
makeDecimalLiteral (position, _, _, str) length =
   let numberStr = take length str
       number = read numberStr
   in return $ Token $ Literal $ Decimal number

makeBasedLiteral :: Char -> AlexInput -> Int -> Alex Token
makeBasedLiteral separator (position, _, _, str) length = do
   let basedStr = take length str
   (base,value,exponent) <- case splitOn [separator] basedStr of
      (base:value:('E':exponent):[]) -> return (base,value,exponent)
      (base:value:"":[]) -> return (base,value,"0")
      _ -> alexError $ GenericBasedLiteralError basedStr position
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
      & Decimal
      & Literal
      & \comp -> Token comp
      & return
   else alexError $ InvalidBaseBasedLiteralError baseInt basedStr position

makeCharLiteral :: AlexInput -> Int -> Alex Token
makeCharLiteral (position, _, _, str) _ =
   let char = head str
   in  return $ Token $ Literal $ Character char

makeStrLiteral :: AlexInput -> Int -> Alex Token
makeStrLiteral (position, _, _, str) length =
   take length str
   & Str
   & Literal
   & \component -> return $ Token component

makeBitStrLiteral :: LiteralBase -> AlexInput -> Int -> Alex Token
makeBitStrLiteral base (position, _, _, str) length =
   take length str
   & init
   & \(_:'"':bitString) ->
      filter (\c -> c /= '_') bitString
      & BitStr base
      & Literal
      & \component -> return $ Token component

makeOperator :: OperatorType -> AlexInput -> Int -> Alex Token
makeOperator op (position, _, _, _) _ =
   Operator op
   & \wrappedOp -> return $ Token wrappedOp

lexer :: (Token -> Alex a) -> Alex a
lexer cont = do
   token <- alexMonadScan
   cont token

main :: IO ()
main = do
   s <- getContents
   print $ runAlex s alexMonadScan

-- | Lexer scan
alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> return $ Token EOF
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
token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)
}