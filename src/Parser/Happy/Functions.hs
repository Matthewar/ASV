module Parser.Happy.Functions where

import Parser.Alex.Types (Alex,ParserError(..)) -- ?? Move this error location
import Parser.Alex.Functions (alexError)
import Parser.Lexer (Token)

parseError :: [Token] -> Alex a
parseError _ = alexError GenericParseError
