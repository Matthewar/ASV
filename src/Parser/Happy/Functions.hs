module Parser.Happy.Functions where

import Parser.Alex.Monad (Alex)
import Parser.Alex.Functions (alexError)
import Parser.TokenTypes (Token)
import Parser.ErrorTypes

parseError :: Token -> Alex a
parseError _ = alexError GenericParseError
