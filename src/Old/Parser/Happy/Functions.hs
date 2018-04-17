module Parser.Happy.Functions 
   ( parseError
   )
where

import Lexer.Types.Monad (Alex)
import Lexer.Types.Token (WrappedToken)
import Lexer.Types.PositionWrapper
import Lexer.Types.Error
import Lexer.Alex.Functions (alexError)

-- |Generate a parser error using current position
parseError :: WrappedToken -> Alex a
parseError err = alexError $ PosnWrapper {getPos = getPos err, unPos = GenericParseError}
