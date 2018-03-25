module Parser.Happy.Functions 
   ( parseError
   )
where

import Parser.Alex.Monad (Alex)
import Parser.Alex.Functions (alexError)
import Parser.TokenTypes (WrappedToken)
import Parser.PositionWrapper
import Parser.ErrorTypes

-- |Generate a parser error using current position
parseError :: WrappedToken -> Alex a
parseError err = alexError $ PosnWrapper {getPos = getPos err, unPos = GenericParseError}
