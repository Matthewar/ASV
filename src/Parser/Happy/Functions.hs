module Parser.Happy.Functions 
   ( parseError
   )
where

import Parser.Alex.Monad (Alex)
import Parser.Alex.Functions (alexError)
import Parser.TokenTypes (WrappedToken)
import Parser.PositionWrapper
import Parser.ErrorTypes

parseError :: WrappedToken -> Alex a
parseError err = alexError $ PosnWrapper {getPos = getPos err, unPos = GenericParseError}
