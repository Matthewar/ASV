{-|
   Module      : Parser.Functions.PositionWrapper
   Description : Functions for the parser positions
|-}
module Parser.Functions.PositionWrapper
   ( newFromToken
   , raisePosition
   , passPosition
   ) where

import Parser.Types.PositionWrapper
--import Lexer.Types.Token

---- |Create new wrapped type from 'WrappedToken'
--newFromToken :: (a -> b) -> (Token -> a) -> WrappedToken -> PosnWrapper b
--newFromToken convertToOutput extractInput token =
--   let position = getPos token
--       value = convertToOutput $ extractInput $ unPos token
--   in PosnWrapper { getPos = position, unPos = value }

raisePosition :: (a -> b) -> PosnWrapper a -> PosnWrapper b
raisePosition raiseFunc low = PosnWrapper { getPos = getPos low, unPos = raiseFunc $ unPos low }

passPosition :: a -> PosnWrapper b -> PosnWrapper a
passPosition info positionWrapper = PosnWrapper { getPos = getPos positionWrapper, unPos = info }
