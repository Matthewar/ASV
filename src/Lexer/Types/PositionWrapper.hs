module Lexer.Types.PositionWrapper where

import Lexer.Alex.Types (AlexPosn)

-- |Basic wrapper type to store position information with data
-- Typically used for nodes and tokens
data PosnWrapper a = PosnWrapper { getPos :: AlexPosn, unPos :: a }
                   deriving (Eq)

instance Show a => Show (PosnWrapper a) where
   show positionWrapper = show $ unPos positionWrapper
