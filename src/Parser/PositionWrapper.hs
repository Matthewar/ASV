module Parser.PositionWrapper where

import Parser.Alex.BaseTypes (AlexPosn)

data PosnWrapper a = PosnWrapper { getPos :: AlexPosn, unPos :: a }
                   deriving (Eq)

instance Show a => Show (PosnWrapper a) where
   show positionWrapper = show $ unPos positionWrapper
