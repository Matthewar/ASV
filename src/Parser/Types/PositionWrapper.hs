{-|
   Module      : Parser.Types.PositionWrapper
   Description : Types for the parser positions
|-}
module Parser.Types.PositionWrapper where

import Text.Parsec.Pos (SourcePos)

-- |Basic wrapper type to store position information with data
-- Typically used for nodes and tokens
data PosnWrapper a = PosnWrapper { getPos :: SourcePos, unPos :: a }
                   deriving (Eq)

instance Show a => Show (PosnWrapper a) where
   show positionWrapper = show $ unPos positionWrapper
