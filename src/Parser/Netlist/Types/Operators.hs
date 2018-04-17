{-|
   Module      : Parser.Netlist.Types.Operators
   Description : Types for acceptable netlist operators.

   Operators used in netlists.
-}
module Parser.Netlist.Types.Operators
         ( Operator(..)
         , convertOperator
         ) where

import qualified Data.Bimap as Bimap
import Data.Char (toUpper)
import Data.Function ((&))

-- |Netlist operators
data Operator =
   -- |AND
   And
   -- |OR
   | Or
   -- |NAND
   | Nand
   -- |NOR
   | Nor
   -- |XOR
   | Xor
   -- |=
   | Equal
   -- |/=
   | Inequality
   -- |<
   | LessThan
   -- |<=
   | Assign
   -- |>
   | GreaterThan
   -- |>=
   | GreaterThanOrEqual
   -- |+
   | Plus
   -- |-
   | Hyphen
   -- |&
   | Ampersand
   -- |*
   | Star
   -- |/
   | Slash
   -- |MOD
   | Mod
   -- |REM
   | Rem
   -- |**
   | DoubleStar
   -- |ABS
   | Abs
   -- |NOT
   | Not
   deriving (Eq,Ord)

-- |Convert operator string to netlist operator
-- Read operator string literal
-- - If doesn't exist: return 'Nothing'
-- - If exists: return 'Just' 'Operator'
convertOperator :: String -> Maybe Operator
convertOperator op = Bimap.lookup (map toUpper op) operatorMap

-- |Map of operators and associated string names
operatorMap :: Bimap.Bimap String Operator
operatorMap =
   [ ("AND",And)
   , ("OR",Or)
   , ("NAND",Nand)
   , ("NOR",Nor)
   , ("XOR",Xor)
   , ("=",Equal)
   , ("/=",Inequality)
   , ("<",LessThan)
   , ("<=",Assign)
   , (">",GreaterThan)
   , (">=",GreaterThanOrEqual)
   , ("+",Plus)
   , ("-",Hyphen)
   , ("&",Ampersand)
   , ("*",Star)
   , ("/",Slash)
   , ("MOD",Mod)
   , ("REM",Rem)
   , ("**",DoubleStar)
   , ("ABS",Abs)
   , ("NOT",Not)
   ]
   & Bimap.fromList

instance (Show Operator) where
   show op = operatorMap Bimap.!> op
