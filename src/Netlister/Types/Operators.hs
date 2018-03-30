{-|
   Module      : Netlister.Types.Operators
   Description : Types for acceptable netlist operators.

   Operators used in netlists.
-}
module Netlister.Types.Operators
         ( Operator(..)
         , convertOperator
         ) where

import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Data.Function ((&))

import qualified Parser.Happy.Types as ParserTypes (OperatorSymbol)

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
convertOperator :: ParserTypes.OperatorSymbol -> Maybe Operator
convertOperator op = MapS.lookup (map toUpper op) operatorMap

-- |Map of operators and associated string names
operatorMap :: MapS.Map String Operator
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
   & MapS.fromList
