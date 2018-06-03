module Sim.Builtin.STD.STANDARD
   ( standardPackage
   ) where

import Data.Text

standardPackage :: Text
standardPackage = pack
   "module STD.STANDARD\n\
   \   ( Type'ANON'BIT(..)\n\
   \   , function'op'EQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'NEQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , Type'ANON'BIT_VECTOR(..)\n\
   \   , mkType'ANON'BIT_VECTOR\n\
   \   , Type'ANON'BOOLEAN(..)\n\
   \   , function'op'EQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'NEQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , Type'ANON'CHARACTER(..)\n\
   \   , function'op'EQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'NEQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , Type'ANON'INTEGER(..)\n\
   \   , mkType'ANON'INTEGER\n\
   \   , extractType'ANON'INTEGER\n\
   \   , function'op'EQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'NEQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'PLUS'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'MINUS'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'MULT'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'DIV'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'MOD'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'REM'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'ABS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'PLUS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'MINUS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , function'op'EXP'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER\n\
   \   , Type'ANON'REAL(..)\n\
   \   , mkType'ANON'REAL\n\
   \   , extractType'ANON'REAL\n\
   \   , function'op'EQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'NEQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'PLUS'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL\n\
   \   , function'op'MINUS'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL\n\
   \   , function'op'MULT'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL\n\
   \   , function'op'DIV'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL\n\
   \   , function'op'ABS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL\n\
   \   , function'op'PLUS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL\n\
   \   , function'op'MINUS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL\n\
   \   , function'op'EXP'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'REAL\n\
   \   , Type'ANON'SEVERITY_LEVEL(..)\n\
   \   , function'op'EQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'NEQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , Type'ANON'STRING(..)\n\
   \   , mkType'ANON'STRING\n\
   \   , Type'ANON'TIME(..)\n\
   \   , mkType'ANON'TIME\n\
   \   , extractType'ANON'TIME\n\
   \   , function'op'EQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'NEQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN\n\
   \   , function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'MINUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'MULT'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'MULT'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'MULT'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'MULT'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'UniversalInteger\n\
   \   , function'op'ABS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'MINUS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , function'op'EXP'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME\n\
   \   , Type'BIT\n\
   \   , mkType'BIT\n\
   \   , Type'BIT_VECTOR\n\
   \   , Type'BOOLEAN\n\
   \   , mkType'BOOLEAN\n\
   \   , Type'CHARACTER\n\
   \   , mkType'CHARACTER\n\
   \   , Type'INTEGER\n\
   \   , mkType'INTEGER\n\
   \   , Type'NATURAL\n\
   \   , mkType'NATURAL\n\
   \   , Type'POSITIVE\n\
   \   , mkType'POSITIVE\n\
   \   , Type'REAL\n\
   \   , mkType'REAL\n\
   \   , Type'SEVERITY_LEVEL\n\
   \   , mkType'SEVERITY_LEVEL\n\
   \   , Type'STRING\n\
   \   , Type'TIME\n\
   \   , mkType'TIME\n\
   \   -- Non standard functions\n\
   \   , boolToBoolean\n\
   \   , SignalOutput(..)\n\
   \   -- NOTE: now function is located in the Control module\n\
   \   ) where\n\
   \\n\
   \-- Non standard type data\n\
   \--   , VHDL_Eq\n\
   \--   , VHDL_Ord\n\
   \--   , VHDL_Add\n\
   \--   , VHDL_Sub\n\
   \--   , VHDL_Mult\n\
   \--   , VHDL_Div\n\
   \--   , VHDL_And\n\
   \--   , VHDL_Or\n\
   \--   , VHDL_Not\n\
   \--   , VHDL_Xor\n\
   \--   , VHDL_Nand\n\
   \--   , VHDL_Nor\n\
   \--   , VHDL_Scalar\n\
   \--   , VHDL_Discrete\n\
   \--   , meta'lessThanArray\n\
   \--   , meta'greaterThanArray\n\
   \\n\
   \import qualified Data.Map.Strict\n\
   \import Data.Int (Int64)\n\
   \import Numeric (showFloat)\n\
   \\n\
   \data Type'ANON'BIT =\n\
   \   Type'ANON'BIT'Char'0\n\
   \   | Type'ANON'BIT'Char'1\n\
   \   deriving (Eq,Ord)\n\
   \instance SignalOutput Type'ANON'BIT where\n\
   \   sigOut Type'ANON'BIT'Char'0 = \"'0'\"\n\
   \   sigOut Type'ANON'BIT'Char'1 = \"'1'\"\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BIT -> Type'ANON'BIT -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 == value2\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BIT -> Type'ANON'BIT -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 /= value2\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BIT -> Type'ANON'BIT -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 < value2\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BIT -> Type'ANON'BIT -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 <= value2\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BIT -> Type'ANON'BIT -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 > value2\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BIT -> Type'ANON'BIT -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'BIT'_'STD'STANDARD'Type'ANON'BIT'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 >= value2\n\
   \--instance VHDL_Eq BIT where\n\
   \--   eq_V BIT'CHAR'0 BIT'CHAR'0 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V BIT'CHAR'1 BIT'CHAR'1 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--   neq_V BIT'CHAR'0 BIT'CHAR'1 = BOOLEAN'IDEN'TRUE\n\
   \--   neq_V BIT'CHAR'1 BIT'CHAR'0 = BOOLEAN'IDEN'TRUE\n\
   \--   neq_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--instance VHDL_Ord BIT where\n\
   \--   lessThan_V BIT'CHAR'0 BIT'CHAR'1 = BOOLEAN'IDEN'TRUE\n\
   \--   lessThan_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--   lessThanOrEqual_V BIT'CHAR'1 BIT'CHAR'0 = BOOLEAN'IDEN'FALSE\n\
   \--   lessThanOrEqual_V _ _ = BOOLEAN'IDEN'TRUE\n\
   \--   greaterThan_V BIT'CHAR'1 BIT'CHAR'0 = BOOLEAN'IDEN'TRUE\n\
   \--   greaterThan_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--   greaterThanOrEqual_V BIT'CHAR'0 BIT'CHAR'1 = BOOLEAN'IDEN'FALSE\n\
   \--   greaterThanOrEqual_V _ _ = BOOLEAN'IDEN'TRUE\n\
   \--instance VHDL_Scalar BIT where\n\
   \--   scalarLeft = BOOLEAN'IDEN'FALSE\n\
   \--   scalarRight = BOOLEAN'IDEN'TRUE\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Discrete BIT where\n\
   \--   discretePos BIT'CHAR'0 = 0\n\
   \--   discretePos BIT'CHAR'1 = 1\n\
   \--   discreteVal 0 = BIT'CHAR'0\n\
   \--   discreteVal 1 = BIT'CHAR'1\n\
   \--   discreteSucc BIT'CHAR'0 = BIT'CHAR'1\n\
   \--   discretePred BIT'CHAR'1 = BIT'CHAR'0\n\
   \--   discreteLeftOf BIT'CHAR'1 = BIT'CHAR'0\n\
   \--   discreteRightOf BIT'CHAR'0 = BIT'CHAR'1\n\
   \--instance VHDL_And BIT where\n\
   \--   and_V BIT'CHAR'1 BIT'CHAR'1 = BIT'CHAR'1\n\
   \--   and_V _ _ = BIT'CHAR'0\n\
   \--instance VHDL_Or BIT where\n\
   \--   or_V BIT'CHAR'1 _ = BIT'CHAR'1\n\
   \--   or_V _ BIT'CHAR'1 = BIT'CHAR'1\n\
   \--   or_V _ _ = BIT'CHAR'0\n\
   \--instance VHDL_Xor BIT where\n\
   \--   xor_V BIT'CHAR'1 BIT'CHAR'0 = BIT'CHAR'1\n\
   \--   xor_V BIT'CHAR'0 BIT'CHAR'1 = BIT'CHAR'1\n\
   \--   xor_V _ _ = BIT'CHAR'0\n\
   \--instance VHDL_Not BIT where\n\
   \--   not_V BIT'CHAR'1 = BIT'CHAR'0\n\
   \--   not_V BIT'CHAR'0 = BIT'CHAR'1\n\
   \--instance VHDL_Nand BIT where\n\
   \--   nand_V v1 v2 = not_V $ and_V v1 v2\n\
   \--instance VHDL_Nor BIT where\n\
   \--   nor_V v1 v2 = not_V $ or_V v1 v2\n\
   \\n\
   \newtype Type'ANON'BIT_VECTOR =\n\
   \   Type'ANON'BIT_VECTOR (Data.Map.Strict.Map (STD.STANDARD.Type'NATURAL) STD.STANDARD.Type'BIT)\n\
   \mkType'ANON'BIT_VECTOR :: [((Integer),STD.STANDARD.Type'BIT)] -> Type'ANON'BIT_VECTOR\n\
   \mkType'ANON'BIT_VECTOR =\n\
   \   let modKeyFunc (value1) =\n\
   \         ( STD.STANDARD.mkType'NATURAL $ STD.STANDARD.mkType'ANON'INTEGER $ value1\n\
   \         )\n\
   \       mapFunc (key,value) = (modKeyFunc key,value)\n\
   \   in Type'ANON'BIT_VECTOR . Data.Map.Strict.fromList . map mapFunc\n\
   \--class ARRAY'BIT_VECTOR a where\n\
   \--   arrayLeft'1'BIT_VECTOR :: NATURAL\n\
   \--   arrayRight'1'BIT_VECTOR :: NATURAL\n\
   \--   arrayHigh'1'BIT_VECTOR :: NATURAL\n\
   \--   arrayLow'1'BIT_VECTOR :: NATURAL\n\
   \--   arrayRange'1'BIT_VECTOR :: (NATURAL,Direction,NATURAL)\n\
   \--   arrayReverseRange'1'BIT_VECTOR :: (NATURAL,Direction,NATURAL)\n\
   \--   arrayLength'1'BIT_VECTOR :: Int64\n\
   \--   concat_V'BIT_VECTOR :: BIT_VECTOR -> BIT_VECTOR -> BIT_VECTOR\n\
   \--   concat_V_E'BIT_VECTOR :: BIT_VECTOR -> BIT -> BIT_VECTOR\n\
   \--   concat_E_V'BIT_VECTOR :: BIT -> BIT_VECTOR -> BIT_VECTOR\n\
   \--   concat_E'BIT_VECTOR :: BIT -> BIT -> BIT_VECTOR\n\
   \--instance VHDL_Eq BIT_VECTOR where\n\
   \--   eq_V (BIT_VECTOR map1) (BIT_VECTOR map2) =\n\
   \--      all (\\(a,b) -> eq_V a b) $ zip (META'IntMap.elems map1) (META'IntMap.elems map2)\n\
   \--   neq_V (BIT_VECTOR map1) (BIT_VECTOR map2) =\n\
   \--      any (\\(a,b) -> neq_V a b) $ zip (META'IntMap.elems map1) (META'IntMap.elems map2)\n\
   \--instance VHDL_Ord BIT_VECTOR where\n\
   \--   lessThan_V (BIT_VECTOR map1) (BIT_VECTOR map2) =\n\
   \--      meta'lessThanArray (META'IntMap.elems map1) (META'IntMap.elems map2)\n\
   \--   lessThanOrEqual_V val1 val2 =\n\
   \--      (eq_V val1 val2) `or_V` (lessThan_V val1 val2)\n\
   \--   greaterThan_V (BIT_VECTOR map1) (BIT_VECTOR map2) =\n\
   \--      meta'greaterThanArray (META'IntMap.elems map1) (META'IntMap.elems map2)\n\
   \--   greaterThanOrEqual_V val1 val2 =\n\
   \--      (eq_V val1 val2) `or_V` (greaterThan_V val1 val2)\n\
   \--instance VHDL_And BIT_VECTOR where\n\
   \--   and_V val1 val2 = map (\\(a,b) -> and_V a b) $ zip val1 val2\n\
   \--instance VHDL_Or BIT_VECTOR where\n\
   \--   or_V val1 val2 = map (\\(a,b) -> or_V a b) $ zip val1 val2\n\
   \--instance VHDL_Not BIT_VECTOR where\n\
   \--   not_V val = map not_V val\n\
   \--instance VHDL_Xor BIT_VECTOR where\n\
   \--   xor_V val1 val2 = map (\\(a,b) -> xor_V a b) $ zip val1 val2\n\
   \--instance VHDL_Nand BIT_VECTOR where\n\
   \--   nand_V val1 val2 = map (\\(a,b) -> nand_V a b) $ zip val1 val2\n\
   \--instance VHDL_Nor BIT_VECTOR where\n\
   \--   nor_V val1 val2 = map (\\(a,b) -> nor_V a b) $ zip val1 val2\n\
   \\n\
   \data Type'ANON'BOOLEAN =\n\
   \   Type'ANON'BOOLEAN'Iden'FALSE\n\
   \   | Type'ANON'BOOLEAN'Iden'TRUE\n\
   \   deriving (Eq,Ord)\n\
   \instance SignalOutput Type'ANON'BOOLEAN where\n\
   \   sigOut Type'ANON'BOOLEAN'Iden'FALSE = \"FALSE\"\n\
   \   sigOut Type'ANON'BOOLEAN'Iden'TRUE = \"TRUE\"\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BOOLEAN -> Type'ANON'BOOLEAN -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 == value2\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BOOLEAN -> Type'ANON'BOOLEAN -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 /= value2\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BOOLEAN -> Type'ANON'BOOLEAN -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 < value2\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BOOLEAN -> Type'ANON'BOOLEAN -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 <= value2\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BOOLEAN -> Type'ANON'BOOLEAN -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 > value2\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'BOOLEAN -> Type'ANON'BOOLEAN -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'BOOLEAN'_'STD'STANDARD'Type'ANON'BOOLEAN'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 >= value2\n\
   \--instance VHDL_Eq BOOLEAN where\n\
   \--   eq_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--   neq_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE\n\
   \--   neq_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE\n\
   \--   neq_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--instance VHDL_Ord BOOLEAN where\n\
   \--   lessThan_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE\n\
   \--   lessThan_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--   lessThanOrEqual_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'FALSE\n\
   \--   lessThanOrEqual_V _ _ = BOOLEAN'IDEN'TRUE\n\
   \--   greaterThan_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE\n\
   \--   greaterThan_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--   greaterThanOrEqual_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'FALSE\n\
   \--   greaterThanOrEqual_V _ _ = BOOLEAN'IDEN'TRUE\n\
   \--instance VHDL_Scalar BOOLEAN where\n\
   \--   scalarLeft = BOOLEAN'IDEN'FALSE\n\
   \--   scalarRight = BOOLEAN'IDEN'TRUE\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Discrete BOOLEAN where\n\
   \--   discretePos BOOLEAN'IDEN'FALSE = 0\n\
   \--   discretePos BOOLEAN'IDEN'TRUE = 1\n\
   \--   discreteVal 0 = BOOLEAN'IDEN'FALSE\n\
   \--   discreteVal 1 = BOOLEAN'IDEN'TRUE\n\
   \--   discreteSucc BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE\n\
   \--   discretePred BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'FALSE\n\
   \--   discreteLeftOf BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'FALSE\n\
   \--   discreteRightOf BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE\n\
   \--instance VHDL_And BOOLEAN where\n\
   \--   and_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE\n\
   \--   and_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--instance VHDL_Or BOOLEAN where\n\
   \--   or_V BOOLEAN'IDEN'TRUE _ = BOOLEAN'IDEN'TRUE\n\
   \--   or_V _ BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE\n\
   \--   or_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--instance VHDL_Xor BOOLEAN where\n\
   \--   xor_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE\n\
   \--   xor_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE\n\
   \--   xor_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--instance VHDL_Not BOOLEAN where\n\
   \--   not_V BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'FALSE\n\
   \--   not_V BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE\n\
   \--instance VHDL_Nand BOOLEAN where\n\
   \--   nand_V v1 v2 = not_V $ and_V v1 v2\n\
   \--instance VHDL_Nor BOOLEAN where\n\
   \--   nor_V v1 v2 = not_V $ or_V v1 v2\n\
   \\n\
   \data Type'ANON'CHARACTER =\n\
   \   Type'ANON'CHARACTER'Iden'NUL\n\
   \   | Type'ANON'CHARACTER'Iden'SOH\n\
   \   | Type'ANON'CHARACTER'Iden'STX\n\
   \   | Type'ANON'CHARACTER'Iden'ETX\n\
   \   | Type'ANON'CHARACTER'Iden'EOT\n\
   \   | Type'ANON'CHARACTER'Iden'ENQ\n\
   \   | Type'ANON'CHARACTER'Iden'ACK\n\
   \   | Type'ANON'CHARACTER'Iden'BEL\n\
   \   | Type'ANON'CHARACTER'Iden'BS\n\
   \   | Type'ANON'CHARACTER'Iden'HT\n\
   \   | Type'ANON'CHARACTER'Iden'LF\n\
   \   | Type'ANON'CHARACTER'Iden'VT\n\
   \   | Type'ANON'CHARACTER'Iden'FF\n\
   \   | Type'ANON'CHARACTER'Iden'CR\n\
   \   | Type'ANON'CHARACTER'Iden'SO\n\
   \   | Type'ANON'CHARACTER'Iden'SI\n\
   \   | Type'ANON'CHARACTER'Iden'DLE\n\
   \   | Type'ANON'CHARACTER'Iden'DC1\n\
   \   | Type'ANON'CHARACTER'Iden'DC2\n\
   \   | Type'ANON'CHARACTER'Iden'DC3\n\
   \   | Type'ANON'CHARACTER'Iden'DC4\n\
   \   | Type'ANON'CHARACTER'Iden'NAK\n\
   \   | Type'ANON'CHARACTER'Iden'SYN\n\
   \   | Type'ANON'CHARACTER'Iden'ETB\n\
   \   | Type'ANON'CHARACTER'Iden'CAN\n\
   \   | Type'ANON'CHARACTER'Iden'EM\n\
   \   | Type'ANON'CHARACTER'Iden'SUB\n\
   \   | Type'ANON'CHARACTER'Iden'ESC\n\
   \   | Type'ANON'CHARACTER'Iden'FSP\n\
   \   | Type'ANON'CHARACTER'Iden'GSP\n\
   \   | Type'ANON'CHARACTER'Iden'RSP\n\
   \   | Type'ANON'CHARACTER'Iden'USP\n\
   \   | Type'ANON'CHARACTER'Char'SPACE\n\
   \   | Type'ANON'CHARACTER'Char'EXCLAMATION\n\
   \   | Type'ANON'CHARACTER'Char'QUOTE\n\
   \   | Type'ANON'CHARACTER'Char'HASH\n\
   \   | Type'ANON'CHARACTER'Char'DOLLAR\n\
   \   | Type'ANON'CHARACTER'Char'PERCENT\n\
   \   | Type'ANON'CHARACTER'Char'AMPERSAND\n\
   \   | Type'ANON'CHARACTER'Char'TICK\n\
   \   | Type'ANON'CHARACTER'Char'LEFTPAREN\n\
   \   | Type'ANON'CHARACTER'Char'RIGHTPAREN\n\
   \   | Type'ANON'CHARACTER'Char'STAR\n\
   \   | Type'ANON'CHARACTER'Char'PLUS\n\
   \   | Type'ANON'CHARACTER'Char'COMMA\n\
   \   | Type'ANON'CHARACTER'Char'HYPHEN\n\
   \   | Type'ANON'CHARACTER'Char'PERIOD\n\
   \   | Type'ANON'CHARACTER'Char'FORWARDSLASH\n\
   \   | Type'ANON'CHARACTER'Char'0\n\
   \   | Type'ANON'CHARACTER'Char'1\n\
   \   | Type'ANON'CHARACTER'Char'2\n\
   \   | Type'ANON'CHARACTER'Char'3\n\
   \   | Type'ANON'CHARACTER'Char'4\n\
   \   | Type'ANON'CHARACTER'Char'5\n\
   \   | Type'ANON'CHARACTER'Char'6\n\
   \   | Type'ANON'CHARACTER'Char'7\n\
   \   | Type'ANON'CHARACTER'Char'8\n\
   \   | Type'ANON'CHARACTER'Char'9\n\
   \   | Type'ANON'CHARACTER'Char'COLON\n\
   \   | Type'ANON'CHARACTER'Char'SEMICOLON\n\
   \   | Type'ANON'CHARACTER'Char'LESSTHAN\n\
   \   | Type'ANON'CHARACTER'Char'EQUAL\n\
   \   | Type'ANON'CHARACTER'Char'GREATERTHAN\n\
   \   | Type'ANON'CHARACTER'Char'QUESTION\n\
   \   | Type'ANON'CHARACTER'Char'AT\n\
   \   | Type'ANON'CHARACTER'Char'A\n\
   \   | Type'ANON'CHARACTER'Char'B\n\
   \   | Type'ANON'CHARACTER'Char'C\n\
   \   | Type'ANON'CHARACTER'Char'D\n\
   \   | Type'ANON'CHARACTER'Char'E\n\
   \   | Type'ANON'CHARACTER'Char'F\n\
   \   | Type'ANON'CHARACTER'Char'G\n\
   \   | Type'ANON'CHARACTER'Char'H\n\
   \   | Type'ANON'CHARACTER'Char'I\n\
   \   | Type'ANON'CHARACTER'Char'J\n\
   \   | Type'ANON'CHARACTER'Char'K\n\
   \   | Type'ANON'CHARACTER'Char'L\n\
   \   | Type'ANON'CHARACTER'Char'M\n\
   \   | Type'ANON'CHARACTER'Char'N\n\
   \   | Type'ANON'CHARACTER'Char'O\n\
   \   | Type'ANON'CHARACTER'Char'P\n\
   \   | Type'ANON'CHARACTER'Char'Q\n\
   \   | Type'ANON'CHARACTER'Char'R\n\
   \   | Type'ANON'CHARACTER'Char'S\n\
   \   | Type'ANON'CHARACTER'Char'T\n\
   \   | Type'ANON'CHARACTER'Char'U\n\
   \   | Type'ANON'CHARACTER'Char'V\n\
   \   | Type'ANON'CHARACTER'Char'W\n\
   \   | Type'ANON'CHARACTER'Char'X\n\
   \   | Type'ANON'CHARACTER'Char'Y\n\
   \   | Type'ANON'CHARACTER'Char'Z\n\
   \   | Type'ANON'CHARACTER'Char'LEFTSQUAREBRACE\n\
   \   | Type'ANON'CHARACTER'Char'BACKSLASH\n\
   \   | Type'ANON'CHARACTER'Char'RIGHTSQUAREBRACE\n\
   \   | Type'ANON'CHARACTER'Char'CAROT\n\
   \   | Type'ANON'CHARACTER'Char'UNDERSCORE\n\
   \   | Type'ANON'CHARACTER'Char'BACKTICK\n\
   \   | Type'ANON'CHARACTER'Char'a\n\
   \   | Type'ANON'CHARACTER'Char'b\n\
   \   | Type'ANON'CHARACTER'Char'c\n\
   \   | Type'ANON'CHARACTER'Char'd\n\
   \   | Type'ANON'CHARACTER'Char'e\n\
   \   | Type'ANON'CHARACTER'Char'f\n\
   \   | Type'ANON'CHARACTER'Char'g\n\
   \   | Type'ANON'CHARACTER'Char'h\n\
   \   | Type'ANON'CHARACTER'Char'i\n\
   \   | Type'ANON'CHARACTER'Char'j\n\
   \   | Type'ANON'CHARACTER'Char'k\n\
   \   | Type'ANON'CHARACTER'Char'l\n\
   \   | Type'ANON'CHARACTER'Char'm\n\
   \   | Type'ANON'CHARACTER'Char'n\n\
   \   | Type'ANON'CHARACTER'Char'o\n\
   \   | Type'ANON'CHARACTER'Char'p\n\
   \   | Type'ANON'CHARACTER'Char'q\n\
   \   | Type'ANON'CHARACTER'Char'r\n\
   \   | Type'ANON'CHARACTER'Char's\n\
   \   | Type'ANON'CHARACTER'Char't\n\
   \   | Type'ANON'CHARACTER'Char'u\n\
   \   | Type'ANON'CHARACTER'Char'v\n\
   \   | Type'ANON'CHARACTER'Char'w\n\
   \   | Type'ANON'CHARACTER'Char'x\n\
   \   | Type'ANON'CHARACTER'Char'y\n\
   \   | Type'ANON'CHARACTER'Char'z\n\
   \   | Type'ANON'CHARACTER'Char'LEFTBRACE\n\
   \   | Type'ANON'CHARACTER'Char'BAR\n\
   \   | Type'ANON'CHARACTER'Char'RIGHTBRACE\n\
   \   | Type'ANON'CHARACTER'Char'TILDE\n\
   \   | Type'ANON'CHARACTER'Iden'DEL\n\
   \   deriving (Eq,Ord)\n\
   \instance SignalOutput Type'ANON'CHARACTER where\n\
   \   sigOut Type'ANON'CHARACTER'Iden'NUL = \"NUL\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'SOH = \"SOH\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'STX = \"STX\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'ETX = \"ETX\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'EOT = \"EOT\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'ENQ = \"ENQ\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'ACK = \"ACK\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'BEL = \"BEL\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'BS = \"BS\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'HT = \"HT\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'LF = \"LF\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'VT = \"VT\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'FF = \"FF\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'CR = \"CR\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'SO = \"SO\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'SI = \"SI\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'DLE = \"DLE\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'DC1 = \"DC1\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'DC2 = \"DC2\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'DC3 = \"DC3\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'DC4 = \"DC4\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'NAK = \"NAK\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'SYN = \"SYN\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'ETB = \"ETB\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'CAN = \"CAN\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'EM = \"EM\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'SUB = \"SUB\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'ESC = \"ESC\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'FSP = \"FSP\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'GSP = \"GSP\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'RSP = \"RSP\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'USP = \"USP\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'SPACE = \"' '\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'EXCLAMATION = \"'!'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'QUOTE = \"'\\\"'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'HASH = \"'#'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'DOLLAR = \"'$'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'PERCENT = \"'%'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'AMPERSAND = \"'&'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'TICK = \"'''\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'LEFTPAREN = \"'('\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'RIGHTPAREN = \"')'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'STAR = \"'*'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'PLUS = \"'+'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'COMMA = \"','\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'HYPHEN = \"'-'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'PERIOD = \"'.'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'FORWARDSLASH = \"'/'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'0 = \"'0'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'1 = \"'1'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'2 = \"'2'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'3 = \"'3'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'4 = \"'4'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'5 = \"'5'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'6 = \"'6'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'7 = \"'7'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'8 = \"'8'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'9 = \"'9'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'COLON = \"':'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'SEMICOLON = \"';'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'LESSTHAN = \"'<'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'EQUAL = \"'='\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'GREATERTHAN = \"'>'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'QUESTION = \"'?'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'AT = \"'@'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'A = \"'A'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'B = \"'B'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'C = \"'C'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'D = \"'D'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'E = \"'E'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'F = \"'F'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'G = \"'G'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'H = \"'H'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'I = \"'I'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'J = \"'J'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'K = \"'K'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'L = \"'L'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'M = \"'M'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'N = \"'N'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'O = \"'O'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'P = \"'P'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'Q = \"'Q'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'R = \"'R'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'S = \"'S'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'T = \"'T'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'U = \"'U'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'V = \"'V'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'W = \"'W'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'X = \"'X'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'Y = \"'Y'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'Z = \"'Z'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'LEFTSQUAREBRACE = \"'['\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'BACKSLASH = \"'\\'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'RIGHTSQUAREBRACE = \"']'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'CAROT = \"'^'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'UNDERSCORE = \"'_'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'BACKTICK = \"'`'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'a = \"'a'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'b = \"'b'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'c = \"'c'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'd = \"'d'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'e = \"'e'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'f = \"'f'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'g = \"'g'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'h = \"'h'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'i = \"'i'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'j = \"'j'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'k = \"'k'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'l = \"'l'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'm = \"'m'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'n = \"'n'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'o = \"'o'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'p = \"'p'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'q = \"'q'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'r = \"'r'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char's = \"'s'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char't = \"'t'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'u = \"'u'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'v = \"'v'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'w = \"'w'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'x = \"'x'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'y = \"'y'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'z = \"'z'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'LEFTBRACE = \"'{'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'BAR = \"'|'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'RIGHTBRACE = \"'}'\"\n\
   \   sigOut Type'ANON'CHARACTER'Char'TILDE = \"'~'\"\n\
   \   sigOut Type'ANON'CHARACTER'Iden'DEL = \"DEL\"\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'CHARACTER -> Type'ANON'CHARACTER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 == value2\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'CHARACTER -> Type'ANON'CHARACTER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 /= value2\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'CHARACTER -> Type'ANON'CHARACTER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 < value2\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'CHARACTER -> Type'ANON'CHARACTER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 <= value2\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'CHARACTER -> Type'ANON'CHARACTER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 > value2\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'CHARACTER -> Type'ANON'CHARACTER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'CHARACTER'_'STD'STANDARD'Type'ANON'CHARACTER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 >= value2\n\
   \--instance VHDL_Eq CHARACTER where\n\
   \--   eq_V CHARACTER'IDEN'NUL CHARACTER'IDEN'NUL = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'SOH CHARACTER'IDEN'SOH = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'STX CHARACTER'IDEN'STX = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'ETX CHARACTER'IDEN'ETX = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'EOT CHARACTER'IDEN'EOT = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'ENQ CHARACTER'IDEN'ENQ = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'ACK CHARACTER'IDEN'ACK = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'BEL CHARACTER'IDEN'BEL = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'BS CHARACTER'IDEN'BS = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'HT CHARACTER'IDEN'HT = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'LF CHARACTER'IDEN'LF = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'VT CHARACTER'IDEN'VT = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'FF CHARACTER'IDEN'FF = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'CR CHARACTER'IDEN'CR = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'SO CHARACTER'IDEN'SO = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'SI CHARACTER'IDEN'SI = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'DLE CHARACTER'IDEN'DLE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'DC1 CHARACTER'IDEN'DC1 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'DC2 CHARACTER'IDEN'DC2 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'DC3 CHARACTER'IDEN'DC3 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'DC4 CHARACTER'IDEN'DC4 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'NAK CHARACTER'IDEN'NAK = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'SYN CHARACTER'IDEN'SYN = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'ETB CHARACTER'IDEN'ETB = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'CAN CHARACTER'IDEN'CAN = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'EM CHARACTER'IDEN'EM = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'SUB CHARACTER'IDEN'SUB = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'ESC CHARACTER'IDEN'ESC = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'FSP CHARACTER'IDEN'FSP = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'GSP CHARACTER'IDEN'GSP = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'RSP CHARACTER'IDEN'RSP = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'USP CHARACTER'IDEN'USP = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'SPACE CHARACTER'CHAR'SPACE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'EXCLAMATION CHARACTER'CHAR'EXCLAMATION = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'QUOTE CHARACTER'CHAR'QUOTE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'HASH CHARACTER'CHAR'HASH = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'DOLLAR CHARACTER'CHAR'DOLLAR = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'PERCENT CHARACTER'CHAR'PERCENT = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'AMPERSAND CHARACTER'CHAR'AMPERSAND = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'TICK CHARACTER'CHAR'TICK = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'LEFTPAREN CHARACTER'CHAR'LEFTPAREN = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'RIGHTPAREN CHARACTER'CHAR'RIGHTPAREN = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'STAR CHARACTER'CHAR'STAR = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'PLUS CHARACTER'CHAR'PLUS = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'COMMA CHARACTER'CHAR'COMMA = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'HYPHEN CHARACTER'CHAR'HYPHEN = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'PERIOD CHARACTER'CHAR'PERIOD = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'FORWARDSLASH CHARACTER'CHAR'FORWARDSLASH = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'0 CHARACTER'CHAR'0 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'1 CHARACTER'CHAR'1 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'2 CHARACTER'CHAR'2 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'3 CHARACTER'CHAR'3 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'4 CHARACTER'CHAR'4 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'5 CHARACTER'CHAR'5 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'6 CHARACTER'CHAR'6 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'7 CHARACTER'CHAR'7 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'8 CHARACTER'CHAR'8 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'9 CHARACTER'CHAR'9 = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'COLON CHARACTER'CHAR'COLON = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'SEMICOLON CHARACTER'CHAR'SEMICOLON = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'LESSTHAN CHARACTER'CHAR'LESSTHAN = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'EQUAL CHARACTER'CHAR'EQUAL = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'GREATERTHAN CHARACTER'CHAR'GREATERTHAN = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'QUESTION CHARACTER'CHAR'QUESTION = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'AT CHARACTER'CHAR'AT = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'A CHARACTER'CHAR'A = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'B CHARACTER'CHAR'B = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'C CHARACTER'CHAR'C = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'D CHARACTER'CHAR'D = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'E CHARACTER'CHAR'E = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'F CHARACTER'CHAR'F = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'G CHARACTER'CHAR'G = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'H CHARACTER'CHAR'H = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'I CHARACTER'CHAR'I = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'J CHARACTER'CHAR'J = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'K CHARACTER'CHAR'K = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'L CHARACTER'CHAR'L = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'M CHARACTER'CHAR'M = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'N CHARACTER'CHAR'N = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'O CHARACTER'CHAR'O = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'P CHARACTER'CHAR'P = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'Q CHARACTER'CHAR'Q = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'R CHARACTER'CHAR'R = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'S CHARACTER'CHAR'S = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'T CHARACTER'CHAR'T = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'U CHARACTER'CHAR'U = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'V CHARACTER'CHAR'V = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'W CHARACTER'CHAR'W = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'X CHARACTER'CHAR'X = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'Y CHARACTER'CHAR'Y = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'Z CHARACTER'CHAR'Z = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'LEFTSQUAREBRACE CHARACTER'CHAR'LEFTSQUAREBRACE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'BACKSLASH CHARACTER'CHAR'BACKSLASH = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'RIGHTSQUAREBRACE CHARACTER'CHAR'RIGHTSQUAREBRACE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'CAROT CHARACTER'CHAR'CAROT = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'UNDERSCORE CHARACTER'CHAR'UNDERSCORE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'BACKTICK CHARACTER'CHAR'BACKTICK = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'a CHARACTER'CHAR'a = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'b CHARACTER'CHAR'b = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'c CHARACTER'CHAR'c = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'd CHARACTER'CHAR'd = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'e CHARACTER'CHAR'e = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'f CHARACTER'CHAR'f = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'g CHARACTER'CHAR'g = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'h CHARACTER'CHAR'h = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'i CHARACTER'CHAR'i = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'j CHARACTER'CHAR'j = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'k CHARACTER'CHAR'k = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'l CHARACTER'CHAR'l = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'm CHARACTER'CHAR'm = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'n CHARACTER'CHAR'n = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'o CHARACTER'CHAR'o = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'p CHARACTER'CHAR'p = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'q CHARACTER'CHAR'q = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'r CHARACTER'CHAR'r = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR's CHARACTER'CHAR's = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR't CHARACTER'CHAR't = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'u CHARACTER'CHAR'u = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'v CHARACTER'CHAR'v = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'w CHARACTER'CHAR'w = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'x CHARACTER'CHAR'x = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'y CHARACTER'CHAR'y = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'z CHARACTER'CHAR'z = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'LEFTBRACE CHARACTER'CHAR'LEFTBRACE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'BAR CHARACTER'CHAR'BAR = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'RIGHTBRACE CHARACTER'CHAR'RIGHTBRACE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'CHAR'TILDE CHARACTER'CHAR'TILDE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V CHARACTER'IDEN'DEL CHARACTER'IDEN'DEL = BOOLEAN'IDEN'TRUE\n\
   \--   neq_V ch1 ch2 = not_V $ eq_V ch1 ch2\n\
   \--instance VHDL_Scalar CHARACTER where\n\
   \--   scalarLeft = CHARACTER'IDEN'NUL\n\
   \--   scalarRight = CHARACTER'IDEN'DEL\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Discrete CHARACTER where\n\
   \--   discretePos CHARACTER'IDEN'NUL = 0\n\
   \--   discretePos CHARACTER'IDEN'SOH = 1\n\
   \--   discretePos CHARACTER'IDEN'STX = 2\n\
   \--   discretePos CHARACTER'IDEN'ETX = 3\n\
   \--   discretePos CHARACTER'IDEN'EOT = 4\n\
   \--   discretePos CHARACTER'IDEN'ENQ = 5\n\
   \--   discretePos CHARACTER'IDEN'ACK = 6\n\
   \--   discretePos CHARACTER'IDEN'BEL = 7\n\
   \--   discretePos CHARACTER'IDEN'BS = 8\n\
   \--   discretePos CHARACTER'IDEN'HT = 9\n\
   \--   discretePos CHARACTER'IDEN'LF = 10\n\
   \--   discretePos CHARACTER'IDEN'VT = 11\n\
   \--   discretePos CHARACTER'IDEN'FF = 12\n\
   \--   discretePos CHARACTER'IDEN'CR = 13\n\
   \--   discretePos CHARACTER'IDEN'SO = 14\n\
   \--   discretePos CHARACTER'IDEN'SI = 15\n\
   \--   discretePos CHARACTER'IDEN'DLE = 16\n\
   \--   discretePos CHARACTER'IDEN'DC1 = 17\n\
   \--   discretePos CHARACTER'IDEN'DC2 = 18\n\
   \--   discretePos CHARACTER'IDEN'DC3 = 19\n\
   \--   discretePos CHARACTER'IDEN'DC4 = 20\n\
   \--   discretePos CHARACTER'IDEN'NAK = 21\n\
   \--   discretePos CHARACTER'IDEN'SYN = 22\n\
   \--   discretePos CHARACTER'IDEN'ETB = 23\n\
   \--   discretePos CHARACTER'IDEN'CAN = 24\n\
   \--   discretePos CHARACTER'IDEN'EM = 25\n\
   \--   discretePos CHARACTER'IDEN'SUB = 26\n\
   \--   discretePos CHARACTER'IDEN'ESC = 27\n\
   \--   discretePos CHARACTER'IDEN'FSP = 28\n\
   \--   discretePos CHARACTER'IDEN'GSP = 29\n\
   \--   discretePos CHARACTER'IDEN'RSP = 30\n\
   \--   discretePos CHARACTER'IDEN'USP = 31\n\
   \--   discretePos CHARACTER'CHAR'SPACE = 32\n\
   \--   discretePos CHARACTER'CHAR'EXCLAMATION = 33\n\
   \--   discretePos CHARACTER'CHAR'QUOTE = 34\n\
   \--   discretePos CHARACTER'CHAR'HASH = 35\n\
   \--   discretePos CHARACTER'CHAR'DOLLAR = 36\n\
   \--   discretePos CHARACTER'CHAR'PERCENT = 37\n\
   \--   discretePos CHARACTER'CHAR'AMPERSAND = 38\n\
   \--   discretePos CHARACTER'CHAR'TICK = 39\n\
   \--   discretePos CHARACTER'CHAR'LEFTPAREN = 40\n\
   \--   discretePos CHARACTER'CHAR'RIGHTPAREN = 41\n\
   \--   discretePos CHARACTER'CHAR'STAR = 42\n\
   \--   discretePos CHARACTER'CHAR'PLUS = 43\n\
   \--   discretePos CHARACTER'CHAR'COMMA = 44\n\
   \--   discretePos CHARACTER'CHAR'HYPHEN = 45\n\
   \--   discretePos CHARACTER'CHAR'PERIOD = 46\n\
   \--   discretePos CHARACTER'CHAR'FORWARDSLASH = 47\n\
   \--   discretePos CHARACTER'CHAR'0 = 48\n\
   \--   discretePos CHARACTER'CHAR'1 = 49\n\
   \--   discretePos CHARACTER'CHAR'2 = 50\n\
   \--   discretePos CHARACTER'CHAR'3 = 51\n\
   \--   discretePos CHARACTER'CHAR'4 = 52\n\
   \--   discretePos CHARACTER'CHAR'5 = 53\n\
   \--   discretePos CHARACTER'CHAR'6 = 54\n\
   \--   discretePos CHARACTER'CHAR'7 = 55\n\
   \--   discretePos CHARACTER'CHAR'8 = 56\n\
   \--   discretePos CHARACTER'CHAR'9 = 57\n\
   \--   discretePos CHARACTER'CHAR'COLON = 58\n\
   \--   discretePos CHARACTER'CHAR'SEMICOLON = 59\n\
   \--   discretePos CHARACTER'CHAR'LESSTHAN = 60\n\
   \--   discretePos CHARACTER'CHAR'EQUAL = 61\n\
   \--   discretePos CHARACTER'CHAR'GREATERTHAN = 62\n\
   \--   discretePos CHARACTER'CHAR'QUESTION = 63\n\
   \--   discretePos CHARACTER'CHAR'AT = 64\n\
   \--   discretePos CHARACTER'CHAR'A = 65\n\
   \--   discretePos CHARACTER'CHAR'B = 66\n\
   \--   discretePos CHARACTER'CHAR'C = 67\n\
   \--   discretePos CHARACTER'CHAR'D = 68\n\
   \--   discretePos CHARACTER'CHAR'E = 69\n\
   \--   discretePos CHARACTER'CHAR'F = 70\n\
   \--   discretePos CHARACTER'CHAR'G = 71\n\
   \--   discretePos CHARACTER'CHAR'H = 72\n\
   \--   discretePos CHARACTER'CHAR'I = 73\n\
   \--   discretePos CHARACTER'CHAR'J = 74\n\
   \--   discretePos CHARACTER'CHAR'K = 75\n\
   \--   discretePos CHARACTER'CHAR'L = 76\n\
   \--   discretePos CHARACTER'CHAR'M = 77\n\
   \--   discretePos CHARACTER'CHAR'N = 78\n\
   \--   discretePos CHARACTER'CHAR'O = 79\n\
   \--   discretePos CHARACTER'CHAR'P = 80\n\
   \--   discretePos CHARACTER'CHAR'Q = 81\n\
   \--   discretePos CHARACTER'CHAR'R = 82\n\
   \--   discretePos CHARACTER'CHAR'S = 83\n\
   \--   discretePos CHARACTER'CHAR'T = 84\n\
   \--   discretePos CHARACTER'CHAR'U = 85\n\
   \--   discretePos CHARACTER'CHAR'V = 86\n\
   \--   discretePos CHARACTER'CHAR'W = 87\n\
   \--   discretePos CHARACTER'CHAR'X = 88\n\
   \--   discretePos CHARACTER'CHAR'Y = 89\n\
   \--   discretePos CHARACTER'CHAR'Z = 90\n\
   \--   discretePos CHARACTER'CHAR'LEFTSQUAREBRACE = 91\n\
   \--   discretePos CHARACTER'CHAR'BACKSLASH = 92\n\
   \--   discretePos CHARACTER'CHAR'RIGHTSQUAREBRACE = 93\n\
   \--   discretePos CHARACTER'CHAR'CAROT = 94\n\
   \--   discretePos CHARACTER'CHAR'UNDERSCORE = 95\n\
   \--   discretePos CHARACTER'CHAR'BACKTICK = 96\n\
   \--   discretePos CHARACTER'CHAR'a = 97\n\
   \--   discretePos CHARACTER'CHAR'b = 98\n\
   \--   discretePos CHARACTER'CHAR'c = 99\n\
   \--   discretePos CHARACTER'CHAR'd = 100\n\
   \--   discretePos CHARACTER'CHAR'e = 101\n\
   \--   discretePos CHARACTER'CHAR'f = 102\n\
   \--   discretePos CHARACTER'CHAR'g = 103\n\
   \--   discretePos CHARACTER'CHAR'h = 104\n\
   \--   discretePos CHARACTER'CHAR'i = 105\n\
   \--   discretePos CHARACTER'CHAR'j = 106\n\
   \--   discretePos CHARACTER'CHAR'k = 107\n\
   \--   discretePos CHARACTER'CHAR'l = 108\n\
   \--   discretePos CHARACTER'CHAR'm = 109\n\
   \--   discretePos CHARACTER'CHAR'n = 110\n\
   \--   discretePos CHARACTER'CHAR'o = 111\n\
   \--   discretePos CHARACTER'CHAR'p = 112\n\
   \--   discretePos CHARACTER'CHAR'q = 113\n\
   \--   discretePos CHARACTER'CHAR'r = 114\n\
   \--   discretePos CHARACTER'CHAR's = 115\n\
   \--   discretePos CHARACTER'CHAR't = 116\n\
   \--   discretePos CHARACTER'CHAR'u = 117\n\
   \--   discretePos CHARACTER'CHAR'v = 118\n\
   \--   discretePos CHARACTER'CHAR'w = 119\n\
   \--   discretePos CHARACTER'CHAR'x = 120\n\
   \--   discretePos CHARACTER'CHAR'y = 121\n\
   \--   discretePos CHARACTER'CHAR'z = 122\n\
   \--   discretePos CHARACTER'CHAR'LEFTBRACE = 123\n\
   \--   discretePos CHARACTER'CHAR'BAR = 124\n\
   \--   discretePos CHARACTER'CHAR'RIGHTBRACE = 125\n\
   \--   discretePos CHARACTER'CHAR'TILDE = 126\n\
   \--   discretePos CHARACTER'IDEN'DEL = 127\n\
   \--   discreteVal 0 = CHARACTER'IDEN'NUL\n\
   \--   discreteVal 1 = CHARACTER'IDEN'SOH\n\
   \--   discreteVal 2 = CHARACTER'IDEN'STX\n\
   \--   discreteVal 3 = CHARACTER'IDEN'ETX\n\
   \--   discreteVal 4 = CHARACTER'IDEN'EOT\n\
   \--   discreteVal 5 = CHARACTER'IDEN'ENQ\n\
   \--   discreteVal 6 = CHARACTER'IDEN'ACK\n\
   \--   discreteVal 7 = CHARACTER'IDEN'BEL\n\
   \--   discreteVal 8 = CHARACTER'IDEN'BS\n\
   \--   discreteVal 9 = CHARACTER'IDEN'HT\n\
   \--   discreteVal 10 = CHARACTER'IDEN'LF\n\
   \--   discreteVal 11 = CHARACTER'IDEN'VT\n\
   \--   discreteVal 12 = CHARACTER'IDEN'FF\n\
   \--   discreteVal 13 = CHARACTER'IDEN'CR\n\
   \--   discreteVal 14 = CHARACTER'IDEN'SO\n\
   \--   discreteVal 15 = CHARACTER'IDEN'SI\n\
   \--   discreteVal 16 = CHARACTER'IDEN'DLE\n\
   \--   discreteVal 17 = CHARACTER'IDEN'DC1\n\
   \--   discreteVal 18 = CHARACTER'IDEN'DC2\n\
   \--   discreteVal 19 = CHARACTER'IDEN'DC3\n\
   \--   discreteVal 20 = CHARACTER'IDEN'DC4\n\
   \--   discreteVal 21 = CHARACTER'IDEN'NAK\n\
   \--   discreteVal 22 = CHARACTER'IDEN'SYN\n\
   \--   discreteVal 23 = CHARACTER'IDEN'ETB\n\
   \--   discreteVal 24 = CHARACTER'IDEN'CAN\n\
   \--   discreteVal 25 = CHARACTER'IDEN'EM\n\
   \--   discreteVal 26 = CHARACTER'IDEN'SUB\n\
   \--   discreteVal 27 = CHARACTER'IDEN'ESC\n\
   \--   discreteVal 28 = CHARACTER'IDEN'FSP\n\
   \--   discreteVal 29 = CHARACTER'IDEN'GSP\n\
   \--   discreteVal 30 = CHARACTER'IDEN'RSP\n\
   \--   discreteVal 31 = CHARACTER'IDEN'USP\n\
   \--   discreteVal 32 = CHARACTER'CHAR'SPACE\n\
   \--   discreteVal 33 = CHARACTER'CHAR'EXCLAMATION\n\
   \--   discreteVal 34 = CHARACTER'CHAR'QUOTE\n\
   \--   discreteVal 35 = CHARACTER'CHAR'HASH\n\
   \--   discreteVal 36 = CHARACTER'CHAR'DOLLAR\n\
   \--   discreteVal 37 = CHARACTER'CHAR'PERCENT\n\
   \--   discreteVal 38 = CHARACTER'CHAR'AMPERSAND\n\
   \--   discreteVal 39 = CHARACTER'CHAR'TICK\n\
   \--   discreteVal 40 = CHARACTER'CHAR'LEFTPAREN\n\
   \--   discreteVal 41 = CHARACTER'CHAR'RIGHTPAREN\n\
   \--   discreteVal 42 = CHARACTER'CHAR'STAR\n\
   \--   discreteVal 43 = CHARACTER'CHAR'PLUS\n\
   \--   discreteVal 44 = CHARACTER'CHAR'COMMA\n\
   \--   discreteVal 45 = CHARACTER'CHAR'HYPHEN\n\
   \--   discreteVal 46 = CHARACTER'CHAR'PERIOD\n\
   \--   discreteVal 47 = CHARACTER'CHAR'FORWARDSLASH\n\
   \--   discreteVal 48 = CHARACTER'CHAR'0\n\
   \--   discreteVal 49 = CHARACTER'CHAR'1\n\
   \--   discreteVal 50 = CHARACTER'CHAR'2\n\
   \--   discreteVal 51 = CHARACTER'CHAR'3\n\
   \--   discreteVal 52 = CHARACTER'CHAR'4\n\
   \--   discreteVal 53 = CHARACTER'CHAR'5\n\
   \--   discreteVal 54 = CHARACTER'CHAR'6\n\
   \--   discreteVal 55 = CHARACTER'CHAR'7\n\
   \--   discreteVal 56 = CHARACTER'CHAR'8\n\
   \--   discreteVal 57 = CHARACTER'CHAR'9\n\
   \--   discreteVal 58 = CHARACTER'CHAR'COLON\n\
   \--   discreteVal 59 = CHARACTER'CHAR'SEMICOLON\n\
   \--   discreteVal 60 = CHARACTER'CHAR'LESSTHAN\n\
   \--   discreteVal 61 = CHARACTER'CHAR'EQUAL\n\
   \--   discreteVal 62 = CHARACTER'CHAR'GREATERTHAN\n\
   \--   discreteVal 63 = CHARACTER'CHAR'QUESTION\n\
   \--   discreteVal 64 = CHARACTER'CHAR'AT\n\
   \--   discreteVal 65 = CHARACTER'CHAR'A\n\
   \--   discreteVal 66 = CHARACTER'CHAR'B\n\
   \--   discreteVal 67 = CHARACTER'CHAR'C\n\
   \--   discreteVal 68 = CHARACTER'CHAR'D\n\
   \--   discreteVal 69 = CHARACTER'CHAR'E\n\
   \--   discreteVal 70 = CHARACTER'CHAR'F\n\
   \--   discreteVal 71 = CHARACTER'CHAR'G\n\
   \--   discreteVal 72 = CHARACTER'CHAR'H\n\
   \--   discreteVal 73 = CHARACTER'CHAR'I\n\
   \--   discreteVal 74 = CHARACTER'CHAR'J\n\
   \--   discreteVal 75 = CHARACTER'CHAR'K\n\
   \--   discreteVal 76 = CHARACTER'CHAR'L\n\
   \--   discreteVal 77 = CHARACTER'CHAR'M\n\
   \--   discreteVal 78 = CHARACTER'CHAR'N\n\
   \--   discreteVal 79 = CHARACTER'CHAR'O\n\
   \--   discreteVal 80 = CHARACTER'CHAR'P\n\
   \--   discreteVal 81 = CHARACTER'CHAR'Q\n\
   \--   discreteVal 82 = CHARACTER'CHAR'R\n\
   \--   discreteVal 83 = CHARACTER'CHAR'S\n\
   \--   discreteVal 84 = CHARACTER'CHAR'T\n\
   \--   discreteVal 85 = CHARACTER'CHAR'U\n\
   \--   discreteVal 86 = CHARACTER'CHAR'V\n\
   \--   discreteVal 87 = CHARACTER'CHAR'W\n\
   \--   discreteVal 88 = CHARACTER'CHAR'X\n\
   \--   discreteVal 89 = CHARACTER'CHAR'Y\n\
   \--   discreteVal 90 = CHARACTER'CHAR'Z\n\
   \--   discreteVal 91 = CHARACTER'CHAR'LEFTSQUAREBRACE\n\
   \--   discreteVal 92 = CHARACTER'CHAR'BACKSLASH\n\
   \--   discreteVal 93 = CHARACTER'CHAR'RIGHTSQUAREBRACE\n\
   \--   discreteVal 94 = CHARACTER'CHAR'CAROT\n\
   \--   discreteVal 95 = CHARACTER'CHAR'UNDERSCORE\n\
   \--   discreteVal 96 = CHARACTER'CHAR'BACKTICK\n\
   \--   discreteVal 97 = CHARACTER'CHAR'a\n\
   \--   discreteVal 98 = CHARACTER'CHAR'b\n\
   \--   discreteVal 99 = CHARACTER'CHAR'c\n\
   \--   discreteVal 100 = CHARACTER'CHAR'd\n\
   \--   discreteVal 101 = CHARACTER'CHAR'e\n\
   \--   discreteVal 102 = CHARACTER'CHAR'f\n\
   \--   discreteVal 103 = CHARACTER'CHAR'g\n\
   \--   discreteVal 104 = CHARACTER'CHAR'h\n\
   \--   discreteVal 105 = CHARACTER'CHAR'i\n\
   \--   discreteVal 106 = CHARACTER'CHAR'j\n\
   \--   discreteVal 107 = CHARACTER'CHAR'k\n\
   \--   discreteVal 108 = CHARACTER'CHAR'l\n\
   \--   discreteVal 109 = CHARACTER'CHAR'm\n\
   \--   discreteVal 110 = CHARACTER'CHAR'n\n\
   \--   discreteVal 111 = CHARACTER'CHAR'o\n\
   \--   discreteVal 112 = CHARACTER'CHAR'p\n\
   \--   discreteVal 113 = CHARACTER'CHAR'q\n\
   \--   discreteVal 114 = CHARACTER'CHAR'r\n\
   \--   discreteVal 115 = CHARACTER'CHAR's\n\
   \--   discreteVal 116 = CHARACTER'CHAR't\n\
   \--   discreteVal 117 = CHARACTER'CHAR'u\n\
   \--   discreteVal 118 = CHARACTER'CHAR'v\n\
   \--   discreteVal 119 = CHARACTER'CHAR'w\n\
   \--   discreteVal 120 = CHARACTER'CHAR'x\n\
   \--   discreteVal 121 = CHARACTER'CHAR'y\n\
   \--   discreteVal 122 = CHARACTER'CHAR'z\n\
   \--   discreteVal 123 = CHARACTER'CHAR'LEFTBRACE\n\
   \--   discreteVal 124 = CHARACTER'CHAR'BAR\n\
   \--   discreteVal 125 = CHARACTER'CHAR'RIGHTBRACE\n\
   \--   discreteVal 126 = CHARACTER'CHAR'TILDE\n\
   \--   discreteVal 127 = CHARACTER'IDEN'DEL\n\
   \--   discreteVal val = error \"Out of bounds CHARACTER value \" ++ show val\n\
   \--   discreteSucc chr = discreteVal $ (discretePos chr) + 1\n\
   \--   discretePred chr = discreteVal $ (discretePos chr) - 1\n\
   \--   discreteLeftOf chr = discreteVal $ (discretePos chr) - 1\n\
   \--   discreteRightOf chr = discreteVal $ (discretePos chr) + 1\n\
   \--instance VHDL_Ord CHARACTER where\n\
   \--   lessThan_V chr1 chr2 =\n\
   \--      meta'boolToBoolean $ (discretePos chr1) < (discretePos chr2)\n\
   \--   lessThanOrEqual_V chr1 chr2 =\n\
   \--      meta'boolToBoolean $ (discretePos chr1) <= (discretePos chr2)\n\
   \--   greaterThan_V chr1 chr2 =\n\
   \--      meta'boolToBoolean $ (discretePos chr1) > (discretePos chr2)\n\
   \--   greaterThanOrEqual_V chr1 chr2 =\n\
   \--      meta'boolToBoolean $ (discretePos chr1) >= (discretePos chr2)\n\
   \\n\
   \newtype Type'ANON'INTEGER = \n\
   \   Type'ANON'INTEGER Int64\n\
   \   deriving (Eq,Ord)\n\
   \mkType'ANON'INTEGER :: Integer -> Type'ANON'INTEGER\n\
   \mkType'ANON'INTEGER value = \n\
   \   if value > 9223372036854775807 || value < -9223372036854775808\n\
   \      then error $ \"Type'ANON'INTEGER value \" ++ show value ++ \" is outside of range -9223372036854775808 < value < 9223372036854775807\"\n\
   \      else Type'ANON'INTEGER $ fromInteger value\n\
   \extractType'ANON'INTEGER :: Type'ANON'INTEGER -> Integer\n\
   \extractType'ANON'INTEGER(Type'ANON'INTEGER value) = toInteger value\n\
   \instance SignalOutput Type'ANON'INTEGER where\n\
   \   sigOut = show . extractType'ANON'INTEGER\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 == value2\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 /= value2\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 < value2\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 <= value2\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 > value2\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 >= value2\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER value1 value2 = mkType'ANON'INTEGER $ (extractType'ANON'INTEGER value1) + (extractType'ANON'INTEGER value2)\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER value1 value2 = mkType'ANON'INTEGER $ (extractType'ANON'INTEGER value1) - (extractType'ANON'INTEGER value2)\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER value1 value2 = mkType'ANON'INTEGER $ (extractType'ANON'INTEGER value1) * (extractType'ANON'INTEGER value2)\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER value1 value2 = mkType'ANON'INTEGER $ (extractType'ANON'INTEGER value1) `div` (extractType'ANON'INTEGER value2)\n\
   \function'op'MOD'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'MOD'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER value1 value2 = mkType'ANON'INTEGER $ (extractType'ANON'INTEGER value1) `mod` (extractType'ANON'INTEGER value2)\n\
   \function'op'REM'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'REM'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER value1 value2 = mkType'ANON'INTEGER $ (extractType'ANON'INTEGER value1) `rem` (extractType'ANON'INTEGER value2)\n\
   \function'op'ABS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'ABS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER = mkType'ANON'INTEGER . abs . extractType'ANON'INTEGER\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER = mkType'ANON'INTEGER . (\\s -> s) . extractType'ANON'INTEGER\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER = mkType'ANON'INTEGER . negate . extractType'ANON'INTEGER\n\
   \function'op'EXP'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER :: Type'ANON'INTEGER -> STD.STANDARD.Type'ANON'INTEGER -> Type'ANON'INTEGER\n\
   \function'op'EXP'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'INTEGER value1 value2 = mkType'ANON'INTEGER $ round $ (fromIntegral $ extractType'ANON'INTEGER value1) ^^ (STD.STANDARD.extractType'ANON'INTEGER value2)\n\
   \--instance VHDL_Scalar INTEGER where\n\
   \--   scalarLeft = INTEGER $ minBound\n\
   \--   scalarRight = INTEGER $ maxBound\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Eq INTEGER where\n\
   \--   eq_V (INTEGER val1) (INTEGER val2) =\n\
   \--      meta'boolToBoolean $ val1 == val2\n\
   \--   neq_V (INTEGER val1) (INTEGER val2) =\n\
   \--      meta'boolToBoolean $ val1 /= val2\n\
   \--instance VHDL_Ord INTEGER where\n\
   \--   lessThan_V (INTEGER val1) (INTEGER val2) =\n\
   \--      meta'boolToBoolean $ val1 < val2\n\
   \--   lessThanOrEqual_V (INTEGER val1) (INTEGER val2) =\n\
   \--      meta'boolToBoolean $ val1 <= val2\n\
   \--   greaterThan_V (INTEGER val1) (INTEGER val2) =\n\
   \--      meta'boolToBoolean $ val1 > val2\n\
   \--   greaterThanOrEqual_V (INTEGER val1) (INTEGER val2) =\n\
   \--      meta'boolToBoolean $ val1 >= val2\n\
   \--mkINTEGER :: Integer -> INTEGER\n\
   \--mkINTEGER val =\n\
   \--   let maxVal = case scalarHigh of (INTEGER v1) -> toInteger v1\n\
   \--       minVal = case scalarLow of (INTEGER v1) -> toInteger v1\n\
   \--   in if val > maxVal || val < minVal\n\
   \--         then error \"Out of bounds INTEGER \" ++ show val\n\
   \--         else INTEGER $ fromInteger\n\
   \--instance VHDL_Add INTEGER where\n\
   \--   add_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) + (toInteger val2)\n\
   \--instance VHDL_Sub INTEGER where\n\
   \--   sub_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) - (toInteger val2)\n\
   \--instance VHDL_Mult INTEGER where\n\
   \--   mult_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) * (toInteger val2)\n\
   \--instance VHDL_Div INTEGER where\n\
   \--   div_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) / (toInteger val2)\n\
   \--instance VHDL_Mod INTEGER where\n\
   \--   mod_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) `mod` (toInteger val2)\n\
   \--instance VHDL_Rem INTEGER where\n\
   \--   rem_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) `rem` (toInteger val2)\n\
   \--instance VHDL_Abs INTEGER where\n\
   \--   abs_V (INTEGER val) = mkINTEGER $ toInteger $ abs val\n\
   \--instance VHDL_Exp INTEGER where\n\
   \--   exp_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ floor $ (fromIntegral val1) ^^ (toInteger val2)\n\
   \--instance VHDL_Discrete BOOLEAN where\n\
   \--   discretePos (INTEGER val) = val\n\
   \--   discreteVal val = mkINTEGER val\n\
   \--   discreteSucc (INTEGER val) = mkINTEGER $ (toInteger val) + 1\n\
   \--   discretePred (INTEGER val) = mkINTEGER $ (toInteger val) - 1\n\
   \--   discreteLeftOf = discretePred\n\
   \--   discreteRightOf = discreteSucc\n\
   \\n\
   \newtype Type'ANON'REAL = \n\
   \   Type'ANON'REAL Double\n\
   \   deriving (Eq,Ord)\n\
   \mkType'ANON'REAL :: Double -> Type'ANON'REAL\n\
   \mkType'ANON'REAL value =\n\
   \   case value of\n\
   \      _ | isInfinite value -> error \"Type'ANON'REAL value is outside of 64 bit IEEE 754 (REAL implementation) range\"\n\
   \      _ -> Type'ANON'REAL value\n\
   \extractType'ANON'REAL :: Type'ANON'REAL -> Double\n\
   \extractType'ANON'REAL(Type'ANON'REAL value) = value\n\
   \instance SignalOutput Type'ANON'REAL where\n\
   \   sigOut flt = (showFloat $ extractType'ANON'REAL flt) \"\"\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'REAL -> Type'ANON'REAL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 == value2\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'REAL -> Type'ANON'REAL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 /= value2\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'REAL -> Type'ANON'REAL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 < value2\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'REAL -> Type'ANON'REAL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 <= value2\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'REAL -> Type'ANON'REAL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 > value2\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'REAL -> Type'ANON'REAL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 >= value2\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL :: Type'ANON'REAL -> Type'ANON'REAL -> Type'ANON'REAL\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL value1 value2 = mkType'ANON'REAL $ (extractType'ANON'REAL value1) + (extractType'ANON'REAL value2)\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL :: Type'ANON'REAL -> Type'ANON'REAL -> Type'ANON'REAL\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL value1 value2 = mkType'ANON'REAL $ (extractType'ANON'REAL value1) - (extractType'ANON'REAL value2)\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL :: Type'ANON'REAL -> Type'ANON'REAL -> Type'ANON'REAL\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL value1 value2 = mkType'ANON'REAL $ (extractType'ANON'REAL value1) * (extractType'ANON'REAL value2)\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL :: Type'ANON'REAL -> Type'ANON'REAL -> Type'ANON'REAL\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL value1 value2 = mkType'ANON'REAL $ (extractType'ANON'REAL value1) / (extractType'ANON'REAL value2)\n\
   \function'op'ABS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL :: Type'ANON'REAL -> Type'ANON'REAL\n\
   \function'op'ABS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL = mkType'ANON'REAL . abs . extractType'ANON'REAL\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL :: Type'ANON'REAL -> Type'ANON'REAL\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL = mkType'ANON'REAL . (\\s -> s) . extractType'ANON'REAL\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL :: Type'ANON'REAL -> Type'ANON'REAL\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'REAL = mkType'ANON'REAL . negate . extractType'ANON'REAL\n\
   \function'op'EXP'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'REAL :: Type'ANON'REAL -> STD.STANDARD.Type'ANON'INTEGER -> Type'ANON'REAL\n\
   \function'op'EXP'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'REAL value1 value2 = mkType'ANON'REAL $ (extractType'ANON'REAL value1) ^^ (STD.STANDARD.extractType'ANON'INTEGER value2)\n\
   \--instance VHDL_Scalar REAL where\n\
   \--   scalarLeft = REAL $ minBound\n\
   \--   scalarRight = REAL $ maxBound\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Eq REAL where\n\
   \--   eq_V (REAL val1) (REAL val2) =\n\
   \--      meta'boolToBoolean $ val1 == val2\n\
   \--   neq_V (REAL val1) (REAL val2) =\n\
   \--      meta'boolToBoolean $ val1 /= val2\n\
   \--instance VHDL_Ord REAL where\n\
   \--   lessThan_V (REAL val1) (REAL val2) =\n\
   \--      meta'boolToBoolean $ val1 < val2\n\
   \--   lessThanOrEqual_V (REAL val1) (REAL val2) =\n\
   \--      meta'boolToBoolean $ val1 <= val2\n\
   \--   greaterThan_V (REAL val1) (REAL val2) =\n\
   \--      meta'boolToBoolean $ val1 > val2\n\
   \--   greaterThanOrEqual_V (REAL val1) (REAL val2) =\n\
   \--      meta'boolToBoolean $ val1 >= val2\n\
   \--instance VHDL_Add REAL where\n\
   \--   add_V (REAL val1) (REAL val2) = REAL $ val1 + val2\n\
   \--instance VHDL_Sub REAL where\n\
   \--   sub_V (REAL val1) (REAL val2) = REAL $ val1 - val2\n\
   \--instance VHDL_Mult REAL where\n\
   \--   mult_V (REAL val1) (REAL val2) = REAL $ val1 * val2\n\
   \--instance VHDL_Div REAL where\n\
   \--   div_V (REAL val1) (REAL val2) = REAL $ val1 / val2\n\
   \--instance VHDL_Abs REAL where\n\
   \--   abs_V (REAL val) = REAL $ abs val\n\
   \--instance VHDL_Exp REAL where\n\
   \--   exp_V (REAL val1) (INTEGER val2) = REAL $ val1 ^^ val2\n\
   \\n\
   \data Type'ANON'SEVERITY_LEVEL =\n\
   \   Type'ANON'SEVERITY_LEVEL'Iden'NOTE\n\
   \   | Type'ANON'SEVERITY_LEVEL'Iden'WARNING\n\
   \   | Type'ANON'SEVERITY_LEVEL'Iden'ERROR\n\
   \   | Type'ANON'SEVERITY_LEVEL'Iden'FAILURE\n\
   \   deriving (Eq,Ord)\n\
   \instance SignalOutput Type'ANON'SEVERITY_LEVEL where\n\
   \   sigOut Type'ANON'SEVERITY_LEVEL'Iden'NOTE = \"NOTE\"\n\
   \   sigOut Type'ANON'SEVERITY_LEVEL'Iden'WARNING = \"WARNING\"\n\
   \   sigOut Type'ANON'SEVERITY_LEVEL'Iden'ERROR = \"ERROR\"\n\
   \   sigOut Type'ANON'SEVERITY_LEVEL'Iden'FAILURE = \"FAILURE\"\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'SEVERITY_LEVEL -> Type'ANON'SEVERITY_LEVEL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 == value2\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'SEVERITY_LEVEL -> Type'ANON'SEVERITY_LEVEL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 /= value2\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'SEVERITY_LEVEL -> Type'ANON'SEVERITY_LEVEL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 < value2\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'SEVERITY_LEVEL -> Type'ANON'SEVERITY_LEVEL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 <= value2\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'SEVERITY_LEVEL -> Type'ANON'SEVERITY_LEVEL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 > value2\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'SEVERITY_LEVEL -> Type'ANON'SEVERITY_LEVEL -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'_'STD'STANDARD'Type'ANON'SEVERITY_LEVEL'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 >= value2\n\
   \--instance VHDL_Eq SEVERITY_LEVEL where\n\
   \--   eq_V SEVERITY_LEVEL'IDEN'NOTE SEVERITY_LEVEL'IDEN'NOTE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V SEVERITY_LEVEL'IDEN'WARNING SEVERITY_LEVEL'IDEN'WARNING = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V SEVERITY_LEVEL'IDEN'ERROR SEVERITY_LEVEL'IDEN'ERROR = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V SEVERITY_LEVEL'IDEN'FAILURE SEVERITY_LEVEL'IDEN'FAILURE = BOOLEAN'IDEN'TRUE\n\
   \--   eq_V _ _ = BOOLEAN'IDEN'FALSE\n\
   \--   neq_V chr1 chr2 = not_V $ eq_V chr1 chr2\n\
   \--instance VHDL_Scalar SEVERITY_LEVEL where\n\
   \--   scalarLeft = SEVERITY_LEVEL'IDEN'NOTE\n\
   \--   scalarRight = SEVERITY_LEVEL'IDEN'FAILURE\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Discrete SEVERITY_LEVEL where\n\
   \--   discretePos SEVERITY_LEVEL'IDEN'NOTE = 0\n\
   \--   discretePos SEVERITY_LEVEL'IDEN'WARNING = 1\n\
   \--   discretePos SEVERITY_LEVEL'IDEN'ERROR = 2\n\
   \--   discretePos SEVERITY_LEVEL'IDEN'FAILURE = 3\n\
   \--   discreteVal 0 = SEVERITY_LEVEL'IDEN'NOTE\n\
   \--   discreteVal 1 = SEVERITY_LEVEL'IDEN'WARNING\n\
   \--   discreteVal 2 = SEVERITY_LEVEL'IDEN'ERROR\n\
   \--   discreteVal 3 = SEVERITY_LEVEL'IDEN'FAILURE\n\
   \--   discreteVal val = error \"Out of bounds SEVERITY_LEVEL value \" ++ show val\n\
   \--   discreteSucc SEVERITY_LEVEL'IDEN'NOTE = SEVERITY_LEVEL'IDEN'WARNING\n\
   \--   discreteSucc SEVERITY_LEVEL'IDEN'WARNING = SEVERITY_LEVEL'IDEN'ERROR\n\
   \--   discreteSucc SEVERITY_LEVEL'IDEN'ERROR = SEVERITY_LEVEL'IDEN'FAILURE\n\
   \--   discretePred SEVERITY_LEVEL'IDEN'WARNING = SEVERITY_LEVEL'IDEN'NOTE\n\
   \--   discretePred SEVERITY_LEVEL'IDEN'ERROR = SEVERITY_LEVEL'IDEN'WARNING\n\
   \--   discretePred SEVERITY_LEVEL'IDEN'FAILURE = SEVERITY_LEVEL'IDEN'ERROR\n\
   \--   discreteLeftOf SEVERITY_LEVEL'IDEN'WARNING = SEVERITY_LEVEL'IDEN'NOTE\n\
   \--   discreteLeftOf SEVERITY_LEVEL'IDEN'ERROR = SEVERITY_LEVEL'IDEN'WARNING\n\
   \--   discreteLeftOf SEVERITY_LEVEL'IDEN'FAILURE = SEVERITY_LEVEL'IDEN'ERROR\n\
   \--   discreteRightOf SEVERITY_LEVEL'IDEN'NOTE = SEVERITY_LEVEL'IDEN'WARNING\n\
   \--   discreteRightOf SEVERITY_LEVEL'IDEN'WARNING = SEVERITY_LEVEL'IDEN'ERROR\n\
   \--   discreteRightOf SEVERITY_LEVEL'IDEN'ERROR = SEVERITY_LEVEL'IDEN'FAILURE\n\
   \--instance VHDL_Ord SEVERITY_LEVEL where\n\
   \--   lessThan_V level1 level2 =\n\
   \--      meta'boolToBoolean $ (discretePos level1) < (discretePos level2)\n\
   \--   lessThanOrEqual_V level1 level2 =\n\
   \--      meta'boolToBoolean $ (discretePos level1) <= (discretePos level2)\n\
   \--   greaterThan_V level1 level2 =\n\
   \--      meta'boolToBoolean $ (discretePos level1) > (discretePos level2)\n\
   \--   greaterThanOrEqual_V level1 level2 =\n\
   \--      meta'boolToBoolean $ (discretePos level1) >= (discretePos level2)\n\
   \\n\
   \newtype Type'ANON'STRING =\n\
   \   Type'ANON'STRING (Data.Map.Strict.Map (STD.STANDARD.Type'POSITIVE) STD.STANDARD.Type'CHARACTER)\n\
   \mkType'ANON'STRING :: [((Integer),STD.STANDARD.Type'CHARACTER)] -> Type'ANON'STRING\n\
   \mkType'ANON'STRING =\n\
   \   let modKeyFunc (value1) =\n\
   \         ( STD.STANDARD.mkType'POSITIVE $ STD.STANDARD.mkType'ANON'INTEGER $ value1\n\
   \         )\n\
   \       mapFunc (key,value) = (modKeyFunc key,value)\n\
   \   in Type'ANON'STRING . Data.Map.Strict.fromList . map mapFunc\n\
   \--class ARRAY'STRING a where\n\
   \--   arrayLeft'1'STRING :: POSITIVE\n\
   \--   arrayRight'1'STRING :: POSITIVE\n\
   \--   arrayHigh'1'STRING :: POSITIVE\n\
   \--   arrayLow'1'STRING :: POSITIVE\n\
   \--   arrayRange'1'STRING :: (POSITIVE,Direction,POSITIVE)\n\
   \--   arrayReverseRange'1'STRING :: (POSITIVE,Direction,POSITIVE)\n\
   \--   arrayLength'1'STRING :: Int64\n\
   \--   concat_V'STRING :: STRING -> STRING -> STRING\n\
   \--   concat_V_E'STRING :: STRING -> CHARACTER -> STRING\n\
   \--   concat_E_V'STRING :: CHARACTER -> STRING -> STRING\n\
   \--   concat_E'STRING :: CHARACTER -> CHARACTER -> STRING\n\
   \--instance VHDL_Eq STRING where\n\
   \--   eq_V (STRING map1) (STRING map2) =\n\
   \--      all (\\(a,b) -> eq_V a b) $ zip (META'IntMap.elems map1) (META'IntMap.elems map2)\n\
   \--   neq_V (STRING map1) (STRING map2) =\n\
   \--      any (\\(a,b) -> neq_V a b) $ zip (META'IntMap.elems map1) (META'IntMap.elems map2)\n\
   \--instance VHDL_Ord STRING where\n\
   \--   lessThan_V (STRING map1) (STRING map2) =\n\
   \--      meta'lessThanArray (META'IntMap.elems map1) (META'IntMap.elems map2)\n\
   \--   lessThanOrEqual_V val1 val2 =\n\
   \--      (eq_V val1 val2) `or_V` (lessThan_V val1 val2)\n\
   \--   greaterThan_V (STRING map1) (STRING map2) =\n\
   \--      meta'greaterThanArray (META'IntMap.elems map1) (META'IntMap.elems map2)\n\
   \--   greaterThanOrEqual_V val1 val2 =\n\
   \--      (eq_V val1 val2) `or_V` (greaterThan_V val1 val2)\n\
   \\n\
   \newtype Type'ANON'TIME = \n\
   \   Type'ANON'TIME Int64\n\
   \   deriving (Eq,Ord)\n\
   \mkType'ANON'TIME :: Integer -> Type'ANON'TIME\n\
   \mkType'ANON'TIME value = \n\
   \   if value > 9223372036854775807 || value < -9223372036854775808\n\
   \      then error $ \"Type'ANON'TIME value \" ++ show value ++ \" is outside of range -9223372036854775808 < value < 9223372036854775807\"\n\
   \      else Type'ANON'TIME $ fromInteger value\n\
   \extractType'ANON'TIME :: Type'ANON'TIME -> Integer\n\
   \extractType'ANON'TIME(Type'ANON'TIME value) = toInteger value\n\
   \instance SignalOutput Type'ANON'TIME where\n\
   \   sigOut phys = (show $ extractType'ANON'TIME phys) ++ \" FS\"\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'TIME -> Type'ANON'TIME -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'EQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 == value2\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'TIME -> Type'ANON'TIME -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'NEQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 /= value2\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'TIME -> Type'ANON'TIME -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHAN'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 < value2\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'TIME -> Type'ANON'TIME -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'LESSTHANOREQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 <= value2\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'TIME -> Type'ANON'TIME -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHAN'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 > value2\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN :: Type'ANON'TIME -> Type'ANON'TIME -> STD.STANDARD.Type'ANON'BOOLEAN\n\
   \function'op'GREATERTHANOREQUAL'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'BOOLEAN value1 value2 = STD.STANDARD.boolToBoolean $ value1 >= value2\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> Type'ANON'TIME -> Type'ANON'TIME\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME value1 value2 = mkType'ANON'TIME $ (extractType'ANON'TIME value1) + (extractType'ANON'TIME value2)\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> Type'ANON'TIME -> Type'ANON'TIME\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME value1 value2 = mkType'ANON'TIME $ (extractType'ANON'TIME value1) - (extractType'ANON'TIME value2)\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> STD.STANDARD.Type'ANON'INTEGER -> Type'ANON'TIME\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME value1 value2 = mkType'ANON'TIME $ (extractType'ANON'TIME value1) * (STD.STANDARD.extractType'ANON'INTEGER value2)\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME :: STD.STANDARD.Type'ANON'INTEGER -> Type'ANON'TIME -> Type'ANON'TIME\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'INTEGER'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME value1 value2 = function'op'MULT'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME value2 value1\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> STD.STANDARD.Type'ANON'REAL -> Type'ANON'TIME\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'TIME value1 value2 = mkType'ANON'TIME $ round $ (fromIntegral $ extractType'ANON'TIME value1) * (STD.STANDARD.extractType'ANON'REAL value2)\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME :: STD.STANDARD.Type'ANON'REAL -> Type'ANON'TIME -> Type'ANON'TIME\n\
   \function'op'MULT'in'STD'STANDARD'Type'ANON'REAL'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME value1 value2 = function'op'MULT'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'TIME value2 value1\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> STD.STANDARD.Type'ANON'INTEGER -> Type'ANON'TIME\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME value1 value2 = mkType'ANON'TIME $ (extractType'ANON'TIME value1) `div` (STD.STANDARD.extractType'ANON'INTEGER value2)\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> STD.STANDARD.Type'ANON'REAL -> Type'ANON'TIME\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'REAL'out'STD'STANDARD'Type'ANON'TIME value1 value2 = mkType'ANON'TIME $ round $ (fromIntegral $ extractType'ANON'TIME value1) / (STD.STANDARD.extractType'ANON'REAL value2)\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'UniversalInteger :: Type'ANON'TIME -> Type'ANON'TIME -> Integer\n\
   \function'op'DIV'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'UniversalInteger value1 value2 = (extractType'ANON'TIME value1) `div` (extractType'ANON'TIME value2)\n\
   \function'op'ABS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> Type'ANON'TIME\n\
   \function'op'ABS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME = mkType'ANON'TIME . abs . extractType'ANON'TIME\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> Type'ANON'TIME\n\
   \function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME = mkType'ANON'TIME . (\\s -> s) . extractType'ANON'TIME\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> Type'ANON'TIME\n\
   \function'op'MINUS'in'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME = mkType'ANON'TIME . negate . extractType'ANON'TIME\n\
   \function'op'EXP'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME :: Type'ANON'TIME -> STD.STANDARD.Type'ANON'INTEGER -> Type'ANON'TIME\n\
   \function'op'EXP'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'INTEGER'out'STD'STANDARD'Type'ANON'TIME value1 value2 = mkType'ANON'TIME $ round $ (fromIntegral $ extractType'ANON'TIME value1) ^^ (STD.STANDARD.extractType'ANON'INTEGER value2)\n\
   \--instance VHDL_Scalar TIME where\n\
   \--   scalarLeft = TIME $ minBound\n\
   \--   scalarRight = TIME $ maxBound\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Eq TIME where\n\
   \--   eq_V (TIME val1) (TIME val2) =\n\
   \--      meta'boolToBoolean $ val1 == val2\n\
   \--   neq_V (TIME val1) (TIME val2) =\n\
   \--      meta'boolToBoolean $ val1 /= val2\n\
   \--instance VHDL_Ord TIME where\n\
   \--   lessThan_V (TIME val1) (TIME val2) =\n\
   \--      meta'boolToBoolean $ val1 < val2\n\
   \--   lessThanOrEqual_V (TIME val1) (TIME val2) =\n\
   \--      meta'boolToBoolean $ val1 <= val2\n\
   \--   greaterThan_V (TIME val1) (TIME val2) =\n\
   \--      meta'boolToBoolean $ val1 > val2\n\
   \--   greaterThanOrEqual_V (TIME val1) (TIME val2) =\n\
   \--      meta'boolToBoolean $ val1 >= val2\n\
   \--mkTIME :: Integer -> TIME\n\
   \--mkTIME val =\n\
   \--   let maxVal = case scalarHigh of (TIME v1) -> toInteger v1\n\
   \--       minVal = case scalarLow of (TIME v1) -> toInteger v1\n\
   \--   in if val > maxVal || val < minVal\n\
   \--         then error \"Out of bounds TIME \" ++ show val\n\
   \--         else TIME $ fromInteger\n\
   \--instance VHDL_Add TIME where\n\
   \--   add_V (TIME val1) (TIME val2) = mkTIME $ (toInteger val1) + (toInteger val2)\n\
   \--instance VHDL_Sub TIME where\n\
   \--   sub_V (TIME val1) (TIME val2) = mkTIME $ (toInteger val1) - (toInteger val2)\n\
   \--instance VHDL_Mult_Phys TIME where\n\
   \--   mult_V_I (TIME val1) (INTEGER val2) = mkTIME $ (toInteger val1) * (toInteger val2)\n\
   \--   mult_V_R (TIME val1) (REAL val2) = mkTIME $ floor $ (fromIntegral val1) *  val2\n\
   \--   mult_I_V int time = mult_V_I time int\n\
   \--   mult_R_V real time = mult_V_R time real\n\
   \--instance VHDL_Div_Phys TIME where\n\
   \--   div_V_I (TIME val1) (INTEGER val2) = mkTIME $ (toInteger val1) / (toInteger val2)\n\
   \--   div_V_R (TIME val1) (REAL val2) = mkTIME $ floor $ (fromIntegral val1) / val2\n\
   \--   div_V_V (TIME val1) (TIME val2) = val1 / val2\n\
   \--instance VHDL_Abs TIME where\n\
   \--   abs_V (TIME val) = mkTIME $ toInteger $ abs val\n\
   \--instance VHDL_Discrete TIME where\n\
   \--   discretePos (TIME val) = val\n\
   \--   discreteVal val = mkTIME val\n\
   \--   discreteSucc val = mkTIME $ (toInteger val) + 1\n\
   \--   discretePred val = mkTIME $ (toInteger val) - 1\n\
   \--   discreteLeftOf = discretePred\n\
   \--   discreteRightOf = discreteSucc\n\
   \\n\
   \type Type'BIT = STD.STANDARD.Type'ANON'BIT\n\
   \mkType'BIT :: Type'BIT -> Type'BIT\n\
   \mkType'BIT STD.STANDARD.Type'ANON'BIT'Char'0 = STD.STANDARD.Type'ANON'BIT'Char'0\n\
   \mkType'BIT STD.STANDARD.Type'ANON'BIT'Char'1 = STD.STANDARD.Type'ANON'BIT'Char'1\n\
   \\n\
   \type Type'BIT_VECTOR = STD.STANDARD.Type'ANON'BIT_VECTOR\n\
   \\n\
   \type Type'BOOLEAN = STD.STANDARD.Type'ANON'BOOLEAN\n\
   \mkType'BOOLEAN :: Type'BOOLEAN -> Type'BOOLEAN\n\
   \mkType'BOOLEAN STD.STANDARD.Type'ANON'BOOLEAN'Iden'FALSE = STD.STANDARD.Type'ANON'BOOLEAN'Iden'FALSE\n\
   \mkType'BOOLEAN STD.STANDARD.Type'ANON'BOOLEAN'Iden'TRUE = STD.STANDARD.Type'ANON'BOOLEAN'Iden'TRUE\n\
   \\n\
   \type Type'CHARACTER = STD.STANDARD.Type'ANON'CHARACTER\n\
   \mkType'CHARACTER :: Type'CHARACTER -> Type'CHARACTER\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'NUL = STD.STANDARD.Type'ANON'CHARACTER'Iden'NUL\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'SOH = STD.STANDARD.Type'ANON'CHARACTER'Iden'SOH\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'STX = STD.STANDARD.Type'ANON'CHARACTER'Iden'STX\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'ETX = STD.STANDARD.Type'ANON'CHARACTER'Iden'ETX\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'EOT = STD.STANDARD.Type'ANON'CHARACTER'Iden'EOT\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'ENQ = STD.STANDARD.Type'ANON'CHARACTER'Iden'ENQ\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'ACK = STD.STANDARD.Type'ANON'CHARACTER'Iden'ACK\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'BEL = STD.STANDARD.Type'ANON'CHARACTER'Iden'BEL\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'BS = STD.STANDARD.Type'ANON'CHARACTER'Iden'BS\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'HT = STD.STANDARD.Type'ANON'CHARACTER'Iden'HT\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'LF = STD.STANDARD.Type'ANON'CHARACTER'Iden'LF\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'VT = STD.STANDARD.Type'ANON'CHARACTER'Iden'VT\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'FF = STD.STANDARD.Type'ANON'CHARACTER'Iden'FF\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'CR = STD.STANDARD.Type'ANON'CHARACTER'Iden'CR\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'SO = STD.STANDARD.Type'ANON'CHARACTER'Iden'SO\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'SI = STD.STANDARD.Type'ANON'CHARACTER'Iden'SI\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'DLE = STD.STANDARD.Type'ANON'CHARACTER'Iden'DLE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'DC1 = STD.STANDARD.Type'ANON'CHARACTER'Iden'DC1\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'DC2 = STD.STANDARD.Type'ANON'CHARACTER'Iden'DC2\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'DC3 = STD.STANDARD.Type'ANON'CHARACTER'Iden'DC3\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'DC4 = STD.STANDARD.Type'ANON'CHARACTER'Iden'DC4\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'NAK = STD.STANDARD.Type'ANON'CHARACTER'Iden'NAK\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'SYN = STD.STANDARD.Type'ANON'CHARACTER'Iden'SYN\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'ETB = STD.STANDARD.Type'ANON'CHARACTER'Iden'ETB\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'CAN = STD.STANDARD.Type'ANON'CHARACTER'Iden'CAN\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'EM = STD.STANDARD.Type'ANON'CHARACTER'Iden'EM\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'SUB = STD.STANDARD.Type'ANON'CHARACTER'Iden'SUB\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'ESC = STD.STANDARD.Type'ANON'CHARACTER'Iden'ESC\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'FSP = STD.STANDARD.Type'ANON'CHARACTER'Iden'FSP\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'GSP = STD.STANDARD.Type'ANON'CHARACTER'Iden'GSP\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'RSP = STD.STANDARD.Type'ANON'CHARACTER'Iden'RSP\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'USP = STD.STANDARD.Type'ANON'CHARACTER'Iden'USP\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'SPACE = STD.STANDARD.Type'ANON'CHARACTER'Char'SPACE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'EXCLAMATION = STD.STANDARD.Type'ANON'CHARACTER'Char'EXCLAMATION\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'QUOTE = STD.STANDARD.Type'ANON'CHARACTER'Char'QUOTE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'HASH = STD.STANDARD.Type'ANON'CHARACTER'Char'HASH\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'DOLLAR = STD.STANDARD.Type'ANON'CHARACTER'Char'DOLLAR\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'PERCENT = STD.STANDARD.Type'ANON'CHARACTER'Char'PERCENT\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'AMPERSAND = STD.STANDARD.Type'ANON'CHARACTER'Char'AMPERSAND\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'TICK = STD.STANDARD.Type'ANON'CHARACTER'Char'TICK\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTPAREN = STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTPAREN\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTPAREN = STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTPAREN\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'STAR = STD.STANDARD.Type'ANON'CHARACTER'Char'STAR\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'PLUS = STD.STANDARD.Type'ANON'CHARACTER'Char'PLUS\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'COMMA = STD.STANDARD.Type'ANON'CHARACTER'Char'COMMA\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'HYPHEN = STD.STANDARD.Type'ANON'CHARACTER'Char'HYPHEN\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'PERIOD = STD.STANDARD.Type'ANON'CHARACTER'Char'PERIOD\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'FORWARDSLASH = STD.STANDARD.Type'ANON'CHARACTER'Char'FORWARDSLASH\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'0 = STD.STANDARD.Type'ANON'CHARACTER'Char'0\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'1 = STD.STANDARD.Type'ANON'CHARACTER'Char'1\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'2 = STD.STANDARD.Type'ANON'CHARACTER'Char'2\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'3 = STD.STANDARD.Type'ANON'CHARACTER'Char'3\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'4 = STD.STANDARD.Type'ANON'CHARACTER'Char'4\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'5 = STD.STANDARD.Type'ANON'CHARACTER'Char'5\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'6 = STD.STANDARD.Type'ANON'CHARACTER'Char'6\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'7 = STD.STANDARD.Type'ANON'CHARACTER'Char'7\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'8 = STD.STANDARD.Type'ANON'CHARACTER'Char'8\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'9 = STD.STANDARD.Type'ANON'CHARACTER'Char'9\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'COLON = STD.STANDARD.Type'ANON'CHARACTER'Char'COLON\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'SEMICOLON = STD.STANDARD.Type'ANON'CHARACTER'Char'SEMICOLON\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'LESSTHAN = STD.STANDARD.Type'ANON'CHARACTER'Char'LESSTHAN\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'EQUAL = STD.STANDARD.Type'ANON'CHARACTER'Char'EQUAL\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'GREATERTHAN = STD.STANDARD.Type'ANON'CHARACTER'Char'GREATERTHAN\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'QUESTION = STD.STANDARD.Type'ANON'CHARACTER'Char'QUESTION\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'AT = STD.STANDARD.Type'ANON'CHARACTER'Char'AT\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'A = STD.STANDARD.Type'ANON'CHARACTER'Char'A\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'B = STD.STANDARD.Type'ANON'CHARACTER'Char'B\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'C = STD.STANDARD.Type'ANON'CHARACTER'Char'C\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'D = STD.STANDARD.Type'ANON'CHARACTER'Char'D\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'E = STD.STANDARD.Type'ANON'CHARACTER'Char'E\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'F = STD.STANDARD.Type'ANON'CHARACTER'Char'F\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'G = STD.STANDARD.Type'ANON'CHARACTER'Char'G\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'H = STD.STANDARD.Type'ANON'CHARACTER'Char'H\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'I = STD.STANDARD.Type'ANON'CHARACTER'Char'I\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'J = STD.STANDARD.Type'ANON'CHARACTER'Char'J\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'K = STD.STANDARD.Type'ANON'CHARACTER'Char'K\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'L = STD.STANDARD.Type'ANON'CHARACTER'Char'L\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'M = STD.STANDARD.Type'ANON'CHARACTER'Char'M\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'N = STD.STANDARD.Type'ANON'CHARACTER'Char'N\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'O = STD.STANDARD.Type'ANON'CHARACTER'Char'O\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'P = STD.STANDARD.Type'ANON'CHARACTER'Char'P\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'Q = STD.STANDARD.Type'ANON'CHARACTER'Char'Q\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'R = STD.STANDARD.Type'ANON'CHARACTER'Char'R\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'S = STD.STANDARD.Type'ANON'CHARACTER'Char'S\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'T = STD.STANDARD.Type'ANON'CHARACTER'Char'T\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'U = STD.STANDARD.Type'ANON'CHARACTER'Char'U\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'V = STD.STANDARD.Type'ANON'CHARACTER'Char'V\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'W = STD.STANDARD.Type'ANON'CHARACTER'Char'W\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'X = STD.STANDARD.Type'ANON'CHARACTER'Char'X\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'Y = STD.STANDARD.Type'ANON'CHARACTER'Char'Y\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'Z = STD.STANDARD.Type'ANON'CHARACTER'Char'Z\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTSQUAREBRACE = STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTSQUAREBRACE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'BACKSLASH = STD.STANDARD.Type'ANON'CHARACTER'Char'BACKSLASH\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTSQUAREBRACE = STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTSQUAREBRACE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'CAROT = STD.STANDARD.Type'ANON'CHARACTER'Char'CAROT\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'UNDERSCORE = STD.STANDARD.Type'ANON'CHARACTER'Char'UNDERSCORE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'a = STD.STANDARD.Type'ANON'CHARACTER'Char'a\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'b = STD.STANDARD.Type'ANON'CHARACTER'Char'b\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'c = STD.STANDARD.Type'ANON'CHARACTER'Char'c\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'd = STD.STANDARD.Type'ANON'CHARACTER'Char'd\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'e = STD.STANDARD.Type'ANON'CHARACTER'Char'e\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'f = STD.STANDARD.Type'ANON'CHARACTER'Char'f\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'g = STD.STANDARD.Type'ANON'CHARACTER'Char'g\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'h = STD.STANDARD.Type'ANON'CHARACTER'Char'h\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'i = STD.STANDARD.Type'ANON'CHARACTER'Char'i\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'j = STD.STANDARD.Type'ANON'CHARACTER'Char'j\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'k = STD.STANDARD.Type'ANON'CHARACTER'Char'k\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'l = STD.STANDARD.Type'ANON'CHARACTER'Char'l\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'm = STD.STANDARD.Type'ANON'CHARACTER'Char'm\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'n = STD.STANDARD.Type'ANON'CHARACTER'Char'n\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'o = STD.STANDARD.Type'ANON'CHARACTER'Char'o\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'p = STD.STANDARD.Type'ANON'CHARACTER'Char'p\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'q = STD.STANDARD.Type'ANON'CHARACTER'Char'q\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'r = STD.STANDARD.Type'ANON'CHARACTER'Char'r\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char's = STD.STANDARD.Type'ANON'CHARACTER'Char's\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char't = STD.STANDARD.Type'ANON'CHARACTER'Char't\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'u = STD.STANDARD.Type'ANON'CHARACTER'Char'u\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'v = STD.STANDARD.Type'ANON'CHARACTER'Char'v\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'w = STD.STANDARD.Type'ANON'CHARACTER'Char'w\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'x = STD.STANDARD.Type'ANON'CHARACTER'Char'x\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'y = STD.STANDARD.Type'ANON'CHARACTER'Char'y\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'z = STD.STANDARD.Type'ANON'CHARACTER'Char'z\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTBRACE = STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTBRACE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'BAR = STD.STANDARD.Type'ANON'CHARACTER'Char'BAR\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTBRACE = STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTBRACE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Char'TILDE = STD.STANDARD.Type'ANON'CHARACTER'Char'TILDE\n\
   \mkType'CHARACTER STD.STANDARD.Type'ANON'CHARACTER'Iden'DEL = STD.STANDARD.Type'ANON'CHARACTER'Iden'DEL\n\
   \\n\
   \type Type'INTEGER = STD.STANDARD.Type'ANON'INTEGER\n\
   \mkType'INTEGER :: Type'INTEGER -> Type'INTEGER\n\
   \mkType'INTEGER wrappedValue = \n\
   \   let value = STD.STANDARD.extractType'ANON'INTEGER wrappedValue\n\
   \   in if value > 9223372036854775807 || value < -9223372036854775808\n\
   \         then error $ \"Type'INTEGER value \" ++ show value ++ \" is outside of range -9223372036854775808 < value < 9223372036854775807\"\n\
   \         else wrappedValue\n\
   \\n\
   \type Type'NATURAL = STD.STANDARD.Type'ANON'INTEGER\n\
   \mkType'NATURAL :: Type'NATURAL -> Type'NATURAL\n\
   \mkType'NATURAL wrappedValue = \n\
   \   let value = STD.STANDARD.extractType'ANON'INTEGER wrappedValue\n\
   \   in if value > 9223372036854775807 || value < 0\n\
   \         then error $ \"Type'NATURAL value \" ++ show value ++ \" is outside of range 0 < value < 9223372036854775807\"\n\
   \         else wrappedValue\n\
   \--instance VHDL_Scalar INTEGER where\n\
   \--   scalarLeft = INTEGER 0\n\
   \--   scalarRight = INTEGER $ maxBound\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Eq NATURAL where\n\
   \--   eq_V (NATURAL val1) (NATURAL val2) =\n\
   \--      meta'boolToBoolean $ val1 == val2\n\
   \--   neq_V (NATURAL val1) (NATURAL val2) =\n\
   \--      meta'boolToBoolean $ val1 /= val2\n\
   \--instance VHDL_Ord NATURAL where\n\
   \--   lessThan_V (NATURAL val1) (NATURAL val2) =\n\
   \--      meta'boolToBoolean $ val1 < val2\n\
   \--   lessThanOrEqual_V (NATURAL val1) (NATURAL val2) =\n\
   \--      meta'boolToBoolean $ val1 <= val2\n\
   \--   greaterThan_V (NATURAL val1) (NATURAL val2) =\n\
   \--      meta'boolToBoolean $ val1 > val2\n\
   \--   greaterThanOrEqual_V (NATURAL val1) (NATURAL val2) =\n\
   \--      meta'boolToBoolean $ val1 >= val2\n\
   \--mkNATURAL :: INTEGER -> NATURAL\n\
   \--mkNATURAL val =\n\
   \--   let maxVal = case scalarHigh of (NATURAL v1) -> v1\n\
   \--       minVal = case scalarLow of (NATURAL v1) -> v1\n\
   \--   in if (greaterThan_V val maxVal) == BOOLEAN'IDEN'TRUE || (lessThan_V val minVal) == BOOLEAN'IDEN'TRUE\n\
   \--         then error \"Out of bounds NATURAL \" ++ show val\n\
   \--         else NATURAL val\n\
   \--instance VHDL_Add NATURAL where\n\
   \--   add_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ add_V val1 val2\n\
   \--instance VHDL_Sub NATURAL where\n\
   \--   sub_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ sub_V val1 val2\n\
   \--instance VHDL_Mult NATURAL where\n\
   \--   mult_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ mult_V val1 val2\n\
   \--instance VHDL_Div NATURAL where\n\
   \--   div_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ div_V val1 val2\n\
   \--instance VHDL_Mod NATURAL where\n\
   \--   mod_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ mod_V val1 val2\n\
   \--instance VHDL_Rem NATURAL where\n\
   \--   rem_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ rem_V val1 val2\n\
   \--instance VHDL_Abs NATURAL where\n\
   \--   abs_V (NATURAL val) = mkNATURAL $ abs_V val\n\
   \--instance VHDL_Exp NATURAL where\n\
   \--   exp_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ exp_V val1 val2\n\
   \--instance VHDL_Discrete BOOLEAN where\n\
   \--   discretePos (NATURAL val) = val\n\
   \--   discreteVal val = mkNATURAL $ mkINTEGER val\n\
   \--   discreteSucc val = mkNATURAL $ add_V val (INTEGER 1)\n\
   \--   discretePred val = mkNATURAL $ sub_V val (INTEGER 1)\n\
   \--   discreteLeftOf = discretePred\n\
   \--   discreteRightOf = discreteSucc\n\
   \\n\
   \type Type'POSITIVE = STD.STANDARD.Type'ANON'INTEGER\n\
   \mkType'POSITIVE :: Type'POSITIVE -> Type'POSITIVE\n\
   \mkType'POSITIVE wrappedValue = \n\
   \   let value = STD.STANDARD.extractType'ANON'INTEGER wrappedValue\n\
   \   in if value > 9223372036854775807 || value < 1\n\
   \         then error $ \"Type'POSITIVE value \" ++ show value ++ \" is outside of range 1 < value < 9223372036854775807\"\n\
   \         else wrappedValue\n\
   \--instance VHDL_Scalar INTEGER where\n\
   \--   scalarLeft = INTEGER 1\n\
   \--   scalarRight = INTEGER $ maxBound\n\
   \--   scalarHigh = scalarRight\n\
   \--   scalarLow = scalarLeft\n\
   \--instance VHDL_Eq POSITIVE where\n\
   \--   eq_V (POSITIVE val1) (POSITIVE val2) =\n\
   \--      meta'boolToBoolean $ val1 == val2\n\
   \--   neq_V (POSITIVE val1) (POSITIVE val2) =\n\
   \--      meta'boolToBoolean $ val1 /= val2\n\
   \--instance VHDL_Ord POSITIVE where\n\
   \--   lessThan_V (POSITIVE val1) (POSITIVE val2) =\n\
   \--      meta'boolToBoolean $ val1 < val2\n\
   \--   lessThanOrEqual_V (POSITIVE val1) (POSITIVE val2) =\n\
   \--      meta'boolToBoolean $ val1 <= val2\n\
   \--   greaterThan_V (POSITIVE val1) (POSITIVE val2) =\n\
   \--      meta'boolToBoolean $ val1 > val2\n\
   \--   greaterThanOrEqual_V (POSITIVE val1) (POSITIVE val2) =\n\
   \--      meta'boolToBoolean $ val1 >= val2\n\
   \--mkPOSITIVE :: INTEGER -> POSITIVE\n\
   \--mkPOSITIVE val =\n\
   \--   let maxVal = case scalarHigh of (POSITIVE v1) -> v1\n\
   \--       minVal = case scalarLow of (POSITIVE v1) -> v1\n\
   \--   in if (greaterThan_V val maxVal) == BOOLEAN'IDEN'TRUE || (lessThan_V val minVal) == BOOLEAN'IDEN'TRUE\n\
   \--         then error \"Out of bounds POSITIVE \" ++ show val\n\
   \--         else POSITIVE val\n\
   \--instance VHDL_Add POSITIVE where\n\
   \--   add_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ add_V val1 val2\n\
   \--instance VHDL_Sub POSITIVE where\n\
   \--   sub_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ sub_V val1 val2\n\
   \--instance VHDL_Mult POSITIVE where\n\
   \--   mult_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ mult_V val1 val2\n\
   \--instance VHDL_Div POSITIVE where\n\
   \--   div_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ div_V val1 val2\n\
   \--instance VHDL_Mod POSITIVE where\n\
   \--   mod_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ mod_V val1 val2\n\
   \--instance VHDL_Rem POSITIVE where\n\
   \--   rem_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ rem_V val1 val2\n\
   \--instance VHDL_Abs POSITIVE where\n\
   \--   abs_V (POSITIVE val) = mkPOSITIVE $ abs_V val\n\
   \--instance VHDL_Exp POSITIVE where\n\
   \--   exp_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ exp_V val1 val2\n\
   \--instance VHDL_Discrete BOOLEAN where\n\
   \--   discretePos (POSITIVE val) = val\n\
   \--   discreteVal val = mkPOSITIVE $ mkINTEGER val\n\
   \--   discreteSucc val = mkPOSITIVE $ add_V val (INTEGER 1)\n\
   \--   discretePred val = mkPOSITIVE $ sub_V val (INTEGER 1)\n\
   \--   discreteLeftOf = discretePred\n\
   \--   discreteRightOf = discreteSucc\n\
   \\n\
   \type Type'REAL = STD.STANDARD.Type'ANON'REAL\n\
   \mkType'REAL :: Type'REAL -> Type'REAL\n\
   \mkType'REAL wrappedValue =\n\
   \   let value = STD.STANDARD.extractType'ANON'REAL wrappedValue\n\
   \   in if value < -179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0 || value > 179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0\n\
   \         then error $ \"Type'REAL value \" ++ show value ++ \" is outside of range -179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0 < value < 179769313486231570000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000.0\"\n\
   \         else wrappedValue\n\
   \\n\
   \type Type'SEVERITY_LEVEL = STD.STANDARD.Type'ANON'SEVERITY_LEVEL\n\
   \mkType'SEVERITY_LEVEL :: Type'SEVERITY_LEVEL -> Type'SEVERITY_LEVEL\n\
   \mkType'SEVERITY_LEVEL STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'NOTE = STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'NOTE\n\
   \mkType'SEVERITY_LEVEL STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'WARNING = STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'WARNING\n\
   \mkType'SEVERITY_LEVEL STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'ERROR = STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'ERROR\n\
   \mkType'SEVERITY_LEVEL STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'FAILURE = STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'FAILURE\n\
   \\n\
   \type Type'STRING = STD.STANDARD.Type'ANON'STRING\n\
   \\n\
   \type Type'TIME = STD.STANDARD.Type'ANON'TIME\n\
   \mkType'TIME :: Type'TIME -> Type'TIME\n\
   \mkType'TIME wrappedValue =\n\
   \   let value = STD.STANDARD.extractType'ANON'TIME wrappedValue\n\
   \   in if value > 9223372036854775807 || value < -9223372036854775808\n\
   \         then error $ \"Type'TIME value \" ++ show value ++ \" is outside of range -9223372036854775808 < value < 9223372036854775807\"\n\
   \         else wrappedValue\n\
   \\n\
   \-- Non Standard Functions\n\
   \\n\
   \boolToBoolean :: Bool -> Type'ANON'BOOLEAN\n\
   \boolToBoolean True = Type'ANON'BOOLEAN'Iden'TRUE\n\
   \boolToBoolean False = Type'ANON'BOOLEAN'Iden'FALSE\n\
   \\n\
   \class SignalOutput a where\n\
   \   sigOut :: a -> String\n\
   \\n\
   \----------------------------------------------------------------------------------------\n\
   \\n\
   \--class VHDL_And a where\n\
   \--   and_V :: a -> a -> a\n\
   \--class VHDL_Or a where\n\
   \--   or_V :: a -> a -> a\n\
   \--class VHDL_Not a where\n\
   \--   not_V :: a -> a\n\
   \--class VHDL_Xor a where\n\
   \--   xor_V :: a -> a -> a\n\
   \--class VHDL_Nand a where\n\
   \--   nand_V :: a -> a -> a\n\
   \--class VHDL_Nor a where\n\
   \--   nor_V :: a -> a -> a\n\
   \--class VHDL_Eq a where\n\
   \--   eq_V :: a -> a -> BOOLEAN\n\
   \--   neq_V :: a -> a -> BOOLEAN\n\
   \--class VHDL_Eq a => VHDL_Ord a where\n\
   \--   lessThan_V :: a -> a -> BOOLEAN\n\
   \--   lessThanOrEqual_V :: a -> a -> BOOLEAN\n\
   \--   greaterThan_V :: a -> a -> BOOLEAN\n\
   \--   greaterThanOrEqual_V :: a -> a -> BOOLEAN\n\
   \--class VHDL_Add a where\n\
   \--   add_V :: a -> a -> a\n\
   \--class VHDL_Sub a where\n\
   \--   sub_V :: a -> a -> a\n\
   \--class VHDL_Mult a where\n\
   \--   mult_V :: a -> a -> a\n\
   \--class VHDL_Div a where\n\
   \--   div_V :: a -> a -> a\n\
   \--class VHDL_Mult_Phys a where\n\
   \--   mult_V_I :: a -> INTEGER -> a\n\
   \--   mult_V_R :: a -> REAL -> a\n\
   \--   mult_I_V :: INTEGER -> a -> a\n\
   \--   mult_R_V :: REAL -> a -> a\n\
   \--class VHDL_Div_Phys a where\n\
   \--   div_V_I :: a -> INTEGER -> a\n\
   \--   div_V_R :: a -> REAL -> a\n\
   \--   div_V_V :: a -> a -> Int64\n\
   \--class VHDL_Mod a where\n\
   \--   mod_V :: a -> a -> a\n\
   \--class VHDL_Rem a where\n\
   \--   rem_V :: a -> a -> a\n\
   \--class VHDL_Exp a where\n\
   \--   exp_V :: a -> a -> a\n\
   \--class VHDL_Abs a where\n\
   \--   abs_V :: a -> a\n\
   \--class VHDL_Scalar a where\n\
   \--   scalarLeft :: a\n\
   \--   scalarRight :: a\n\
   \--   scalarHigh :: a\n\
   \--   scalarLow :: a\n\
   \--class VHDL_Discrete a where\n\
   \--   discretePos :: a -> Int64\n\
   \--   discreteVal :: Int64 -> a\n\
   \--   discreteSucc :: a -> a\n\
   \--   discretePred :: a -> a\n\
   \--   discreteLeftOf :: a -> a\n\
   \--   discreteRightOf :: a -> a\n\
   \\n\
   \--meta'lessThanArray :: VHDL_Ord a => [a] -> [a] -> BOOLEAN\n\
   \--meta'lessThanArray (elem1:others1) (elem2:others2) =\n\
   \--   if lessThan_V elem1 elem2\n\
   \--      then BOOLEAN'IDEN'TRUE\n\
   \--      else if eq_V elem1 elem2\n\
   \--            then meta'lessThanArray others1 others2\n\
   \--            else BOOLEAN'IDEN'FALSE\n\
   \--meta'lessThanArray [] (_:_) = BOOLEAN'IDEN'TRUE\n\
   \--meta'lessThanArray _ _ = BOOLEAN'IDEN'FALSE\n\
   \--meta'greaterThanArray :: VHDL_Ord a => [a] -> [a] -> BOOLEAN\n\
   \--meta'greaterThanArray (elem1:others1) (elem2:others2) =\n\
   \--   if greaterThan_V elem1 elem2\n\
   \--      then BOOLEAN'IDEN'TRUE\n\
   \--      else if eq_V elem1 elem2\n\
   \--            then meta'greaterThanArray others1 others2\n\
   \--            else BOOLEAN'IDEN'FALSE\n\
   \--meta'greaterThanArray (_:_) [] = BOOLEAN'IDEN'TRUE\n\
   \--meta'greaterThanArray _ _ = BOOLEAN'IDEN'FALSE"
