module STD.STANDARD
   ( BOOLEAN(..)
   , BIT(..)
   , CHARACTER(..)
   , SEVERITY_LEVEL(..)
   , INTEGER(..)
   , REAL(..)
   , TIME(..)
   , now
   , NATURAL(..)
   , POSITIVE(..)
   , STRING(..)
   , ARRAY'STRING
   , BIT_VECTOR(..)
   , ARRAY'BIT_VECTOR
   -- Non standard type data
   , VHDL_Eq
   , VHDL_Ord
   , VHDL_Add
   , VHDL_Sub
   , VHDL_Mult
   , VHDL_Div
   , VHDL_And
   , VHDL_Or
   , VHDL_Not
   , VHDL_Xor
   , VHDL_Nand
   , VHDL_Nor
   , VHDL_Scalar
   , VHDL_Discrete
   , meta'boolToBoolean
   , meta'lessThanArray
   , meta'greaterThanArray
   ) where

import qualified Data.IntMap.Strict as META'IntMap
import Data.Int (Int64)

class VHDL_And a where
   and_V :: a -> a -> a
class VHDL_Or a where
   or_V :: a -> a -> a
class VHDL_Not a where
   not_V :: a -> a
class VHDL_Xor a where
   xor_V :: a -> a -> a
class VHDL_Nand a where
   nand_V :: a -> a -> a
class VHDL_Nor a where
   nor_V :: a -> a -> a
class VHDL_Eq a where
   eq_V :: a -> a -> BOOLEAN
   neq_V :: a -> a -> BOOLEAN
class VHDL_Eq a => VHDL_Ord a where
   lessThan_V :: a -> a -> BOOLEAN
   lessThanOrEqual_V :: a -> a -> BOOLEAN
   greaterThan_V :: a -> a -> BOOLEAN
   greaterThanOrEqual_V :: a -> a -> BOOLEAN
class VHDL_Add a where
   add_V :: a -> a -> a
class VHDL_Sub a where
   sub_V :: a -> a -> a
class VHDL_Mult a where
   mult_V :: a -> a -> a
class VHDL_Div a where
   div_V :: a -> a -> a
class VHDL_Mult_Phys a where
   mult_V_I :: a -> INTEGER -> a
   mult_V_R :: a -> REAL -> a
   mult_I_V :: INTEGER -> a -> a
   mult_R_V :: REAL -> a -> a
class VHDL_Div_Phys a where
   div_V_I :: a -> INTEGER -> a
   div_V_R :: a -> REAL -> a
   div_V_V :: a -> a -> Int64
class VHDL_Mod a where
   mod_V :: a -> a -> a
class VHDL_Rem a where
   rem_V :: a -> a -> a
class VHDL_Exp a where
   exp_V :: a -> a -> a
class VHDL_Abs a where
   abs_V :: a -> a
class VHDL_Scalar a where
   scalarLeft :: a
   scalarRight :: a
   scalarHigh :: a
   scalarLow :: a
class VHDL_Discrete a where
   discretePos :: a -> Int64
   discreteVal :: Int64 -> a
   discreteSucc :: a -> a
   discretePred :: a -> a
   discreteLeftOf :: a -> a
   discreteRightOf :: a -> a

meta'boolToBoolean :: Bool -> BOOLEAN
meta'boolToBoolean True = BOOLEAN'IDEN'TRUE
meta'boolToBoolean False = BOOLEAN'IDEN'FALSE
meta'lessThanArray :: VHDL_Ord a => [a] -> [a] -> BOOLEAN
meta'lessThanArray (elem1:others1) (elem2:others2) =
   if lessThan_V elem1 elem2
      then BOOLEAN'IDEN'TRUE
      else if eq_V elem1 elem2
            then meta'lessThanArray others1 others2
            else BOOLEAN'IDEN'FALSE
meta'lessThanArray [] (_:_) = BOOLEAN'IDEN'TRUE
meta'lessThanArray _ _ = BOOLEAN'IDEN'FALSE
meta'greaterThanArray :: VHDL_Ord a => [a] -> [a] -> BOOLEAN
meta'greaterThanArray (elem1:others1) (elem2:others2) =
   if greaterThan_V elem1 elem2
      then BOOLEAN'IDEN'TRUE
      else if eq_V elem1 elem2
            then meta'greaterThanArray others1 others2
            else BOOLEAN'IDEN'FALSE
meta'greaterThanArray (_:_) [] = BOOLEAN'IDEN'TRUE
meta'greaterThanArray _ _ = BOOLEAN'IDEN'FALSE

data BOOLEAN =
   BOOLEAN'IDEN'FALSE
   | BOOLEAN'IDEN'TRUE
instance VHDL_Eq BOOLEAN where
   eq_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE
   eq_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE
   eq_V _ _ = BOOLEAN'IDEN'FALSE
   neq_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE
   neq_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE
   neq_V _ _ = BOOLEAN'IDEN'FALSE
instance VHDL_Ord BOOLEAN where
   lessThan_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE
   lessThan_V _ _ = BOOLEAN'IDEN'FALSE
   lessThanOrEqual_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'FALSE
   lessThanOrEqual_V _ _ = BOOLEAN'IDEN'TRUE
   greaterThan_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE
   greaterThan_V _ _ = BOOLEAN'IDEN'FALSE
   greaterThanOrEqual_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'FALSE
   greaterThanOrEqual_V _ _ = BOOLEAN'IDEN'TRUE
instance VHDL_Scalar BOOLEAN where
   scalarLeft = BOOLEAN'IDEN'FALSE
   scalarRight = BOOLEAN'IDEN'TRUE
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Discrete BOOLEAN where
   discretePos BOOLEAN'IDEN'FALSE = 0
   discretePos BOOLEAN'IDEN'TRUE = 1
   discreteVal 0 = BOOLEAN'IDEN'FALSE
   discreteVal 1 = BOOLEAN'IDEN'TRUE
   discreteSucc BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE
   discretePred BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'FALSE
   discreteLeftOf BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'FALSE
   discreteRightOf BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE
instance VHDL_And BOOLEAN where
   and_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE
   and_V _ _ = BOOLEAN'IDEN'FALSE
instance VHDL_Or BOOLEAN where
   or_V BOOLEAN'IDEN'TRUE _ = BOOLEAN'IDEN'TRUE
   or_V _ BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE
   or_V _ _ = BOOLEAN'IDEN'FALSE
instance VHDL_Xor BOOLEAN where
   xor_V BOOLEAN'IDEN'TRUE BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE
   xor_V BOOLEAN'IDEN'FALSE BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'TRUE
   xor_V _ _ = BOOLEAN'IDEN'FALSE
instance VHDL_Not BOOLEAN where
   not_V BOOLEAN'IDEN'TRUE = BOOLEAN'IDEN'FALSE
   not_V BOOLEAN'IDEN'FALSE = BOOLEAN'IDEN'TRUE
instance VHDL_Nand BOOLEAN where
   nand_V v1 v2 = not_V $ and_V v1 v2
instance VHDL_Nor BOOLEAN where
   nor_V v1 v2 = not_V $ or_V v1 v2

data BIT =
   BIT'CHAR'0
   | BIT'CHAR'1
instance VHDL_Eq BIT where
   eq_V BIT'CHAR'0 BIT'CHAR'0 = BOOLEAN'IDEN'TRUE
   eq_V BIT'CHAR'1 BIT'CHAR'1 = BOOLEAN'IDEN'TRUE
   eq_V _ _ = BOOLEAN'IDEN'FALSE
   neq_V BIT'CHAR'0 BIT'CHAR'1 = BOOLEAN'IDEN'TRUE
   neq_V BIT'CHAR'1 BIT'CHAR'0 = BOOLEAN'IDEN'TRUE
   neq_V _ _ = BOOLEAN'IDEN'FALSE
instance VHDL_Ord BIT where
   lessThan_V BIT'CHAR'0 BIT'CHAR'1 = BOOLEAN'IDEN'TRUE
   lessThan_V _ _ = BOOLEAN'IDEN'FALSE
   lessThanOrEqual_V BIT'CHAR'1 BIT'CHAR'0 = BOOLEAN'IDEN'FALSE
   lessThanOrEqual_V _ _ = BOOLEAN'IDEN'TRUE
   greaterThan_V BIT'CHAR'1 BIT'CHAR'0 = BOOLEAN'IDEN'TRUE
   greaterThan_V _ _ = BOOLEAN'IDEN'FALSE
   greaterThanOrEqual_V BIT'CHAR'0 BIT'CHAR'1 = BOOLEAN'IDEN'FALSE
   greaterThanOrEqual_V _ _ = BOOLEAN'IDEN'TRUE
instance VHDL_Scalar BIT where
   scalarLeft = BOOLEAN'IDEN'FALSE
   scalarRight = BOOLEAN'IDEN'TRUE
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Discrete BIT where
   discretePos BIT'CHAR'0 = 0
   discretePos BIT'CHAR'1 = 1
   discreteVal 0 = BIT'CHAR'0
   discreteVal 1 = BIT'CHAR'1
   discreteSucc BIT'CHAR'0 = BIT'CHAR'1
   discretePred BIT'CHAR'1 = BIT'CHAR'0
   discreteLeftOf BIT'CHAR'1 = BIT'CHAR'0
   discreteRightOf BIT'CHAR'0 = BIT'CHAR'1
instance VHDL_And BIT where
   and_V BIT'CHAR'1 BIT'CHAR'1 = BIT'CHAR'1
   and_V _ _ = BIT'CHAR'0
instance VHDL_Or BIT where
   or_V BIT'CHAR'1 _ = BIT'CHAR'1
   or_V _ BIT'CHAR'1 = BIT'CHAR'1
   or_V _ _ = BIT'CHAR'0
instance VHDL_Xor BIT where
   xor_V BIT'CHAR'1 BIT'CHAR'0 = BIT'CHAR'1
   xor_V BIT'CHAR'0 BIT'CHAR'1 = BIT'CHAR'1
   xor_V _ _ = BIT'CHAR'0
instance VHDL_Not BIT where
   not_V BIT'CHAR'1 = BIT'CHAR'0
   not_V BIT'CHAR'0 = BIT'CHAR'1
instance VHDL_Nand BIT where
   nand_V v1 v2 = not_V $ and_V v1 v2
instance VHDL_Nor BIT where
   nor_V v1 v2 = not_V $ or_V v1 v2

data CHARACTER =
   CHARACTER'IDEN'NUL
   | CHARACTER'IDEN'SOH
   | CHARACTER'IDEN'STX
   | CHARACTER'IDEN'ETX
   | CHARACTER'IDEN'EOT
   | CHARACTER'IDEN'ENQ
   | CHARACTER'IDEN'ACK
   | CHARACTER'IDEN'BEL
   | CHARACTER'IDEN'BS
   | CHARACTER'IDEN'HT
   | CHARACTER'IDEN'LF
   | CHARACTER'IDEN'VT
   | CHARACTER'IDEN'FF
   | CHARACTER'IDEN'CR
   | CHARACTER'IDEN'SO
   | CHARACTER'IDEN'SI
   | CHARACTER'IDEN'DLE
   | CHARACTER'IDEN'DC1
   | CHARACTER'IDEN'DC2
   | CHARACTER'IDEN'DC3
   | CHARACTER'IDEN'DC4
   | CHARACTER'IDEN'NAK
   | CHARACTER'IDEN'SYN
   | CHARACTER'IDEN'ETB
   | CHARACTER'IDEN'CAN
   | CHARACTER'IDEN'EM
   | CHARACTER'IDEN'SUB
   | CHARACTER'IDEN'ESC
   | CHARACTER'IDEN'FSP
   | CHARACTER'IDEN'GSP
   | CHARACTER'IDEN'RSP
   | CHARACTER'IDEN'USP
   | CHARACTER'CHAR'SPACE
   | CHARACTER'CHAR'EXCLAMATION
   | CHARACTER'CHAR'QUOTE
   | CHARACTER'CHAR'HASH
   | CHARACTER'CHAR'DOLLAR
   | CHARACTER'CHAR'PERCENT
   | CHARACTER'CHAR'AMPERSAND
   | CHARACTER'CHAR'TICK
   | CHARACTER'CHAR'LEFTPAREN
   | CHARACTER'CHAR'RIGHTPAREN
   | CHARACTER'CHAR'STAR
   | CHARACTER'CHAR'PLUS
   | CHARACTER'CHAR'COMMA
   | CHARACTER'CHAR'HYPHEN
   | CHARACTER'CHAR'PERIOD
   | CHARACTER'CHAR'FORWARDSLASH
   | CHARACTER'CHAR'0
   | CHARACTER'CHAR'1
   | CHARACTER'CHAR'2
   | CHARACTER'CHAR'3
   | CHARACTER'CHAR'4
   | CHARACTER'CHAR'5
   | CHARACTER'CHAR'6
   | CHARACTER'CHAR'7
   | CHARACTER'CHAR'8
   | CHARACTER'CHAR'9
   | CHARACTER'CHAR'COLON
   | CHARACTER'CHAR'SEMICOLON
   | CHARACTER'CHAR'LESSTHAN
   | CHARACTER'CHAR'EQUAL
   | CHARACTER'CHAR'GREATERTHAN
   | CHARACTER'CHAR'QUESTION
   | CHARACTER'CHAR'AT
   | CHARACTER'CHAR'A
   | CHARACTER'CHAR'B
   | CHARACTER'CHAR'C
   | CHARACTER'CHAR'D
   | CHARACTER'CHAR'E
   | CHARACTER'CHAR'F
   | CHARACTER'CHAR'G
   | CHARACTER'CHAR'H
   | CHARACTER'CHAR'I
   | CHARACTER'CHAR'J
   | CHARACTER'CHAR'K
   | CHARACTER'CHAR'L
   | CHARACTER'CHAR'M
   | CHARACTER'CHAR'N
   | CHARACTER'CHAR'O
   | CHARACTER'CHAR'P
   | CHARACTER'CHAR'Q
   | CHARACTER'CHAR'R
   | CHARACTER'CHAR'S
   | CHARACTER'CHAR'T
   | CHARACTER'CHAR'U
   | CHARACTER'CHAR'V
   | CHARACTER'CHAR'W
   | CHARACTER'CHAR'X
   | CHARACTER'CHAR'Y
   | CHARACTER'CHAR'Z
   | CHARACTER'CHAR'LEFTSQUAREBRACE
   | CHARACTER'CHAR'BACKSLASH
   | CHARACTER'CHAR'RIGHTSQUAREBRACE
   | CHARACTER'CHAR'CAROT
   | CHARACTER'CHAR'UNDERSCORE
   | CHARACTER'CHAR'BACKTICK
   | CHARACTER'CHAR'a
   | CHARACTER'CHAR'b
   | CHARACTER'CHAR'c
   | CHARACTER'CHAR'd
   | CHARACTER'CHAR'e
   | CHARACTER'CHAR'f
   | CHARACTER'CHAR'g
   | CHARACTER'CHAR'h
   | CHARACTER'CHAR'i
   | CHARACTER'CHAR'j
   | CHARACTER'CHAR'k
   | CHARACTER'CHAR'l
   | CHARACTER'CHAR'm
   | CHARACTER'CHAR'n
   | CHARACTER'CHAR'o
   | CHARACTER'CHAR'p
   | CHARACTER'CHAR'q
   | CHARACTER'CHAR'r
   | CHARACTER'CHAR's
   | CHARACTER'CHAR't
   | CHARACTER'CHAR'u
   | CHARACTER'CHAR'v
   | CHARACTER'CHAR'w
   | CHARACTER'CHAR'x
   | CHARACTER'CHAR'y
   | CHARACTER'CHAR'z
   | CHARACTER'CHAR'LEFTBRACE
   | CHARACTER'CHAR'BAR
   | CHARACTER'CHAR'RIGHTBRACE
   | CHARACTER'CHAR'TILDE
   | CHARACTER'IDEN'DEL
instance VHDL_Eq CHARACTER where
   eq_V CHARACTER'IDEN'NUL CHARACTER'IDEN'NUL = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'SOH CHARACTER'IDEN'SOH = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'STX CHARACTER'IDEN'STX = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'ETX CHARACTER'IDEN'ETX = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'EOT CHARACTER'IDEN'EOT = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'ENQ CHARACTER'IDEN'ENQ = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'ACK CHARACTER'IDEN'ACK = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'BEL CHARACTER'IDEN'BEL = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'BS CHARACTER'IDEN'BS = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'HT CHARACTER'IDEN'HT = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'LF CHARACTER'IDEN'LF = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'VT CHARACTER'IDEN'VT = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'FF CHARACTER'IDEN'FF = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'CR CHARACTER'IDEN'CR = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'SO CHARACTER'IDEN'SO = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'SI CHARACTER'IDEN'SI = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'DLE CHARACTER'IDEN'DLE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'DC1 CHARACTER'IDEN'DC1 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'DC2 CHARACTER'IDEN'DC2 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'DC3 CHARACTER'IDEN'DC3 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'DC4 CHARACTER'IDEN'DC4 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'NAK CHARACTER'IDEN'NAK = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'SYN CHARACTER'IDEN'SYN = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'ETB CHARACTER'IDEN'ETB = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'CAN CHARACTER'IDEN'CAN = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'EM CHARACTER'IDEN'EM = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'SUB CHARACTER'IDEN'SUB = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'ESC CHARACTER'IDEN'ESC = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'FSP CHARACTER'IDEN'FSP = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'GSP CHARACTER'IDEN'GSP = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'RSP CHARACTER'IDEN'RSP = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'USP CHARACTER'IDEN'USP = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'SPACE CHARACTER'CHAR'SPACE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'EXCLAMATION CHARACTER'CHAR'EXCLAMATION = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'QUOTE CHARACTER'CHAR'QUOTE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'HASH CHARACTER'CHAR'HASH = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'DOLLAR CHARACTER'CHAR'DOLLAR = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'PERCENT CHARACTER'CHAR'PERCENT = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'AMPERSAND CHARACTER'CHAR'AMPERSAND = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'TICK CHARACTER'CHAR'TICK = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'LEFTPAREN CHARACTER'CHAR'LEFTPAREN = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'RIGHTPAREN CHARACTER'CHAR'RIGHTPAREN = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'STAR CHARACTER'CHAR'STAR = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'PLUS CHARACTER'CHAR'PLUS = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'COMMA CHARACTER'CHAR'COMMA = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'HYPHEN CHARACTER'CHAR'HYPHEN = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'PERIOD CHARACTER'CHAR'PERIOD = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'FORWARDSLASH CHARACTER'CHAR'FORWARDSLASH = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'0 CHARACTER'CHAR'0 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'1 CHARACTER'CHAR'1 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'2 CHARACTER'CHAR'2 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'3 CHARACTER'CHAR'3 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'4 CHARACTER'CHAR'4 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'5 CHARACTER'CHAR'5 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'6 CHARACTER'CHAR'6 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'7 CHARACTER'CHAR'7 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'8 CHARACTER'CHAR'8 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'9 CHARACTER'CHAR'9 = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'COLON CHARACTER'CHAR'COLON = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'SEMICOLON CHARACTER'CHAR'SEMICOLON = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'LESSTHAN CHARACTER'CHAR'LESSTHAN = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'EQUAL CHARACTER'CHAR'EQUAL = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'GREATERTHAN CHARACTER'CHAR'GREATERTHAN = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'QUESTION CHARACTER'CHAR'QUESTION = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'AT CHARACTER'CHAR'AT = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'A CHARACTER'CHAR'A = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'B CHARACTER'CHAR'B = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'C CHARACTER'CHAR'C = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'D CHARACTER'CHAR'D = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'E CHARACTER'CHAR'E = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'F CHARACTER'CHAR'F = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'G CHARACTER'CHAR'G = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'H CHARACTER'CHAR'H = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'I CHARACTER'CHAR'I = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'J CHARACTER'CHAR'J = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'K CHARACTER'CHAR'K = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'L CHARACTER'CHAR'L = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'M CHARACTER'CHAR'M = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'N CHARACTER'CHAR'N = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'O CHARACTER'CHAR'O = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'P CHARACTER'CHAR'P = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'Q CHARACTER'CHAR'Q = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'R CHARACTER'CHAR'R = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'S CHARACTER'CHAR'S = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'T CHARACTER'CHAR'T = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'U CHARACTER'CHAR'U = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'V CHARACTER'CHAR'V = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'W CHARACTER'CHAR'W = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'X CHARACTER'CHAR'X = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'Y CHARACTER'CHAR'Y = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'Z CHARACTER'CHAR'Z = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'LEFTSQUAREBRACE CHARACTER'CHAR'LEFTSQUAREBRACE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'BACKSLASH CHARACTER'CHAR'BACKSLASH = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'RIGHTSQUAREBRACE CHARACTER'CHAR'RIGHTSQUAREBRACE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'CAROT CHARACTER'CHAR'CAROT = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'UNDERSCORE CHARACTER'CHAR'UNDERSCORE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'BACKTICK CHARACTER'CHAR'BACKTICK = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'a CHARACTER'CHAR'a = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'b CHARACTER'CHAR'b = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'c CHARACTER'CHAR'c = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'd CHARACTER'CHAR'd = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'e CHARACTER'CHAR'e = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'f CHARACTER'CHAR'f = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'g CHARACTER'CHAR'g = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'h CHARACTER'CHAR'h = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'i CHARACTER'CHAR'i = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'j CHARACTER'CHAR'j = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'k CHARACTER'CHAR'k = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'l CHARACTER'CHAR'l = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'm CHARACTER'CHAR'm = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'n CHARACTER'CHAR'n = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'o CHARACTER'CHAR'o = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'p CHARACTER'CHAR'p = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'q CHARACTER'CHAR'q = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'r CHARACTER'CHAR'r = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR's CHARACTER'CHAR's = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR't CHARACTER'CHAR't = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'u CHARACTER'CHAR'u = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'v CHARACTER'CHAR'v = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'w CHARACTER'CHAR'w = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'x CHARACTER'CHAR'x = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'y CHARACTER'CHAR'y = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'z CHARACTER'CHAR'z = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'LEFTBRACE CHARACTER'CHAR'LEFTBRACE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'BAR CHARACTER'CHAR'BAR = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'RIGHTBRACE CHARACTER'CHAR'RIGHTBRACE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'CHAR'TILDE CHARACTER'CHAR'TILDE = BOOLEAN'IDEN'TRUE
   eq_V CHARACTER'IDEN'DEL CHARACTER'IDEN'DEL = BOOLEAN'IDEN'TRUE
   neq_V ch1 ch2 = not_V $ eq_V ch1 ch2
instance VHDL_Scalar CHARACTER where
   scalarLeft = CHARACTER'IDEN'NUL
   scalarRight = CHARACTER'IDEN'DEL
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Discrete CHARACTER where
   discretePos CHARACTER'IDEN'NUL = 0
   discretePos CHARACTER'IDEN'SOH = 1
   discretePos CHARACTER'IDEN'STX = 2
   discretePos CHARACTER'IDEN'ETX = 3
   discretePos CHARACTER'IDEN'EOT = 4
   discretePos CHARACTER'IDEN'ENQ = 5
   discretePos CHARACTER'IDEN'ACK = 6
   discretePos CHARACTER'IDEN'BEL = 7
   discretePos CHARACTER'IDEN'BS = 8
   discretePos CHARACTER'IDEN'HT = 9
   discretePos CHARACTER'IDEN'LF = 10
   discretePos CHARACTER'IDEN'VT = 11
   discretePos CHARACTER'IDEN'FF = 12
   discretePos CHARACTER'IDEN'CR = 13
   discretePos CHARACTER'IDEN'SO = 14
   discretePos CHARACTER'IDEN'SI = 15
   discretePos CHARACTER'IDEN'DLE = 16
   discretePos CHARACTER'IDEN'DC1 = 17
   discretePos CHARACTER'IDEN'DC2 = 18
   discretePos CHARACTER'IDEN'DC3 = 19
   discretePos CHARACTER'IDEN'DC4 = 20
   discretePos CHARACTER'IDEN'NAK = 21
   discretePos CHARACTER'IDEN'SYN = 22
   discretePos CHARACTER'IDEN'ETB = 23
   discretePos CHARACTER'IDEN'CAN = 24
   discretePos CHARACTER'IDEN'EM = 25
   discretePos CHARACTER'IDEN'SUB = 26
   discretePos CHARACTER'IDEN'ESC = 27
   discretePos CHARACTER'IDEN'FSP = 28
   discretePos CHARACTER'IDEN'GSP = 29
   discretePos CHARACTER'IDEN'RSP = 30
   discretePos CHARACTER'IDEN'USP = 31
   discretePos CHARACTER'CHAR'SPACE = 32
   discretePos CHARACTER'CHAR'EXCLAMATION = 33
   discretePos CHARACTER'CHAR'QUOTE = 34
   discretePos CHARACTER'CHAR'HASH = 35
   discretePos CHARACTER'CHAR'DOLLAR = 36
   discretePos CHARACTER'CHAR'PERCENT = 37
   discretePos CHARACTER'CHAR'AMPERSAND = 38
   discretePos CHARACTER'CHAR'TICK = 39
   discretePos CHARACTER'CHAR'LEFTPAREN = 40
   discretePos CHARACTER'CHAR'RIGHTPAREN = 41
   discretePos CHARACTER'CHAR'STAR = 42
   discretePos CHARACTER'CHAR'PLUS = 43
   discretePos CHARACTER'CHAR'COMMA = 44
   discretePos CHARACTER'CHAR'HYPHEN = 45
   discretePos CHARACTER'CHAR'PERIOD = 46
   discretePos CHARACTER'CHAR'FORWARDSLASH = 47
   discretePos CHARACTER'CHAR'0 = 48
   discretePos CHARACTER'CHAR'1 = 49
   discretePos CHARACTER'CHAR'2 = 50
   discretePos CHARACTER'CHAR'3 = 51
   discretePos CHARACTER'CHAR'4 = 52
   discretePos CHARACTER'CHAR'5 = 53
   discretePos CHARACTER'CHAR'6 = 54
   discretePos CHARACTER'CHAR'7 = 55
   discretePos CHARACTER'CHAR'8 = 56
   discretePos CHARACTER'CHAR'9 = 57
   discretePos CHARACTER'CHAR'COLON = 58
   discretePos CHARACTER'CHAR'SEMICOLON = 59
   discretePos CHARACTER'CHAR'LESSTHAN = 60
   discretePos CHARACTER'CHAR'EQUAL = 61
   discretePos CHARACTER'CHAR'GREATERTHAN = 62
   discretePos CHARACTER'CHAR'QUESTION = 63
   discretePos CHARACTER'CHAR'AT = 64
   discretePos CHARACTER'CHAR'A = 65
   discretePos CHARACTER'CHAR'B = 66
   discretePos CHARACTER'CHAR'C = 67
   discretePos CHARACTER'CHAR'D = 68
   discretePos CHARACTER'CHAR'E = 69
   discretePos CHARACTER'CHAR'F = 70
   discretePos CHARACTER'CHAR'G = 71
   discretePos CHARACTER'CHAR'H = 72
   discretePos CHARACTER'CHAR'I = 73
   discretePos CHARACTER'CHAR'J = 74
   discretePos CHARACTER'CHAR'K = 75
   discretePos CHARACTER'CHAR'L = 76
   discretePos CHARACTER'CHAR'M = 77
   discretePos CHARACTER'CHAR'N = 78
   discretePos CHARACTER'CHAR'O = 79
   discretePos CHARACTER'CHAR'P = 80
   discretePos CHARACTER'CHAR'Q = 81
   discretePos CHARACTER'CHAR'R = 82
   discretePos CHARACTER'CHAR'S = 83
   discretePos CHARACTER'CHAR'T = 84
   discretePos CHARACTER'CHAR'U = 85
   discretePos CHARACTER'CHAR'V = 86
   discretePos CHARACTER'CHAR'W = 87
   discretePos CHARACTER'CHAR'X = 88
   discretePos CHARACTER'CHAR'Y = 89
   discretePos CHARACTER'CHAR'Z = 90
   discretePos CHARACTER'CHAR'LEFTSQUAREBRACE = 91
   discretePos CHARACTER'CHAR'BACKSLASH = 92
   discretePos CHARACTER'CHAR'RIGHTSQUAREBRACE = 93
   discretePos CHARACTER'CHAR'CAROT = 94
   discretePos CHARACTER'CHAR'UNDERSCORE = 95
   discretePos CHARACTER'CHAR'BACKTICK = 96
   discretePos CHARACTER'CHAR'a = 97
   discretePos CHARACTER'CHAR'b = 98
   discretePos CHARACTER'CHAR'c = 99
   discretePos CHARACTER'CHAR'd = 100
   discretePos CHARACTER'CHAR'e = 101
   discretePos CHARACTER'CHAR'f = 102
   discretePos CHARACTER'CHAR'g = 103
   discretePos CHARACTER'CHAR'h = 104
   discretePos CHARACTER'CHAR'i = 105
   discretePos CHARACTER'CHAR'j = 106
   discretePos CHARACTER'CHAR'k = 107
   discretePos CHARACTER'CHAR'l = 108
   discretePos CHARACTER'CHAR'm = 109
   discretePos CHARACTER'CHAR'n = 110
   discretePos CHARACTER'CHAR'o = 111
   discretePos CHARACTER'CHAR'p = 112
   discretePos CHARACTER'CHAR'q = 113
   discretePos CHARACTER'CHAR'r = 114
   discretePos CHARACTER'CHAR's = 115
   discretePos CHARACTER'CHAR't = 116
   discretePos CHARACTER'CHAR'u = 117
   discretePos CHARACTER'CHAR'v = 118
   discretePos CHARACTER'CHAR'w = 119
   discretePos CHARACTER'CHAR'x = 120
   discretePos CHARACTER'CHAR'y = 121
   discretePos CHARACTER'CHAR'z = 122
   discretePos CHARACTER'CHAR'LEFTBRACE = 123
   discretePos CHARACTER'CHAR'BAR = 124
   discretePos CHARACTER'CHAR'RIGHTBRACE = 125
   discretePos CHARACTER'CHAR'TILDE = 126
   discretePos CHARACTER'IDEN'DEL = 127
   discreteVal 0 = CHARACTER'IDEN'NUL
   discreteVal 1 = CHARACTER'IDEN'SOH
   discreteVal 2 = CHARACTER'IDEN'STX
   discreteVal 3 = CHARACTER'IDEN'ETX
   discreteVal 4 = CHARACTER'IDEN'EOT
   discreteVal 5 = CHARACTER'IDEN'ENQ
   discreteVal 6 = CHARACTER'IDEN'ACK
   discreteVal 7 = CHARACTER'IDEN'BEL
   discreteVal 8 = CHARACTER'IDEN'BS
   discreteVal 9 = CHARACTER'IDEN'HT
   discreteVal 10 = CHARACTER'IDEN'LF
   discreteVal 11 = CHARACTER'IDEN'VT
   discreteVal 12 = CHARACTER'IDEN'FF
   discreteVal 13 = CHARACTER'IDEN'CR
   discreteVal 14 = CHARACTER'IDEN'SO
   discreteVal 15 = CHARACTER'IDEN'SI
   discreteVal 16 = CHARACTER'IDEN'DLE
   discreteVal 17 = CHARACTER'IDEN'DC1
   discreteVal 18 = CHARACTER'IDEN'DC2
   discreteVal 19 = CHARACTER'IDEN'DC3
   discreteVal 20 = CHARACTER'IDEN'DC4
   discreteVal 21 = CHARACTER'IDEN'NAK
   discreteVal 22 = CHARACTER'IDEN'SYN
   discreteVal 23 = CHARACTER'IDEN'ETB
   discreteVal 24 = CHARACTER'IDEN'CAN
   discreteVal 25 = CHARACTER'IDEN'EM
   discreteVal 26 = CHARACTER'IDEN'SUB
   discreteVal 27 = CHARACTER'IDEN'ESC
   discreteVal 28 = CHARACTER'IDEN'FSP
   discreteVal 29 = CHARACTER'IDEN'GSP
   discreteVal 30 = CHARACTER'IDEN'RSP
   discreteVal 31 = CHARACTER'IDEN'USP
   discreteVal 32 = CHARACTER'CHAR'SPACE
   discreteVal 33 = CHARACTER'CHAR'EXCLAMATION
   discreteVal 34 = CHARACTER'CHAR'QUOTE
   discreteVal 35 = CHARACTER'CHAR'HASH
   discreteVal 36 = CHARACTER'CHAR'DOLLAR
   discreteVal 37 = CHARACTER'CHAR'PERCENT
   discreteVal 38 = CHARACTER'CHAR'AMPERSAND
   discreteVal 39 = CHARACTER'CHAR'TICK
   discreteVal 40 = CHARACTER'CHAR'LEFTPAREN
   discreteVal 41 = CHARACTER'CHAR'RIGHTPAREN
   discreteVal 42 = CHARACTER'CHAR'STAR
   discreteVal 43 = CHARACTER'CHAR'PLUS
   discreteVal 44 = CHARACTER'CHAR'COMMA
   discreteVal 45 = CHARACTER'CHAR'HYPHEN
   discreteVal 46 = CHARACTER'CHAR'PERIOD
   discreteVal 47 = CHARACTER'CHAR'FORWARDSLASH
   discreteVal 48 = CHARACTER'CHAR'0
   discreteVal 49 = CHARACTER'CHAR'1
   discreteVal 50 = CHARACTER'CHAR'2
   discreteVal 51 = CHARACTER'CHAR'3
   discreteVal 52 = CHARACTER'CHAR'4
   discreteVal 53 = CHARACTER'CHAR'5
   discreteVal 54 = CHARACTER'CHAR'6
   discreteVal 55 = CHARACTER'CHAR'7
   discreteVal 56 = CHARACTER'CHAR'8
   discreteVal 57 = CHARACTER'CHAR'9
   discreteVal 58 = CHARACTER'CHAR'COLON
   discreteVal 59 = CHARACTER'CHAR'SEMICOLON
   discreteVal 60 = CHARACTER'CHAR'LESSTHAN
   discreteVal 61 = CHARACTER'CHAR'EQUAL
   discreteVal 62 = CHARACTER'CHAR'GREATERTHAN
   discreteVal 63 = CHARACTER'CHAR'QUESTION
   discreteVal 64 = CHARACTER'CHAR'AT
   discreteVal 65 = CHARACTER'CHAR'A
   discreteVal 66 = CHARACTER'CHAR'B
   discreteVal 67 = CHARACTER'CHAR'C
   discreteVal 68 = CHARACTER'CHAR'D
   discreteVal 69 = CHARACTER'CHAR'E
   discreteVal 70 = CHARACTER'CHAR'F
   discreteVal 71 = CHARACTER'CHAR'G
   discreteVal 72 = CHARACTER'CHAR'H
   discreteVal 73 = CHARACTER'CHAR'I
   discreteVal 74 = CHARACTER'CHAR'J
   discreteVal 75 = CHARACTER'CHAR'K
   discreteVal 76 = CHARACTER'CHAR'L
   discreteVal 77 = CHARACTER'CHAR'M
   discreteVal 78 = CHARACTER'CHAR'N
   discreteVal 79 = CHARACTER'CHAR'O
   discreteVal 80 = CHARACTER'CHAR'P
   discreteVal 81 = CHARACTER'CHAR'Q
   discreteVal 82 = CHARACTER'CHAR'R
   discreteVal 83 = CHARACTER'CHAR'S
   discreteVal 84 = CHARACTER'CHAR'T
   discreteVal 85 = CHARACTER'CHAR'U
   discreteVal 86 = CHARACTER'CHAR'V
   discreteVal 87 = CHARACTER'CHAR'W
   discreteVal 88 = CHARACTER'CHAR'X
   discreteVal 89 = CHARACTER'CHAR'Y
   discreteVal 90 = CHARACTER'CHAR'Z
   discreteVal 91 = CHARACTER'CHAR'LEFTSQUAREBRACE
   discreteVal 92 = CHARACTER'CHAR'BACKSLASH
   discreteVal 93 = CHARACTER'CHAR'RIGHTSQUAREBRACE
   discreteVal 94 = CHARACTER'CHAR'CAROT
   discreteVal 95 = CHARACTER'CHAR'UNDERSCORE
   discreteVal 96 = CHARACTER'CHAR'BACKTICK
   discreteVal 97 = CHARACTER'CHAR'a
   discreteVal 98 = CHARACTER'CHAR'b
   discreteVal 99 = CHARACTER'CHAR'c
   discreteVal 100 = CHARACTER'CHAR'd
   discreteVal 101 = CHARACTER'CHAR'e
   discreteVal 102 = CHARACTER'CHAR'f
   discreteVal 103 = CHARACTER'CHAR'g
   discreteVal 104 = CHARACTER'CHAR'h
   discreteVal 105 = CHARACTER'CHAR'i
   discreteVal 106 = CHARACTER'CHAR'j
   discreteVal 107 = CHARACTER'CHAR'k
   discreteVal 108 = CHARACTER'CHAR'l
   discreteVal 109 = CHARACTER'CHAR'm
   discreteVal 110 = CHARACTER'CHAR'n
   discreteVal 111 = CHARACTER'CHAR'o
   discreteVal 112 = CHARACTER'CHAR'p
   discreteVal 113 = CHARACTER'CHAR'q
   discreteVal 114 = CHARACTER'CHAR'r
   discreteVal 115 = CHARACTER'CHAR's
   discreteVal 116 = CHARACTER'CHAR't
   discreteVal 117 = CHARACTER'CHAR'u
   discreteVal 118 = CHARACTER'CHAR'v
   discreteVal 119 = CHARACTER'CHAR'w
   discreteVal 120 = CHARACTER'CHAR'x
   discreteVal 121 = CHARACTER'CHAR'y
   discreteVal 122 = CHARACTER'CHAR'z
   discreteVal 123 = CHARACTER'CHAR'LEFTBRACE
   discreteVal 124 = CHARACTER'CHAR'BAR
   discreteVal 125 = CHARACTER'CHAR'RIGHTBRACE
   discreteVal 126 = CHARACTER'CHAR'TILDE
   discreteVal 127 = CHARACTER'IDEN'DEL
   discreteVal val = error "Out of bounds CHARACTER value " ++ show val
   discreteSucc chr = discreteVal $ (discretePos chr) + 1
   discretePred chr = discreteVal $ (discretePos chr) - 1
   discreteLeftOf chr = discreteVal $ (discretePos chr) - 1
   discreteRightOf chr = discreteVal $ (discretePos chr) + 1
instance VHDL_Ord CHARACTER where
   lessThan_V chr1 chr2 =
      meta'boolToBoolean $ (discretePos chr1) < (discretePos chr2)
   lessThanOrEqual_V chr1 chr2 =
      meta'boolToBoolean $ (discretePos chr1) <= (discretePos chr2)
   greaterThan_V chr1 chr2 =
      meta'boolToBoolean $ (discretePos chr1) > (discretePos chr2)
   greaterThanOrEqual_V chr1 chr2 =
      meta'boolToBoolean $ (discretePos chr1) >= (discretePos chr2)

data SEVERITY_LEVEL =
   SEVERITY_LEVEL'IDEN'NOTE
   | SEVERITY_LEVEL'IDEN'WARNING
   | SEVERITY_LEVEL'IDEN'ERROR
   | SEVERITY_LEVEL'IDEN'FAILURE
instance VHDL_Eq SEVERITY_LEVEL where
   eq_V SEVERITY_LEVEL'IDEN'NOTE SEVERITY_LEVEL'IDEN'NOTE = BOOLEAN'IDEN'TRUE
   eq_V SEVERITY_LEVEL'IDEN'WARNING SEVERITY_LEVEL'IDEN'WARNING = BOOLEAN'IDEN'TRUE
   eq_V SEVERITY_LEVEL'IDEN'ERROR SEVERITY_LEVEL'IDEN'ERROR = BOOLEAN'IDEN'TRUE
   eq_V SEVERITY_LEVEL'IDEN'FAILURE SEVERITY_LEVEL'IDEN'FAILURE = BOOLEAN'IDEN'TRUE
   eq_V _ _ = BOOLEAN'IDEN'FALSE
   neq_V chr1 chr2 = not_V $ eq_V chr1 chr2
instance VHDL_Scalar SEVERITY_LEVEL where
   scalarLeft = SEVERITY_LEVEL'IDEN'NOTE
   scalarRight = SEVERITY_LEVEL'IDEN'FAILURE
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Discrete SEVERITY_LEVEL where
   discretePos SEVERITY_LEVEL'IDEN'NOTE = 0
   discretePos SEVERITY_LEVEL'IDEN'WARNING = 1
   discretePos SEVERITY_LEVEL'IDEN'ERROR = 2
   discretePos SEVERITY_LEVEL'IDEN'FAILURE = 3
   discreteVal 0 = SEVERITY_LEVEL'IDEN'NOTE
   discreteVal 1 = SEVERITY_LEVEL'IDEN'WARNING
   discreteVal 2 = SEVERITY_LEVEL'IDEN'ERROR
   discreteVal 3 = SEVERITY_LEVEL'IDEN'FAILURE
   discreteVal val = error "Out of bounds SEVERITY_LEVEL value " ++ show val
   discreteSucc SEVERITY_LEVEL'IDEN'NOTE = SEVERITY_LEVEL'IDEN'WARNING
   discreteSucc SEVERITY_LEVEL'IDEN'WARNING = SEVERITY_LEVEL'IDEN'ERROR
   discreteSucc SEVERITY_LEVEL'IDEN'ERROR = SEVERITY_LEVEL'IDEN'FAILURE
   discretePred SEVERITY_LEVEL'IDEN'WARNING = SEVERITY_LEVEL'IDEN'NOTE
   discretePred SEVERITY_LEVEL'IDEN'ERROR = SEVERITY_LEVEL'IDEN'WARNING
   discretePred SEVERITY_LEVEL'IDEN'FAILURE = SEVERITY_LEVEL'IDEN'ERROR
   discreteLeftOf SEVERITY_LEVEL'IDEN'WARNING = SEVERITY_LEVEL'IDEN'NOTE
   discreteLeftOf SEVERITY_LEVEL'IDEN'ERROR = SEVERITY_LEVEL'IDEN'WARNING
   discreteLeftOf SEVERITY_LEVEL'IDEN'FAILURE = SEVERITY_LEVEL'IDEN'ERROR
   discreteRightOf SEVERITY_LEVEL'IDEN'NOTE = SEVERITY_LEVEL'IDEN'WARNING
   discreteRightOf SEVERITY_LEVEL'IDEN'WARNING = SEVERITY_LEVEL'IDEN'ERROR
   discreteRightOf SEVERITY_LEVEL'IDEN'ERROR = SEVERITY_LEVEL'IDEN'FAILURE
instance VHDL_Ord SEVERITY_LEVEL where
   lessThan_V level1 level2 =
      meta'boolToBoolean $ (discretePos level1) < (discretePos level2)
   lessThanOrEqual_V level1 level2 =
      meta'boolToBoolean $ (discretePos level1) <= (discretePos level2)
   greaterThan_V level1 level2 =
      meta'boolToBoolean $ (discretePos level1) > (discretePos level2)
   greaterThanOrEqual_V level1 level2 =
      meta'boolToBoolean $ (discretePos level1) >= (discretePos level2)

newtype INTEGER = INTEGER Int64
instance VHDL_Scalar INTEGER where
   scalarLeft = INTEGER $ minBound
   scalarRight = INTEGER $ maxBound
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Eq INTEGER where
   eq_V (INTEGER val1) (INTEGER val2) =
      meta'boolToBoolean $ val1 == val2
   neq_V (INTEGER val1) (INTEGER val2) =
      meta'boolToBoolean $ val1 /= val2
instance VHDL_Ord INTEGER where
   lessThan_V (INTEGER val1) (INTEGER val2) =
      meta'boolToBoolean $ val1 < val2
   lessThanOrEqual_V (INTEGER val1) (INTEGER val2) =
      meta'boolToBoolean $ val1 <= val2
   greaterThan_V (INTEGER val1) (INTEGER val2) =
      meta'boolToBoolean $ val1 > val2
   greaterThanOrEqual_V (INTEGER val1) (INTEGER val2) =
      meta'boolToBoolean $ val1 >= val2
mkINTEGER :: Integer -> INTEGER
mkINTEGER val =
   let maxVal = case scalarHigh of (INTEGER v1) -> toInteger v1
       minVal = case scalarLow of (INTEGER v1) -> toInteger v1
   in if val > maxVal || val < minVal
         then error "Out of bounds INTEGER " ++ show val
         else INTEGER $ fromInteger
instance VHDL_Add INTEGER where
   add_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) + (toInteger val2)
instance VHDL_Sub INTEGER where
   sub_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) - (toInteger val2)
instance VHDL_Mult INTEGER where
   mult_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) * (toInteger val2)
instance VHDL_Div INTEGER where
   div_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) / (toInteger val2)
instance VHDL_Mod INTEGER where
   mod_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) `mod` (toInteger val2)
instance VHDL_Rem INTEGER where
   rem_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ (toInteger val1) `rem` (toInteger val2)
instance VHDL_Abs INTEGER where
   abs_V (INTEGER val) = mkINTEGER $ toInteger $ abs val
instance VHDL_Exp INTEGER where
   exp_V (INTEGER val1) (INTEGER val2) = mkINTEGER $ floor $ (fromIntegral val1) ^^ (toInteger val2)
instance VHDL_Discrete BOOLEAN where
   discretePos (INTEGER val) = val
   discreteVal val = mkINTEGER val
   discreteSucc (INTEGER val) = mkINTEGER $ (toInteger val) + 1
   discretePred (INTEGER val) = mkINTEGER $ (toInteger val) - 1
   discreteLeftOf = discretePred
   discreteRightOf = discreteSucc

newtype REAL = REAL Double
instance VHDL_Scalar REAL where
   scalarLeft = REAL $ minBound
   scalarRight = REAL $ maxBound
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Eq REAL where
   eq_V (REAL val1) (REAL val2) =
      meta'boolToBoolean $ val1 == val2
   neq_V (REAL val1) (REAL val2) =
      meta'boolToBoolean $ val1 /= val2
instance VHDL_Ord REAL where
   lessThan_V (REAL val1) (REAL val2) =
      meta'boolToBoolean $ val1 < val2
   lessThanOrEqual_V (REAL val1) (REAL val2) =
      meta'boolToBoolean $ val1 <= val2
   greaterThan_V (REAL val1) (REAL val2) =
      meta'boolToBoolean $ val1 > val2
   greaterThanOrEqual_V (REAL val1) (REAL val2) =
      meta'boolToBoolean $ val1 >= val2
instance VHDL_Add REAL where
   add_V (REAL val1) (REAL val2) = REAL $ val1 + val2
instance VHDL_Sub REAL where
   sub_V (REAL val1) (REAL val2) = REAL $ val1 - val2
instance VHDL_Mult REAL where
   mult_V (REAL val1) (REAL val2) = REAL $ val1 * val2
instance VHDL_Div REAL where
   div_V (REAL val1) (REAL val2) = REAL $ val1 / val2
instance VHDL_Abs REAL where
   abs_V (REAL val) = REAL $ abs val
instance VHDL_Exp REAL where
   exp_V (REAL val1) (INTEGER val2) = REAL $ val1 ^^ val2

newtype TIME = TIME Int64
instance VHDL_Scalar TIME where
   scalarLeft = TIME $ minBound
   scalarRight = TIME $ maxBound
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Eq TIME where
   eq_V (TIME val1) (TIME val2) =
      meta'boolToBoolean $ val1 == val2
   neq_V (TIME val1) (TIME val2) =
      meta'boolToBoolean $ val1 /= val2
instance VHDL_Ord TIME where
   lessThan_V (TIME val1) (TIME val2) =
      meta'boolToBoolean $ val1 < val2
   lessThanOrEqual_V (TIME val1) (TIME val2) =
      meta'boolToBoolean $ val1 <= val2
   greaterThan_V (TIME val1) (TIME val2) =
      meta'boolToBoolean $ val1 > val2
   greaterThanOrEqual_V (TIME val1) (TIME val2) =
      meta'boolToBoolean $ val1 >= val2
mkTIME :: Integer -> TIME
mkTIME val =
   let maxVal = case scalarHigh of (TIME v1) -> toInteger v1
       minVal = case scalarLow of (TIME v1) -> toInteger v1
   in if val > maxVal || val < minVal
         then error "Out of bounds TIME " ++ show val
         else TIME $ fromInteger
instance VHDL_Add TIME where
   add_V (TIME val1) (TIME val2) = mkTIME $ (toInteger val1) + (toInteger val2)
instance VHDL_Sub TIME where
   sub_V (TIME val1) (TIME val2) = mkTIME $ (toInteger val1) - (toInteger val2)
instance VHDL_Mult_Phys TIME where
   mult_V_I (TIME val1) (INTEGER val2) = mkTIME $ (toInteger val1) * (toInteger val2)
   mult_V_R (TIME val1) (REAL val2) = mkTIME $ floor $ (fromIntegral val1) *  val2
   mult_I_V int time = mult_V_I time int
   mult_R_V real time = mult_V_R time real
instance VHDL_Div_Phys TIME where
   div_V_I (TIME val1) (INTEGER val2) = mkTIME $ (toInteger val1) / (toInteger val2)
   div_V_R (TIME val1) (REAL val2) = mkTIME $ floor $ (fromIntegral val1) / val2
   div_V_V (TIME val1) (TIME val2) = val1 / val2
instance VHDL_Abs TIME where
   abs_V (TIME val) = mkTIME $ toInteger $ abs val
instance VHDL_Discrete TIME where
   discretePos (TIME val) = val
   discreteVal val = mkTIME val
   discreteSucc val = mkTIME $ (toInteger val) + 1
   discretePred val = mkTIME $ (toInteger val) - 1
   discreteLeftOf = discretePred
   discreteRightOf = discreteSucc

now :: SimStack TIME
now = gets $ timeVal

newtype NATURAL = NATURAL INTEGER
instance VHDL_Scalar INTEGER where
   scalarLeft = INTEGER 0
   scalarRight = INTEGER $ maxBound
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Eq NATURAL where
   eq_V (NATURAL val1) (NATURAL val2) =
      meta'boolToBoolean $ val1 == val2
   neq_V (NATURAL val1) (NATURAL val2) =
      meta'boolToBoolean $ val1 /= val2
instance VHDL_Ord NATURAL where
   lessThan_V (NATURAL val1) (NATURAL val2) =
      meta'boolToBoolean $ val1 < val2
   lessThanOrEqual_V (NATURAL val1) (NATURAL val2) =
      meta'boolToBoolean $ val1 <= val2
   greaterThan_V (NATURAL val1) (NATURAL val2) =
      meta'boolToBoolean $ val1 > val2
   greaterThanOrEqual_V (NATURAL val1) (NATURAL val2) =
      meta'boolToBoolean $ val1 >= val2
mkNATURAL :: INTEGER -> NATURAL
mkNATURAL val =
   let maxVal = case scalarHigh of (NATURAL v1) -> v1
       minVal = case scalarLow of (NATURAL v1) -> v1
   in if (greaterThan_V val maxVal) == BOOLEAN'IDEN'TRUE || (lessThan_V val minVal) == BOOLEAN'IDEN'TRUE
         then error "Out of bounds NATURAL " ++ show val
         else NATURAL val
instance VHDL_Add NATURAL where
   add_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ add_V val1 val2
instance VHDL_Sub NATURAL where
   sub_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ sub_V val1 val2
instance VHDL_Mult NATURAL where
   mult_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ mult_V val1 val2
instance VHDL_Div NATURAL where
   div_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ div_V val1 val2
instance VHDL_Mod NATURAL where
   mod_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ mod_V val1 val2
instance VHDL_Rem NATURAL where
   rem_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ rem_V val1 val2
instance VHDL_Abs NATURAL where
   abs_V (NATURAL val) = mkNATURAL $ abs_V val
instance VHDL_Exp NATURAL where
   exp_V (NATURAL val1) (NATURAL val2) = mkNATURAL $ exp_V val1 val2
instance VHDL_Discrete BOOLEAN where
   discretePos (NATURAL val) = val
   discreteVal val = mkNATURAL $ mkINTEGER val
   discreteSucc val = mkNATURAL $ add_V val (INTEGER 1)
   discretePred val = mkNATURAL $ sub_V val (INTEGER 1)
   discreteLeftOf = discretePred
   discreteRightOf = discreteSucc

newtype POSITIVE = POSITIVE INTEGER
instance VHDL_Scalar INTEGER where
   scalarLeft = INTEGER 1
   scalarRight = INTEGER $ maxBound
   scalarHigh = scalarRight
   scalarLow = scalarLeft
instance VHDL_Eq POSITIVE where
   eq_V (POSITIVE val1) (POSITIVE val2) =
      meta'boolToBoolean $ val1 == val2
   neq_V (POSITIVE val1) (POSITIVE val2) =
      meta'boolToBoolean $ val1 /= val2
instance VHDL_Ord POSITIVE where
   lessThan_V (POSITIVE val1) (POSITIVE val2) =
      meta'boolToBoolean $ val1 < val2
   lessThanOrEqual_V (POSITIVE val1) (POSITIVE val2) =
      meta'boolToBoolean $ val1 <= val2
   greaterThan_V (POSITIVE val1) (POSITIVE val2) =
      meta'boolToBoolean $ val1 > val2
   greaterThanOrEqual_V (POSITIVE val1) (POSITIVE val2) =
      meta'boolToBoolean $ val1 >= val2
mkPOSITIVE :: INTEGER -> POSITIVE
mkPOSITIVE val =
   let maxVal = case scalarHigh of (POSITIVE v1) -> v1
       minVal = case scalarLow of (POSITIVE v1) -> v1
   in if (greaterThan_V val maxVal) == BOOLEAN'IDEN'TRUE || (lessThan_V val minVal) == BOOLEAN'IDEN'TRUE
         then error "Out of bounds POSITIVE " ++ show val
         else POSITIVE val
instance VHDL_Add POSITIVE where
   add_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ add_V val1 val2
instance VHDL_Sub POSITIVE where
   sub_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ sub_V val1 val2
instance VHDL_Mult POSITIVE where
   mult_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ mult_V val1 val2
instance VHDL_Div POSITIVE where
   div_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ div_V val1 val2
instance VHDL_Mod POSITIVE where
   mod_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ mod_V val1 val2
instance VHDL_Rem POSITIVE where
   rem_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ rem_V val1 val2
instance VHDL_Abs POSITIVE where
   abs_V (POSITIVE val) = mkPOSITIVE $ abs_V val
instance VHDL_Exp POSITIVE where
   exp_V (POSITIVE val1) (POSITIVE val2) = mkPOSITIVE $ exp_V val1 val2
instance VHDL_Discrete BOOLEAN where
   discretePos (POSITIVE val) = val
   discreteVal val = mkPOSITIVE $ mkINTEGER val
   discreteSucc val = mkPOSITIVE $ add_V val (INTEGER 1)
   discretePred val = mkPOSITIVE $ sub_V val (INTEGER 1)
   discreteLeftOf = discretePred
   discreteRightOf = discreteSucc

newtype STRING = STRING (META'IntMap.IntMap CHARACTER)
class ARRAY'STRING a where
   arrayLeft'1'STRING :: POSITIVE
   arrayRight'1'STRING :: POSITIVE
   arrayHigh'1'STRING :: POSITIVE
   arrayLow'1'STRING :: POSITIVE
   arrayRange'1'STRING :: (POSITIVE,Direction,POSITIVE)
   arrayReverseRange'1'STRING :: (POSITIVE,Direction,POSITIVE)
   arrayLength'1'STRING :: Int64
   concat_V'STRING :: STRING -> STRING -> STRING
   concat_V_E'STRING :: STRING -> CHARACTER -> STRING
   concat_E_V'STRING :: CHARACTER -> STRING -> STRING
   concat_E'STRING :: CHARACTER -> CHARACTER -> STRING
instance VHDL_Eq STRING where
   eq_V (STRING map1) (STRING map2) =
      all (\(a,b) -> eq_V a b) $ zip (META'IntMap.elems map1) (META'IntMap.elems map2)
   neq_V (STRING map1) (STRING map2) =
      any (\(a,b) -> neq_V a b) $ zip (META'IntMap.elems map1) (META'IntMap.elems map2)
instance VHDL_Ord STRING where
   lessThan_V (STRING map1) (STRING map2) =
      meta'lessThanArray (META'IntMap.elems map1) (META'IntMap.elems map2)
   lessThanOrEqual_V val1 val2 =
      (eq_V val1 val2) `or_V` (lessThan_V val1 val2)
   greaterThan_V (STRING map1) (STRING map2) =
      meta'greaterThanArray (META'IntMap.elems map1) (META'IntMap.elems map2)
   greaterThanOrEqual_V val1 val2 =
      (eq_V val1 val2) `or_V` (greaterThan_V val1 val2)

newtype BIT_VECTOR = BIT_VECTOR (META'IntMap.IntMap BIT)
class ARRAY'BIT_VECTOR a where
   arrayLeft'1'BIT_VECTOR :: NATURAL
   arrayRight'1'BIT_VECTOR :: NATURAL
   arrayHigh'1'BIT_VECTOR :: NATURAL
   arrayLow'1'BIT_VECTOR :: NATURAL
   arrayRange'1'BIT_VECTOR :: (NATURAL,Direction,NATURAL)
   arrayReverseRange'1'BIT_VECTOR :: (NATURAL,Direction,NATURAL)
   arrayLength'1'BIT_VECTOR :: Int64
   concat_V'BIT_VECTOR :: BIT_VECTOR -> BIT_VECTOR -> BIT_VECTOR
   concat_V_E'BIT_VECTOR :: BIT_VECTOR -> BIT -> BIT_VECTOR
   concat_E_V'BIT_VECTOR :: BIT -> BIT_VECTOR -> BIT_VECTOR
   concat_E'BIT_VECTOR :: BIT -> BIT -> BIT_VECTOR
instance VHDL_Eq BIT_VECTOR where
   eq_V (BIT_VECTOR map1) (BIT_VECTOR map2) =
      all (\(a,b) -> eq_V a b) $ zip (META'IntMap.elems map1) (META'IntMap.elems map2)
   neq_V (BIT_VECTOR map1) (BIT_VECTOR map2) =
      any (\(a,b) -> neq_V a b) $ zip (META'IntMap.elems map1) (META'IntMap.elems map2)
instance VHDL_Ord BIT_VECTOR where
   lessThan_V (BIT_VECTOR map1) (BIT_VECTOR map2) =
      meta'lessThanArray (META'IntMap.elems map1) (META'IntMap.elems map2)
   lessThanOrEqual_V val1 val2 =
      (eq_V val1 val2) `or_V` (lessThan_V val1 val2)
   greaterThan_V (BIT_VECTOR map1) (BIT_VECTOR map2) =
      meta'greaterThanArray (META'IntMap.elems map1) (META'IntMap.elems map2)
   greaterThanOrEqual_V val1 val2 =
      (eq_V val1 val2) `or_V` (greaterThan_V val1 val2)
instance VHDL_And BIT_VECTOR where
   and_V val1 val2 = map (\(a,b) -> and_V a b) $ zip val1 val2
instance VHDL_Or BIT_VECTOR where
   or_V val1 val2 = map (\(a,b) -> or_V a b) $ zip val1 val2
instance VHDL_Not BIT_VECTOR where
   not_V val = map not_V val
instance VHDL_Xor BIT_VECTOR where
   xor_V val1 val2 = map (\(a,b) -> xor_V a b) $ zip val1 val2
instance VHDL_Nand BIT_VECTOR where
   nand_V val1 val2 = map (\(a,b) -> nand_V a b) $ zip val1 val2
instance VHDL_Nor BIT_VECTOR where
   nor_V val1 val2 = map (\(a,b) -> nor_V a b) $ zip val1 val2
