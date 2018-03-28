{-|
   Module      : Netlister.Builtin.Standard
   Description : Implementation of bultin \\'standard\\' package
-}
module Netlister.Builtin.Standard 
   ( standardPackage
   ) where

import Netlister.TypeData
         ( TypeStore
         )

import qualified Data.Map.Strict as MapS

types :: TypeStore
types =
   [  ( "BOOLEAN"
      , EnumerationType
         [ Enum_Identifier "FALSE"
         , Enum_Identifier "TRUE"
         ]
      )
   ,  ( "BIT"
      , EnumerationType
         [ Enum_Char '0'
         , Enum_Char '1'
         ]
      )
   ,  ( "CHARACTER",
      , EnumerationType -- ?? Missing valid characters
         [ Enum_Char ' '
         , Enum_Char '!'
         , Enum_Char '"'
         , Enum_Char '#'
         , Enum_Char '$'
         , Enum_Char '%'
         , Enum_Char '&'
         , Enum_Char '\''
         , Enum_Char '('
         , Enum_Char ')'
         , Enum_Char '*'
         , Enum_Char '+'
         , Enum_Char ','
         , Enum_Char '-'
         , Enum_Char '.'
         , Enum_Char '/'
         , Enum_Char '0'
         , Enum_Char '1'
         , Enum_Char '2'
         , Enum_Char '3'
         , Enum_Char '4'
         , Enum_Char '5'
         , Enum_Char '6'
         , Enum_Char '7'
         , Enum_Char '8'
         , Enum_Char '9'
         , Enum_Char ':'
         , Enum_Char ';'
         , Enum_Char '<'
         , Enum_Char '='
         , Enum_Char '>'
         , Enum_Char '?'
         , Enum_Char '@'
         , Enum_Char 'A'
         , Enum_Char 'B'
         , Enum_Char 'C'
         , Enum_Char 'D'
         , Enum_Char 'E'
         , Enum_Char 'F'
         , Enum_Char 'G'
         , Enum_Char 'H'
         , Enum_Char 'I'
         , Enum_Char 'J'
         , Enum_Char 'K'
         , Enum_Char 'L'
         , Enum_Char 'M'
         , Enum_Char 'N'
         , Enum_Char 'O'
         , Enum_Char 'P'
         , Enum_Char 'Q'
         , Enum_Char 'R'
         , Enum_Char 'S'
         , Enum_Char 'T'
         , Enum_Char 'U'
         , Enum_Char 'V'
         , Enum_Char 'W'
         , Enum_Char 'X'
         , Enum_Char 'Y'
         , Enum_Char 'Z'
         , Enum_Char '['
         , Enum_Char '\\'
         , Enum_Char ']'
         , Enum_Char '^'
         , Enum_Char '_'
         , Enum_Char 'a'
         , Enum_Char 'b'
         , Enum_Char 'c'
         , Enum_Char 'd'
         , Enum_Char 'e'
         , Enum_Char 'f'
         , Enum_Char 'g'
         , Enum_Char 'h'
         , Enum_Char 'i'
         , Enum_Char 'j'
         , Enum_Char 'k'
         , Enum_Char 'l'
         , Enum_Char 'm'
         , Enum_Char 'n'
         , Enum_Char 'o'
         , Enum_Char 'p'
         , Enum_Char 'q'
         , Enum_Char 'r'
         , Enum_Char 's'
         , Enum_Char 't'
         , Enum_Char 'u'
         , Enum_Char 'v'
         , Enum_Char 'w'
         , Enum_Char 'x'
         , Enum_Char 'y'
         , Enum_Char 'z'
         , Enum_Char '{'
         , Enum_Char '|'
         , Enum_Char '}'
         , Enum_Char '~'
         ]
      )
   ,  ( "SEVERITY_LEVEL"
      , EnumerationType
         [ Enum_Identifier "NOTE"
         , Enum_Identifier "WARNING"
         , Enum_Identifier "ERROR"
         , Enum_Identifier "FAILURE"
         ]
      )
   ,  ( "INTEGER"
      , IntegerType
         IntegerRange 
            minBound
            maxBound
            To
      )
   ,  ( "REAL"
      , FloatingType
         FloatRange
            minBound
            maxBound
            To
      )
   ,  ( "TIME"
      , PhysicalType
         IntegerRange
            minBound
            maxBound
            To
         "fs"
         [ ("ps",1000)
         , ("ns",10^6)
         , ("us",10^9)
         , ("ms",10^12)
         , ("sec",10^15)
         , ("min",60*10^15)
         , ("hour",(60^2)*(10^15))
         ] & MapS.fromList
      )
   ]

functions :: FunctionStore
functions =

-- -- function that returns the current simulation time:
-- function NOW return TIME;
-- -- predefined numeric subtypes:
-- subtype NATURAL is INTEGER range 0 to INTEGERHIGH;
-- subtype POSITIVE is INTEGER range 1 to INTEGERHIGH;
-- -- predefined array types:
-- type STRING is array (POSITIVE range o)
-- of CHARACTER;
-- type BIT-VECTOR is array (NATURAL range o)
-- of BIT;
-- end STANDARD;
