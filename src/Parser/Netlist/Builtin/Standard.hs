{-|
   Module      : Parser.Netlist.Builtin.Standard
   Description : Implementation of bultin \\'standard\\' package
-}
module Parser.Netlist.Builtin.Standard 
   ( standardPackage
   ) where

import Parser.Netlist.Types.Representation
         ( Type(..)
         , IntegerRange(..)
         , FloatRange(..)
         , Enumerate(..)
         , RangeDirection(..)
         , Subtype(..)
         , Function(..)
         , Designator(..)
         , NetlistName(..)
         )
import Parser.Netlist.Types.Stores
         ( TypeStore
         , SubtypeStore
         , FunctionStore
         , Package(..)
         , emptyPackage
         , ScopeStore(..)
         , emptyScopeStore
         )
import Lexer.Types.Error (getFloatBound)

import qualified Data.Map.Strict as MapS
import Data.Function ((&))

thisPackage :: NetlistName
thisPackage = NetlistName "STD" "STANDARD"

types :: TypeStore
types =
   [  ( "ANON'BOOLEAN"
      , EnumerationType
         [ Enum_Identifier "FALSE"
         , Enum_Identifier "TRUE"
         ]
      )
   ,  ( "ANON'BIT"
      , EnumerationType
         [ Enum_Char '0'
         , Enum_Char '1'
         ]
      )
   ,  ( "ANON'CHARACTER"
      , EnumerationType
         [ Enum_Identifier "NUL"
         , Enum_Identifier "SOH"
         , Enum_Identifier "STX"
         , Enum_Identifier "ETX"
         , Enum_Identifier "EOT"
         , Enum_Identifier "ENQ"
         , Enum_Identifier "ACK"
         , Enum_Identifier "BEL"
         , Enum_Identifier "BS"
         , Enum_Identifier "HT"
         , Enum_Identifier "LF"
         , Enum_Identifier "VT"
         , Enum_Identifier "FF"
         , Enum_Identifier "CR"
         , Enum_Identifier "SO"
         , Enum_Identifier "SI"
         , Enum_Identifier "DLE"
         , Enum_Identifier "DC1"
         , Enum_Identifier "DC2"
         , Enum_Identifier "DC3"
         , Enum_Identifier "DC4"
         , Enum_Identifier "NAK"
         , Enum_Identifier "SYN"
         , Enum_Identifier "ETB"
         , Enum_Identifier "CAN"
         , Enum_Identifier "EM"
         , Enum_Identifier "SUB"
         , Enum_Identifier "ESC"
         , Enum_Identifier "FSP"
         , Enum_Identifier "GSP"
         , Enum_Identifier "RSP"
         , Enum_Identifier "USP"
         , Enum_Char ' '
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
         , Enum_Identifier "DEL"
         ]
      )
   ,  ( "ANON'SEVERITY_LEVEL"
      , EnumerationType
         [ Enum_Identifier "NOTE"
         , Enum_Identifier "WARNING"
         , Enum_Identifier "ERROR"
         , Enum_Identifier "FAILURE"
         ]
      )
   ,  ( "ANON'INTEGER"
      , IntegerType
      )
   ,  ( "ANON'REAL"
      , FloatingType
      )
   ,  ( "ANON'TIME"
      , PhysicalType
         "FS"
         (
            [ ("PS",1000)
            , ("NS",10^6)
            , ("US",10^9)
            , ("MS",10^12)
            , ("SEC",10^15)
            , ("MIN",60*10^15)
            , ("HOUR",(60^2)*(10^15))
            ]
            & MapS.fromList
         )
      )
   ,  ( "ANON'STRING"
      , ArrayType
         [(thisPackage,"POSITIVE",subtypes MapS.! "POSITIVE")]
         (thisPackage,"CHARACTER")
         (subtypes MapS.! "CHARACTER")
      )
   ,  ( "ANON'BIT_VECTOR"
      , ArrayType
         [(thisPackage,"NATURAL",subtypes MapS.! "NATURAL")]
         (thisPackage,"BIT")
         (subtypes MapS.! "BIT")
      )
   ]
   & MapS.fromList

subtypes :: SubtypeStore
subtypes =
   [  ( "BOOLEAN"
      , (types MapS.! "ANON'BOOLEAN") &
         (\(EnumerationType enums) ->
            EnumerationSubtype
               Nothing
               (thisPackage,"ANON'BOOLEAN")
               enums
               (head enums,last enums)
         )
      )
   ,  ( "BIT"
      , (types MapS.! "ANON'BIT") &
         (\(EnumerationType enums) ->
            EnumerationSubtype
               Nothing
               (thisPackage,"ANON'BIT")
               enums
               (head enums,last enums)
         )
      )
   ,  ( "CHARACTER"
      , (types MapS.! "ANON'CHARACTER") &
         (\(EnumerationType enums) ->
            EnumerationSubtype
               Nothing
               (thisPackage,"ANON'CHARACTER")
               enums
               (head enums,last enums)
         )
      )
   ,  ( "SEVERITY_LEVEL"
      , (types MapS.! "ANON'SEVERITY_LEVEL") &
         (\(EnumerationType enums) ->
            EnumerationSubtype
               Nothing
               (thisPackage,"ANON'SEVERITY_LEVEL")
               enums
               (head enums,last enums)
         )
      )
   ,  ( "INTEGER"
      , IntegerSubtype
         Nothing
         (thisPackage,"ANON'INTEGER")
         ( IntegerRange
            minBound
            maxBound
            To
         )
      )
   ,  ( "REAL"
      , FloatingSubtype
         Nothing
         (thisPackage,"ANON'REAL")
         ( FloatRange
            (-floatBound)
            floatBound
            To
         )
      )
   ,  ( "TIME"
      , PhysicalSubtype
         Nothing
         (thisPackage,"ANON'TIME")
         "FS"
         (
            [ ("PS",1000)
            , ("NS",10^6)
            , ("US",10^9)
            , ("MS",10^12)
            , ("SEC",10^15)
            , ("MIN",60*10^15)
            , ("HOUR",(60^2)*(10^15))
            ]
            & MapS.fromList
         )
         ( IntegerRange
            minBound
            maxBound
            To
         )
      )
   ,  ( "NATURAL"
      , IntegerSubtype
         Nothing
         (thisPackage,"ANON'INTEGER")
         ( IntegerRange
            0
            maxBound
            To
         )
      )
   ,  ( "POSITIVE"
      , IntegerSubtype
         Nothing
         (thisPackage,"ANON'INTEGER")
         ( IntegerRange
            1
            maxBound
            To
         )
      )
   ,  ( "STRING"
      , ArraySubtype
         Nothing
         (thisPackage,"ANON'STRING")
         [(thisPackage,"POSITIVE",subtypes MapS.! "POSITIVE")]
         (thisPackage,"CHARACTER")
         (subtypes MapS.! "CHARACTER")
      )
   ,  ( "BIT_VECTOR"
      , ArraySubtype
         Nothing
         (thisPackage,"ANON'BIT_VECTOR")
         [(thisPackage,"NATURAL",subtypes MapS.! "NATURAL")]
         (thisPackage,"BIT")
         (subtypes MapS.! "BIT")
      )
   ]
   & MapS.fromList
   where floatBound = getFloatBound (0.0 :: Double)

functions :: FunctionStore
functions =
   [  ( Function
         (Designator_Identifier "NOW")
         []
         (thisPackage,"TIME")
         (subtypes MapS.! "TIME")
      , Nothing
      ) -- ?? MUST BE DEALT WITH IN INTERMEDIARY CONVERSION
   ]
   & MapS.fromList

standardPackage :: Package
standardPackage =
   emptyPackage
      { packageFunctions = functions
      , packageTypes = types
      , packageSubtypes = subtypes
      }
