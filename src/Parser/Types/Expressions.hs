module Parser.Types.Expressions
   ( Staticity(..)
   , Parser.Netlist.Types.Representation.AllTypes(..)
   ) where

import Parser.Netlist.Types.Representation
         ( NetlistName
         , Type
         , AllTypes(..)
         )

data Staticity =
   LocallyStatic
   | GloballyStatic
   | NotStatic

-- ?? Should be here once custom specific data type written for Calculation types
--data AllTypes =
--   Type_Type (NetlistName,String) Type
--   -- | Type_Subtype NetlistName Subtype
--   -- | Type_Range Type Direction
--   | Type_UniversalInt
--   | Type_UniversalReal
--   | Type_String String
--   | Type_BitString Int
--   deriving (Eq)

--data ExpectedType =
--   ExpectedType Type
--   -- | ExpectedRange Type Direction
--   | NotExpecting
