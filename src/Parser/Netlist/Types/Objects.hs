{-|
   Module      : Netlister.Types.Objects
   Description : Types for storing object declarations in netlists
-}
module Parser.Netlist.Types.Objects
         ( Constant(..)
         , Signal(..)
         , Variable(..)
         ) where

import Parser.Netlist.Types.Representation
         ( Type
         , Subtype
         , Calculation
         )

-- |Constant representation
-- Type of constant, constant value
-- ?? Only can have not calculation ("deferred constant") in package (header) declaration, make separate type
data Constant = Constant Type (Maybe Calculation)

-- |Signal representation
-- Resolution function body, type of signal
-- ?? Need signal kind - signal guard only acceptable if resolved signal
-- Default value if calculation is not included is Type'left
-- Type cannot be file or access
data Signal = Signal Subtype Calculation

-- |Variable representation
-- Type of variable, default value (Type'left is not included)
-- Type cannot be file
data Variable = Variable Type Calculation
