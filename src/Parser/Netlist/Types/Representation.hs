{-|
   Module      : Netlister.Types.Representation
   Description : Representation of VHDL types/functions

   Haskell types for containing and representing VHDL builtin and user types, functions, other low level elements
-}
module Parser.Netlist.Types.Representation
   ( Type(..)
   , Enumerate(..)
   , IntegerRange(..)
   , FloatRange(..)
   , RangeDirection(..)
   , Subtype(..)
   , ArrayBounds(..)
   , FunctionBody(..)
   , Constraint(..)
   , IndexConstraint
   , DiscreteRange(..)
   , Function(..)
   , Designator(..)
   , Calculation(..)
   ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as MapS

import Parser.Netlist.Types.Operators (Operator)

-- ?? Type attribute data
-- Name, Attributes
--data Type =
--   BaseType String BaseTypeAttributes
--   | Subtype String BaseTypeAttributes SubtypeAttributes
--   | ArrayType String BaseTypeAttributes ArrayTypeAttributes
--
--type TypeAttributes = MapS.Map Attributes 
--
--data BaseAttributes = Base

-- |Type representation
-- What is type, if enumerate what are valid values, how to map to actual values
data Type =
   -- |Enumeration type
   -- Contains ordered enumerates ?? Ensure order is parsed correctly, may need to reverse
   EnumerationType [Enumerate]
   -- |Integer type
   -- Covers some integer range
   -- Must be within implementation bounds
   | IntegerType IntegerRange
   -- |Floating type
   -- Covers some floating point range
   -- Must be within implementation bounds
   | FloatingType FloatRange
   -- |Physical type
   -- Integer range of values
   -- Name of base unit
   -- Secondary unit declarations
   | PhysicalType IntegerRange String (MapS.Map String Int64)
   -- |Array type
   -- Array of another type
   -- ?? Does this need entire subtype indication, not sure if need function and/or constraint
   | ArrayType ArrayBounds Subtype--Indication
   deriving (Eq,Ord)

-- |Enumerate
-- Can be identifier or character
data Enumerate =
   -- |Enumerate Identifier
   Enum_Identifier String
   -- |Enumerate Character
   | Enum_Char Char
   deriving (Eq,Ord)

-- |Integer range
-- Left direction right
data IntegerRange = IntegerRange Int64 Int64 RangeDirection
                  deriving (Eq,Ord)

-- |Floating point range
-- Left direction right
data FloatRange = FloatRange Double Double RangeDirection
                deriving (Eq,Ord)

-- |Range direction
data RangeDirection =
   -- |To
   -- Left < Right
   -- If Left > Right: null range
   To
   -- |Downto
   -- Left > Right
   -- If Left < Right: null range
   | Downto
   deriving (Eq,Ord)

-- |Subtype
-- Type built on another type
-- Essentially a type link
-- Includes resolution function, type, constraint
-- ?? Link to Function name or function body
data Subtype = Subtype (Maybe FunctionBody) Type (Maybe Constraint)
               deriving (Eq,Ord)

-- |Array bounds
-- Range/dimensions of the array type specified
data ArrayBounds =
   -- |Constrained
   -- Predefined array dimensions
   Constrained IndexConstraint
   -- |Unconstrained
   -- Array dimension types defined, but not actual array size
   | Unconstrained [Type]
   deriving (Eq,Ord)

-- |Function body
data FunctionBody = FunctionBody -- ?? [FunctionDeclarative] [FunctionStatement]
                  deriving (Eq,Ord)

-- |Constraint of subtype
data Constraint =
   -- |Integer range constraint
   Constraint_IntegerRange IntegerRange
   -- |Floating point range constraint
   | Constraint_FloatingRange FloatRange
   -- |Index constraint
   | Constraint_Index IndexConstraint
   deriving (Eq,Ord)

-- |List of discrete ranges
-- Ranges of each array dimension
type IndexConstraint = [DiscreteRange]

-- |Discrete range
-- ?? Can floating point be discrete, should make custom subtype indication for discrete
data DiscreteRange = Discrete_SubtypeIndication Subtype--Indication
                   | Discrete_IntegerRange IntegerRange
                   | Discrete_FloatingRange FloatRange
                   deriving (Eq,Ord)

-- |Function header information
-- Designator (name/operator reference), interface (inputs), return type
data Function = Function Designator [FunctionInterface] Type
              deriving (Eq,Ord)

-- |Function designators
-- Can be function name (identifier) or operator
data Designator =
   Designator_Identifier String
   | Designator_Operator Operator
   deriving (Eq,Ord)

-- |Interface to function
-- ?? Need to define
data FunctionInterface = FunctionInterface FunctionInterfaceType String Subtype--Indication
               deriving (Eq,Ord)

data FunctionInterfaceType =
   FunctionInterfaceType_Constant
   | FunctionInterfaceType_Signal --SignalBus
   deriving (Eq,Ord)

data Calculation = Calculation
