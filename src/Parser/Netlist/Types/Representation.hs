{-|
   Module      : Netlister.Types.Representation
   Description : Representation of VHDL types/functions

   Haskell types for containing and representing VHDL builtin and user types, functions, other low level elements
-}
module Parser.Netlist.Types.Representation
   ( NetlistName(..)
   , Type(..)
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
   , Signal(..)
   , Constant(..)
   ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as MapS

import Parser.Netlist.Types.Operators (Operator)

-- |Netlist name representation
-- Made up of the library the unit is in and its name
data NetlistName = NetlistName { libraryName :: String, unitName :: String }
                 deriving (Eq,Ord)

instance (Show NetlistName) where
   show (NetlistName lib unit) = lib ++ "." ++ unit

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
   | ArrayType
      { array_bounds :: [Subtype]
      --{ array_bounds :: ArrayBounds
      , array_typeName :: String
      , array_typePackage :: NetlistName
      , array_typeData :: Subtype
      }
   deriving (Eq,Ord,Show)

-- |Enumerate
-- Can be identifier or character
data Enumerate =
   -- |Enumerate Identifier
   Enum_Identifier String
   -- |Enumerate Character
   | Enum_Char Char
   deriving (Eq,Ord)

instance (Show Enumerate) where
   show (Enum_Identifier str) =
      "enumerate identifier " ++ str
   show (Enum_Char chr) =
      "enumerate character " ++ ['\'',chr,'\'']

-- |Integer range
-- Left direction right
data IntegerRange = IntegerRange Int64 Int64 RangeDirection
                  deriving (Eq,Ord,Show)

-- |Floating point range
-- Left direction right
data FloatRange = FloatRange Double Double RangeDirection
                deriving (Eq,Ord,Show)

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
   deriving (Eq,Ord,Show)

-- |Subtype
-- Type built on another type
-- Essentially a type link
-- Includes resolution function, type, constraint
-- ?? Link to Function name or function body
data Subtype =
   Subtype
      { subtype_resolutionFunction :: Maybe FunctionBody
      , subtype_typeName :: String
      , subtype_typePackage :: NetlistName
      , subtype_typeData :: Type
      , subtype_Constraint :: Maybe Constraint
      }
      deriving (Eq,Ord,Show)

-- |Array bounds
-- Range/dimensions of the array type specified
data ArrayBounds =
   -- |Constrained
   -- Predefined array dimensions
   Constrained [(String,NetlistName,Subtype)]
   -- |Unconstrained
   -- Array dimension types defined, but not actual array size
   | Unconstrained [(String,NetlistName,Subtype)]
   deriving (Eq,Ord,Show)

-- |Function body
-- ?? [FunctionDeclarative]
data FunctionBody = FunctionBody [Statement]
                  deriving (Eq,Ord,Show)

-- |Constraint of subtype
data Constraint =
   -- |Integer range constraint
   Constraint_IntegerRange IntegerRange
   -- |Floating point range constraint
   | Constraint_FloatingRange FloatRange
   -- |Index constraint
   | Constraint_Index IndexConstraint
   deriving (Eq,Ord,Show)

-- |List of discrete ranges
-- Ranges of each array dimension
type IndexConstraint = [DiscreteRange]

-- |Discrete range
-- ?? Can floating point be discrete, should make custom subtype indication for discrete
data DiscreteRange = Discrete_SubtypeIndication Subtype--Indication
                   | Discrete_IntegerRange IntegerRange
                   | Discrete_FloatingRange FloatRange
                   deriving (Eq,Ord,Show)

-- |Function header information
-- Designator (name/operator reference), interface (inputs), return type
data Function = Function Designator [FunctionInterface] Type
              deriving (Eq,Ord,Show)

-- |Function designators
-- Can be function name (identifier) or operator
data Designator =
   Designator_Identifier String
   | Designator_Operator Operator
   deriving (Eq,Ord,Show)

-- |Interface to function
-- ?? Need to define
data FunctionInterface = FunctionInterface FunctionInterfaceType String Subtype--Indication
               deriving (Eq,Ord,Show)

data FunctionInterfaceType =
   FunctionInterfaceType_Constant
   | FunctionInterfaceType_Signal --SignalBus
   deriving (Eq,Ord,Show)

data Statement = Statement
   deriving (Eq,Ord,Show)

data Calculation =
   Calc_Value NetlistName Value
   | Calc_FunctionCall NetlistName --Function
   --Calc_SignalDelayed (String,Signal) Int64 -- save and carry out after time
   -- | Calc_SignalStable (String,Signal) Int64 -- check changes over timeframe (need to record last time changed)
   -- | Calc_SignalQuiet (String,Signal) Int64
   -- -- ?? Signal transaction
   -- | Calc_SignalEvent (String,Signal)
   -- | Calc_SignalActive (String,Signal)
   -- | Calc_SignalLastEvent (String,Signal)
   -- | Calc_SignalLastActive (String,Signal)
   -- | Calc_SignalLastValue (String,Signal) 
   deriving (Show)

data Value =
   Value_Enum String Enumerate
   | Value_Int Int64
   | Value_Float Float
   | Value_Physical Int64
   | Value_Array [Value]
   deriving (Eq,Ord,Show)

data ScalarAttributes =
   ScalarAttributes
      { scalarAttr_Left :: Value
      , scalarAttr_Right :: Value
      , scalarAttr_High :: Value
      , scalarAttr_Low :: Value
      }
   deriving (Eq,Ord)

data DiscreteAttributes =
   DiscreteAttributes
      { discAttr_Pos :: (Function,FunctionBody)
      , discAttr_Val :: (Function,FunctionBody)
      , discAttr_Succ :: (Function,FunctionBody)
      , discAttr_Pred :: (Function,FunctionBody)
      , discAttr_Leftof :: (Function,FunctionBody)
      , discAttr_Rightof :: (Function,FunctionBody)
      }

data ArrayAttributes =
   ArrayAttributes
      { arrayAttr_Left :: (Function,FunctionBody)
      , arrayAttr_Right :: (Function,FunctionBody)
      , arrayAttr_High :: (Function,FunctionBody)
      , arrayAttr_Low :: (Function,FunctionBody)
      , arrayAttr_Range :: IntegerRange
      , arrayAttr_ReverseRange :: IntegerRange
      , arrayAttr_Length :: Value
      }

--type BlockAttributeStore = MapS.Map BlockAttributes Calculation
--
--data BlockAttributes =
--   BlkAttr_Behaviour
--   | BlkAttr_Structure

-- |Constant representation
-- Type of constant, constant value
-- ?? Only can have not calculation ("deferred constant") in package (header) declaration, make separate type
data Constant = Constant Type (Maybe Value)
              deriving (Show)

-- |Signal representation
-- Resolution function body, type of signal
-- ?? Need signal kind - signal guard only acceptable if resolved signal
-- Default value if calculation is not included is Type'left
-- Type cannot be file or access
data Signal =
   Signal
      { signalType :: Type
      , signalDefault :: Calculation
      }
   deriving (Show)

-- |Variable representation
-- Type of variable, default value (Type'left is not included)
-- Type cannot be file
data Variable = Variable Type Calculation