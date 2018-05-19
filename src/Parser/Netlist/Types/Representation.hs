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
   , FunctionBody(..)
   , Constraint(..)
   , IndexConstraint
   , DiscreteRange(..)
   , Function(..)
   , Designator(..)
   , FunctionInterface(..)
   , FunctionInterfaceType(..)
   , Calculation(..)
   , Value(..)
   , Generic(..)
   , Port(..)
   , Mode(..)
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
   | IntegerType
   -- |Floating type
   -- Covers some floating point range
   -- Must be within implementation bounds
   | FloatingType
   -- |Physical type
   -- Integer range of values
   -- Name of base unit
   -- Secondary unit declarations
   | PhysicalType String (MapS.Map String Int64)
   -- |Array type
   -- Array of another type
   -- ?? Does this need entire subtype indication, not sure if need function and/or constraint
   | ArrayType
      { array_bounds :: [(NetlistName,String,Subtype)]
      , array_elementTypeName :: (NetlistName,String)
      , array_elementTypeData :: Subtype
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
   EnumerationSubtype
      { enumSubtype_resolutionFunction :: Maybe (NetlistName,String)
      , enumSubtype_typeName :: (NetlistName,String)
      , enumSubtype_enums :: [Enumerate]
      , enumSubtype_constraint :: (Enumerate,Enumerate)
      }
   | IntegerSubtype
      { intSubtype_resolutionFunction :: Maybe (NetlistName,String)
      , intSubtype_typeName :: (NetlistName,String)
      , intSubtype_constraint :: IntegerRange
      }
   | FloatingSubtype
      { floatSubtype_resolutionFunction :: Maybe (NetlistName,String)
      , floatSubtype_typeName :: (NetlistName,String)
      , floatSubtype_constraint :: FloatRange
      }
   | PhysicalSubtype
      { physSubtype_resolutionFunction :: Maybe (NetlistName,String)
      , physSubtype_typeName :: (NetlistName,String)
      , physSubtype_baseUnit :: String
      , physSubtype_secondaryUnits :: MapS.Map String Int64
      , physSubtype_constraint :: IntegerRange
      }
   | ArraySubtype
      { arraySubtype_resolutionFunction :: Maybe (NetlistName,String)
      , arraySubtype_typeName :: (NetlistName,String)
      , arraySubtype_bounds :: [(NetlistName,String,Subtype)]
      -- , arraySubtype_size :: [ specified ranges ]
      , arraySubtype_elementTypeName :: (NetlistName,String)
      , arraySubtype_elementTypeData :: Subtype
      }
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
data Function =
   Function
      { function_name :: Designator
      , function_interface :: [FunctionInterface]
      , function_returnTypeName :: (NetlistName,String)
      , function_returnTypeData :: Subtype
      }
   deriving (Eq,Ord,Show)

-- |Function designators
-- Can be function name (identifier) or operator
data Designator =
   Designator_Identifier String
   | Designator_Operator Operator
   deriving (Eq,Ord,Show)

-- |Interface to function
-- ?? Need to define
data FunctionInterface =
   FunctionInterface
      { functionInterface_kind :: FunctionInterfaceType
      , functionInterface_name :: String
      , functionInterface_typeName :: (NetlistName,String)
      , functionInterface_typeData :: Subtype
      }
   deriving (Eq,Ord,Show)

data FunctionInterfaceType =
   FunctionInterfaceType_Constant
   | FunctionInterfaceType_Signal --SignalBus
   deriving (Eq,Ord,Show)

data Statement = Statement
   deriving (Eq,Ord,Show)

data Calculation =
   -- |Any static value that can be calculated by netlister
   Calc_Value Value
   -- |Function call
   | Calc_FunctionCall (NetlistName,Function) [Calculation]
   -- |Non-static (deferred) constant in expression
   | Calc_Const (NetlistName,String)
   -- |Builtin non-static negate call
   | Calc_BuiltinNegate Calculation
   | Calc_BuiltinSum Calculation Calculation
   | Calc_BuiltinSubtract Calculation Calculation
   | Calc_BuiltinConcat Calculation Calculation
   | Calc_BuiltinMult Calculation Calculation
   | Calc_BuiltinDiv Calculation Calculation
   | Calc_BuiltinMod Calculation Calculation
   | Calc_BuiltinRem Calculation Calculation
   | Calc_BuiltinExp Calculation Calculation
   | Calc_BuiltinAbs Calculation
   | Calc_BuiltinNot Calculation
   | Calc_BuiltinEqual Calculation Calculation
   | Calc_BuiltinNotEqual Calculation Calculation
   | Calc_BuiltinLessThan Calculation Calculation
   | Calc_BuiltinLessThanOrEqual Calculation Calculation
   | Calc_BuiltinGreaterThan Calculation Calculation
   | Calc_BuiltinGreaterThanOrEqual Calculation Calculation
   | Calc_BuiltinAnd Calculation Calculation
   | Calc_BuiltinOr Calculation Calculation
   | Calc_BuiltinXor Calculation Calculation
   | Calc_BuiltinNand Calculation Calculation
   | Calc_BuiltinNor Calculation Calculation
   --Calc_SignalDelayed (String,Signal) Int64 -- save and carry out after time
   -- | Calc_SignalStable (String,Signal) Int64 -- check changes over timeframe (need to record last time changed)
   -- | Calc_SignalQuiet (String,Signal) Int64
   -- -- ?? Signal transaction
   -- | Calc_SignalEvent (String,Signal)
   -- | Calc_SignalActive (String,Signal)
   -- | Calc_SignalLastEvent (String,Signal)
   -- | Calc_SignalLastActive (String,Signal)
   -- | Calc_SignalLastValue (String,Signal) 
   deriving (Eq,Show)

data Value =
   Value_Enum (NetlistName,String) Enumerate -- ?? Is the type name String needed?
   | Value_Int Integer
   | Value_Float Double
   | Value_Physical Integer
   | Value_Array [Value]
   deriving (Eq,Ord,Show)

-- |Generic type
data Generic =
   Generic
      { generic_name :: String
      , generic_subtypeName :: (NetlistName,String)
      , generic_subtypeData :: Subtype
      , generic_default :: Maybe Value
      }
   deriving (Show)

-- |Port type
data Port =
   Port
      { port_name :: String
      , port_mode :: Mode
      , port_subtypeName :: (NetlistName,String)
      , port_subtypeData :: Subtype
      -- ?? is bus
      , port_default :: Maybe Value
      }
   deriving (Show)

data Mode =
   Mode_In
   | Mode_Out
   | Mode_Inout
   | Mode_Buffer
   | Mode_Linkage
   deriving (Show)

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
data Constant =
   Constant
      { constant_typeName :: (NetlistName,String)
      , constant_typeData :: Subtype
      , constant_Value :: Maybe Value
      }
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
