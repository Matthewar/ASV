{-|
   Module      : Netlister.TypeData
   Description : Data on bultin \\'standard\\' package
-}
module Netlister.TypeData where

import qualified Data.Map.Strict as MapS
import Data.Int (Int64)
import Data.Char (toUpper)
import Data.Function ((&))

import Netlister.Types.Operators (Operator)

-- What is type, if enumerate what are valid values, how to map to actual values
data NetlistStore =
   NetlistStore
      --{ entities :: MapS.Map [String] Entity -- ?? ports :: [Port], generics
      --, configurations :: MapS.Map [String] Configuration
      { packages :: MapS.Map [String] Package
      --, architectures :: MapS.Map [String] Architecture
      }

-- |Type data
-- Name, Attributes
--data Type =
--   BaseType String BaseTypeAttributes
--   | Subtype String BaseTypeAttributes SubtypeAttributes
--   | ArrayType String BaseTypeAttributes ArrayTypeAttributes
--
--type TypeAttributes = MapS.Map Attributes 
--
--data BaseAttributes = Base

data Type =
   EnumerationType Enumerates
   | IntegerType IntegerRange
   | FloatingType FloatRange
   -- |Physical type
   -- Integer range of values
   -- Name of base unit
   -- Secondary unit declarations
   | PhysicalType IntegerRange String (MapS.Map String Int64)
   -- ?? Function or function body
   | Subtype SubtypeIndication
   | ArrayType ArrayBounds SubtypeIndication -- ?? Does this need entire subtype indication
   deriving (Eq,Ord)

-- |Essentially a type link
data SubtypeIndication = SubtypeIndication (Maybe FunctionBody) Type (Maybe Constraint)
                       deriving (Eq,Ord)

type Enumerates = [Enumerate]

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
data RangeDirection = To | Downto
                    deriving (Eq,Ord)

data Constraint =
   Constraint_IntegerRange IntegerRange
   | Constraint_FloatingRange FloatRange
   | Constraint_Index IndexConstraint
   deriving (Eq,Ord)

type IndexConstraint = [DiscreteRange]

data DiscreteRange = Discrete_SubtypeIndication SubtypeIndication
                   | Discrete_IntegerRange IntegerRange
                   | Discrete_FloatingRange FloatRange
                   deriving (Eq,Ord)

data ArrayBounds =
   Constrained IndexConstraint
   | Unconstrained [Type]
   deriving (Eq,Ord)

type TypeStore = MapS.Map String Type

data Function = Function Designator [Interface] Type
              deriving (Eq,Ord)

data Designator =
   Designator_Identifier String
   | Designator_Operator Operator
   deriving (Eq,Ord)

data Interface = Interface -- ??
               deriving (Eq,Ord)

data FunctionBody = FunctionBody -- ?? [FunctionDeclarative] [FunctionStatement]
                  deriving (Eq,Ord)

type FunctionStore = MapS.Map Function (Maybe FunctionBody)

-- [String] provides selected names
type PackageStore = MapS.Map [String] Package

data Package =
   Package
      -- |Subprogram declaration
      --SubprogramHeaderStore ?? Instead do separately
      --ProcedureStore
      FunctionStore
      TypeStore
      --ConstantStore
      --SignalStore
      --FileStore
      --AliasStore
      --ComponentStore
      --AttributeStore
      --DisconnectStore
      --UseStore

   -- ?? Maybe PackageBodyStore
      --Subprogram
      --Type
      --Constant
      --File
      --Alias
      --Use
