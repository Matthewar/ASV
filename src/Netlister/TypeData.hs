{-|
   Module      : Netlister.TypeData
   Description : Data on bultin \\'standard\\' package
-}
module Netlister.TypeData where

import qualified Data.Map.Strict as MapS

-- What is type, if enumerate what are valid values, how to map to actual values
data NetlistUnit =
   Entity --{-- ports :: [Port], generics
   | Configuration
   | Package PackageHeaderStore PackageBodyStore
   | Architecture

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
   | ArrayType ArrayBounds Type

type Enumerates = [Enumerate]

data Enumerate =
   -- |Enumerate Identifier
   Enum_Identifier String
   -- |Enumerate Character
   | Enum_Char Char

-- |Integer range
-- Left direction right
newtype IntegerRange = IntegerRange Int64 Int64 RangeDirection

-- |Floating point range
-- Left direction right
newtype FloatRange = FloatRange Double Double RangeDirection

-- |Range direction
data RangeDirection = To | Downto

data ArrayBounds =
   Constrained IntegerRange
   | Unconstrained String

type TypeStore = MapS.Map String Type

newtype Function = Function Designator (Maybe [Interface]) Type (Maybe FunctionBody)

newtype FunctionBody = FunctionBody [FunctionDeclarative] [FunctionStatement]

type FunctionStore = MapS.Map String Function

newtype PackageHeaderStore =
   PackageHeaderStore
      -- |Subprogram declaration
      SubprogramHeaderStore
      TypeStore
      ConstantStore
      SignalStore
      FileStore
      AliasStore
      ComponentStore
      AttributeStore
      DisconnectStore
      UseStore

      Subprogram
      Type
      Constant
      File
      Alias
      Use
