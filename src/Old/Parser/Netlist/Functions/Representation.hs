module Parser.Netlist.Functions.Representation
   ( enum_scalarLeft
   , enum_scalarRight
   , enum_scalarHigh
   , enum_scalarLow
   , enum_discretePos
   , enum_discreteVal
   , enum_discreteSucc
   , enum_discretePred
   , enum_discreteLeftOf
   , enum_discreteRightOf
   , int_scalarLeft
   , int_scalarRight
   , int_scalarHigh
   , int_scalarLow
   , int_discreteSucc
   , int_discretePred
   , int_discreteLeftOf
   , int_discreteRightOf
   , float_scalarLeft
   , float_scalarRight
   , float_scalarHigh
   , float_scalarLow
   , findSignalsInCalculation
   ) where

import Data.Int (Int64)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

import Parser.Netlist.Types.Representation
         ( Enumerate
         , IntegerRange(..)
         , FloatRange(..)
         , RangeDirection(..)
         , Calculation(..)
         , SignalType
         )

enum_scalarLeft :: [Enumerate] -> Enumerate
enum_scalarLeft = head

enum_scalarRight :: [Enumerate] -> Enumerate
enum_scalarRight = last

enum_scalarHigh :: [Enumerate] -> Enumerate
enum_scalarHigh = last

enum_scalarLow :: [Enumerate] -> Enumerate
enum_scalarLow = head

enum_discretePos :: [Enumerate] -> Enumerate -> Int64
enum_discretePos enums enum = fromIntegral $ fromJust $ elemIndex enum enums -- ?? Use of fromjust

enum_discreteVal :: [Enumerate] -> Int64 -> Enumerate
enum_discreteVal enums val = enums !! (fromIntegral val)

enum_discreteSucc :: [Enumerate] -> Enumerate -> Enumerate
enum_discreteSucc enums =
   let getPos = enum_discretePos enums
       getVal = enum_discreteVal enums
   in getVal . (\pos -> pos + 1) . getPos

enum_discretePred :: [Enumerate] -> Enumerate -> Enumerate
enum_discretePred enums =
   let getPos = enum_discretePos enums
       getVal = enum_discreteVal enums
   in getVal . (\pos -> pos - 1) . getPos

enum_discreteLeftOf :: [Enumerate] -> Enumerate -> Enumerate
enum_discreteLeftOf = enum_discretePred

enum_discreteRightOf :: [Enumerate] -> Enumerate -> Enumerate
enum_discreteRightOf = enum_discreteSucc

int_scalarLeft :: IntegerRange -> Int64
int_scalarLeft (IntegerRange val _ _) = val

int_scalarRight :: IntegerRange -> Int64
int_scalarRight (IntegerRange _ val _) = val

int_scalarHigh :: IntegerRange -> Int64
int_scalarHigh (IntegerRange val _ Downto) = val
int_scalarHigh (IntegerRange _ val To) = val

int_scalarLow :: IntegerRange -> Int64
int_scalarLow (IntegerRange _ val Downto) = val
int_scalarLow (IntegerRange val _ To) = val

--int_discretePos :: IntegerRange -> Int64 -> Int64
--int_discreteVal :: IntegerRange -> Int64 -> Int64

int_discreteSucc :: IntegerRange -> Int64 -> Int64
int_discreteSucc _ = (+) 1
int_discretePred :: IntegerRange -> Int64 -> Int64
int_discretePred _ i = i - 1
int_discreteLeftOf :: IntegerRange -> Int64 -> Int64
int_discreteLeftOf range@(IntegerRange _ _ Downto) = int_discreteSucc range
int_discreteLeftOf range@(IntegerRange _ _ To) = int_discretePred range
int_discreteRightOf :: IntegerRange -> Int64 -> Int64
int_discreteRightOf range@(IntegerRange _ _ Downto) = int_discretePred range
int_discreteRightOf range@(IntegerRange _ _ To) = int_discreteSucc range

float_scalarLeft :: FloatRange -> Double
float_scalarLeft (FloatRange val _ _) = val

float_scalarRight :: FloatRange -> Double
float_scalarRight (FloatRange _ val _) = val

float_scalarHigh :: FloatRange -> Double
float_scalarHigh (FloatRange val _ Downto) = val
float_scalarHigh (FloatRange _ val To) = val

float_scalarLow :: FloatRange -> Double
float_scalarLow (FloatRange _ val Downto) = val
float_scalarLow (FloatRange val _ To) = val

findSignalsInCalculation :: Calculation -> [(SignalType,String)]
findSignalsInCalculation (Calc_Value _ _) = []
findSignalsInCalculation (Calc_FunctionCall _ calcs) = concat $ map findSignalsInCalculation calcs
findSignalsInCalculation (Calc_Const _) = []
findSignalsInCalculation (Calc_BuiltinNegate calc _) = findSignalsInCalculation calc
findSignalsInCalculation (Calc_BuiltinSum (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinSubtract (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinMult (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinDiv (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinMod calc1 calc2 _) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinRem calc1 calc2 _) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinExp (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinAbs calc _) = findSignalsInCalculation calc
findSignalsInCalculation (Calc_BuiltinNot calc _) = findSignalsInCalculation calc
findSignalsInCalculation (Calc_BuiltinEqual (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinNotEqual (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinLessThan (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinLessThanOrEqual (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinGreaterThan (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinGreaterThanOrEqual (calc1,_) (calc2,_)) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinAnd calc1 calc2 _) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinOr calc1 calc2 _) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinXor calc1 calc2 _) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinNand calc1 calc2 _) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_BuiltinNor calc1 calc2 _) = concat $ map findSignalsInCalculation [calc1,calc2]
findSignalsInCalculation (Calc_ImplicitTypeConversion _ calc) = findSignalsInCalculation calc
findSignalsInCalculation (Calc_SubtypeResult _ calc) = findSignalsInCalculation calc
findSignalsInCalculation (Calc_ExplicitTypeConversion _ (calc,_)) = findSignalsInCalculation calc
findSignalsInCalculation (Calc_Signal (Nothing,signalName) signalType) = [(signalType,signalName)]
findSignalsInCalculation (Calc_Signal (Just _,_) _) = []
findSignalsInCalculation (Calc_SignalEvent (Nothing,signalName) signalType) = [(signalType,signalName)]
findSignalsInCalculation (Calc_SignalEvent (Just _,_) _) = []
