module Sim.Functions.ConvertNames
   ( convertEntityNames
   , convertArchitectureNames
   ) where

import qualified Data.Map.Strict as MapS

import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Function(..)
         , FunctionInterface(..)
         , FunctionBody(..)
         , Constant(..)
         , Signal(..)
         , Port(..)
         , Generic(..)
         , Subtype(..)
         , Calculation(..)
         , Value(..)
         , AllTypes(..)
         , SequentialStatement(..)
         , Waveform(..)
         )
import Parser.Netlist.Types.Stores
         ( Entity(..)
         , Architecture(..)
         , Process(..)
         , ProcessStore
         , SubtypeStore
         , FunctionStore
         , ConstantStore
         , SignalStore
         )

convertEntityNames :: Entity -> NetlistName -> NetlistName -> Entity
convertEntityNames (Entity scope generics ports funcs types subtypes consts signals processes) oldName newName =
   Entity
      scope
      (convertGenericNames generics oldName newName)
      (convertPortNames ports oldName newName)
      (convertFunctionNames funcs oldName newName)
      (convertTypeNames types oldName newName)
      (convertSubtypeNames subtypes oldName newName)
      (convertConstantNames consts oldName newName)
      (convertSignalNames signals oldName newName)
      (convertProcessNames processes oldName newName)

convertArchitectureNames :: Architecture -> NetlistName -> NetlistName -> Architecture
convertArchitectureNames (Architecture scope generics ports funcs types subtypes consts signals processes) oldName newName =
   Architecture
      scope
      (convertGenericNames generics oldName newName)
      (convertPortNames ports oldName newName)
      (convertFunctionNames funcs oldName newName)
      (convertTypeNames types oldName newName)
      (convertSubtypeNames subtypes oldName newName)
      (convertConstantNames consts oldName newName)
      (convertSignalNames signals oldName newName)
      (convertProcessNames processes oldName newName)

convertGenericNames :: [Generic] -> NetlistName -> NetlistName -> [Generic]
convertGenericNames generics oldName newName = map (\s -> convertGenericName s oldName newName) generics

convertGenericName :: Generic -> NetlistName -> NetlistName -> Generic
convertGenericName (Generic name (package,name2) subtype value) oldName newName =
   Generic
      name
      ( if package == oldName
         then let (NetlistName lib name3) = newName
              in NetlistName lib $ name3 ++ "'GENERICS"
         else package
      , name2
      )
      (convertSubtypeName subtype oldName newName)
      (case value of
         Just val -> Just $ convertValueName val oldName newName
         Nothing -> Nothing
      )

convertPortNames :: [Port] -> NetlistName -> NetlistName -> [Port]
convertPortNames ports oldName newName = map (\s -> convertPortName s oldName newName) ports

convertPortName :: Port -> NetlistName -> NetlistName -> Port
convertPortName (Port name mode (package,name2) subtype value) oldName newName =
   Port
      name
      mode
      ( if package == oldName
         then let (NetlistName lib name3) = newName
              in NetlistName lib $ name3 ++ "'GENERICS"
         else package
      , name2
      )
      (convertSubtypeName subtype oldName newName)
      (convertValueName value oldName newName)

convertFunctionNames :: FunctionStore -> NetlistName -> NetlistName -> FunctionStore
convertFunctionNames funcs oldName newName =
   MapS.mapKeys (\funcKey -> convertFunctionName funcKey oldName newName) $
   MapS.map convertFunctionBody funcs
   where convertFunctionBody body = body
   -- ?? Need to add statements
   --where convertFunctionBody (Just (FunctionBody statements)) = Just $ FunctionBody $ convertSequentialStatementNames statements oldName newName
   --      convertFunctionBody Nothing = Nothing

convertFunctionName :: Function -> NetlistName -> NetlistName -> Function
convertFunctionName (Function desig interface typeName subtype) oldName newName =
   Function
      desig
      (convertInterface interface)
      (convertName typeName oldName newName)
      (convertSubtypeName subtype oldName newName)
   where convertInterface = map convertInterfaceElement
         convertInterfaceElement (FunctionInterface kind name typeName subtype) =
            FunctionInterface
               kind
               name
               (convertName typeName oldName newName)
               (convertSubtypeName subtype oldName newName)

-- ?? Not dealt with arrays
convertTypeNames types _ _ = types
convertTypeName typeData _ _ = typeData

convertSubtypeNames :: SubtypeStore -> NetlistName -> NetlistName -> SubtypeStore
convertSubtypeNames subtypes oldName newName = MapS.map (\s -> convertSubtypeName s oldName newName) subtypes

convertSubtypeName :: Subtype -> NetlistName -> NetlistName -> Subtype
convertSubtypeName subtype oldName newName =
   case subtype of
      EnumerationSubtype resFunc typeName enums constraint -> EnumerationSubtype (convertResFunc resFunc oldName newName) (convertName typeName oldName newName) enums constraint
      IntegerSubtype resFunc typeName constraint -> IntegerSubtype (convertResFunc resFunc oldName newName) (convertName typeName oldName newName) constraint
      FloatingSubtype resFunc typeName constraint -> FloatingSubtype (convertResFunc resFunc oldName newName) (convertName typeName oldName newName) constraint
      PhysicalSubtype resFunc typeName baseUnit otherUnits constraint -> PhysicalSubtype (convertResFunc resFunc oldName newName) (convertName typeName oldName newName) baseUnit otherUnits constraint
      -- Array
   where convertResFunc (Just name) oldName newName = Just $ convertName name oldName newName
         convertResFunc Nothing _ _ = Nothing

convertName :: (NetlistName,String) -> NetlistName -> NetlistName -> (NetlistName,String)
convertName (package,name) oldName newName
   | package == oldName = (newName,name)
   | otherwise = (package,name)

convertConstantNames :: ConstantStore -> NetlistName -> NetlistName -> ConstantStore
convertConstantNames consts oldName newName = MapS.map (\const -> convertConstantName const oldName newName) consts

convertConstantName :: Constant -> NetlistName -> NetlistName -> Constant
convertConstantName (Constant typeName subtype value) oldName newName =
   let convertValue value = convertValueName value oldName newName
   in Constant
         (convertName typeName oldName newName)
         (convertSubtypeName subtype oldName newName)
         (pure convertValue <*> value)

convertSignalNames :: SignalStore -> NetlistName -> NetlistName -> SignalStore
convertSignalNames signals oldName newName = MapS.map (\signal -> convertSignalName signal oldName newName) signals

convertSignalName :: Signal -> NetlistName -> NetlistName -> Signal
convertSignalName (Signal typeName subtype value) oldName newName =
   Signal
      (convertName typeName oldName newName)
      (convertSubtypeName subtype oldName newName)
      (convertValueName value oldName newName)

convertProcessNames :: ProcessStore -> NetlistName -> NetlistName -> ProcessStore
convertProcessNames processes oldName newName = MapS.map (\proc -> convertProcessName proc oldName newName) processes

convertProcessName :: Process -> NetlistName -> NetlistName -> Process
convertProcessName (Process funcs types subtypes consts statements) oldName newName =
   Process
      (convertFunctionNames funcs oldName newName)
      (convertTypeNames types oldName newName)
      (convertSubtypeNames subtypes oldName newName)
      (convertConstantNames consts oldName newName)
      (convertSequentialStatementNames statements oldName newName)

convertSequentialStatementNames :: [SequentialStatement] -> NetlistName -> NetlistName -> [SequentialStatement]
convertSequentialStatementNames statements oldName newName = map (\s -> convertSequentialStatementName s oldName newName) statements

convertSequentialStatementName :: SequentialStatement -> NetlistName -> NetlistName -> SequentialStatement
convertSequentialStatementName (WaitStatement sensitivities condition wait) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in WaitStatement
         sensitivities
         (convertCalc condition)
         (pure convertCalc <*> wait)
convertSequentialStatementName (AssertStatement calc1 calc2 calc3) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in AssertStatement
         (convertCalc calc1)
         (convertCalc calc2)
         (convertCalc calc3)
convertSequentialStatementName (SignalAssignStatement sigName sigType waveforms) oldName newName =
   SignalAssignStatement
      sigName
      sigType
      (map convertWaveform waveforms)
   where convertWaveform (ValueWaveform calc1 calc2) = ValueWaveform (convertCalc calc1) (convertCalc calc2)
         convertWaveform (NullWaveform calc) = NullWaveform $ convertCalc calc
         convertCalc calc = convertCalculationNames calc oldName newName
convertSequentialStatementName NullStatement _ _ = NullStatement

convertCalculationNames :: Calculation -> NetlistName -> NetlistName -> Calculation
convertCalculationNames (Calc_Value value alltype) oldName newName =
   Calc_Value
      (convertValueName value oldName newName)
      (convertAllTypesNames alltype oldName newName)
convertCalculationNames (Calc_FunctionCall (package,function) calcs) oldName newName =
   Calc_FunctionCall
      ( if package == oldName
         then newName
         else package
      , convertFunctionName function oldName newName
      )
      (map (\calc -> convertCalculationNames calc oldName newName) calcs)
convertCalculationNames (Calc_Const constName) oldName newName = Calc_Const $ convertName constName oldName newName
convertCalculationNames (Calc_BuiltinNegate calc alltypes) oldName newName =
   Calc_BuiltinNegate (convertCalculationNames calc oldName newName) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinSum pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinSum (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinSubtract pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinSubtract (convertCalcPair pair1) (convertCalcPair pair2)
-- Calc_BuiltinConcat Calculation Calculation AllTypes
convertCalculationNames (Calc_BuiltinMult pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinMult (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinDiv pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinDiv (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinMod calc1 calc2 alltypes) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in Calc_BuiltinMod (convertCalc calc1) (convertCalc calc2) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinRem calc1 calc2 alltypes) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in Calc_BuiltinRem (convertCalc calc1) (convertCalc calc2) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinExp pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinExp (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinAbs calc alltypes) oldName newName =
   Calc_BuiltinAbs (convertCalculationNames calc oldName newName) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinNot calc alltypes) oldName newName =
   Calc_BuiltinNot (convertCalculationNames calc oldName newName) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinEqual pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinEqual (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinNotEqual pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinNotEqual (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinLessThan pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinLessThan (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinLessThanOrEqual pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinLessThanOrEqual (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinGreaterThan pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinGreaterThan (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinGreaterThanOrEqual pair1 pair2) oldName newName =
   let convertCalcPair pair = convertCalculationPair pair oldName newName
   in Calc_BuiltinGreaterThanOrEqual (convertCalcPair pair1) (convertCalcPair pair2)
convertCalculationNames (Calc_BuiltinAnd calc1 calc2 alltypes) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in Calc_BuiltinAnd (convertCalc calc1) (convertCalc calc2) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinOr calc1 calc2 alltypes) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in Calc_BuiltinOr (convertCalc calc1) (convertCalc calc2) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinXor calc1 calc2 alltypes) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in Calc_BuiltinXor (convertCalc calc1) (convertCalc calc2) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinNand calc1 calc2 alltypes) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in Calc_BuiltinNand (convertCalc calc1) (convertCalc calc2) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_BuiltinNor calc1 calc2 alltypes) oldName newName =
   let convertCalc calc = convertCalculationNames calc oldName newName
   in Calc_BuiltinNor (convertCalc calc1) (convertCalc calc2) (convertAllTypesNames alltypes oldName newName)
convertCalculationNames (Calc_ImplicitTypeConversion typeName calc) oldName newName =
   Calc_ImplicitTypeConversion (convertName typeName oldName newName) (convertCalculationNames calc oldName newName)
convertCalculationNames (Calc_SubtypeResult typeName calc) oldName newName =
   Calc_SubtypeResult (convertName typeName oldName newName) (convertCalculationNames calc oldName newName)
convertCalculationNames (Calc_ExplicitTypeConversion (typeName,typeData) pair) oldName newName =
   Calc_ExplicitTypeConversion
      ( convertName typeName oldName newName
      , convertTypeName typeData oldName newName
      )
      (convertCalculationPair pair oldName newName)
convertCalculationNames calc@(Calc_Signal _ _) _ _ = calc
convertCalculationNames calc@(Calc_SignalEvent _ _) _ _ = calc

convertCalculationPair :: (Calculation,AllTypes) -> NetlistName -> NetlistName -> (Calculation,AllTypes)
convertCalculationPair (calc,alltypes) oldName newName =
   (convertCalculationNames calc oldName newName,convertAllTypesNames alltypes oldName newName)

convertAllTypesNames :: AllTypes -> NetlistName -> NetlistName -> AllTypes
convertAllTypesNames (Type_Type typeName typeData) oldName newName =
   Type_Type
      (convertName typeName oldName newName)
      (convertTypeName typeData oldName newName)
convertAllTypesNames litType _ _ = litType

convertValueName :: Value -> NetlistName -> NetlistName -> Value
convertValueName val oldName newName =
   case val of
      Value_Enum (package,name) enum | package == oldName -> Value_Enum (newName,name) enum
      other -> other
      -- ?? Array
