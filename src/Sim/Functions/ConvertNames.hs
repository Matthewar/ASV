module Sim.Functions.ConvertNames
   ( convertEntityNames
   ) where

import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Value(..)
         , Port(..)
         , Generic(..)
         , Subtype(..)
         )
import Parser.Netlist.Types.Stores
         ( Entity(..)
         )

convertEntityNames :: Entity -> NetlistName -> NetlistName -> Entity
convertEntityNames (Entity scope generics ports funcs types subtypes consts signals processes) oldName newName =
   Entity
      scope
      (convertGenericNames generics oldName newName)
      (convertPortNames ports oldName newName)
      -- ?? TODO
      funcs
      types
      subtypes
      consts
      signals
      processes
      --(convertFunctionNames funcs oldName newName)
      --(convertTypeNames types oldName newName)
      --(convertSubtypeNames subtypes oldName newName)
      --(convertConstantNames consts oldName newName)
      --(convertSignalNames signals oldName newName)
      --(convertProcessNames processes oldName newName)

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

--convertFunctionNames :: Function
--convertTypeNames

--convertSubtypeNames 

convertSubtypeName :: Subtype -> NetlistName -> NetlistName -> Subtype
convertSubtypeName subtype oldName newName =
   case subtype of
      EnumerationSubtype resFunc typeName enums constraint -> EnumerationSubtype (convertResFunc resFunc oldName newName) (convertName typeName oldName newName) enums constraint
      IntegerSubtype resFunc typeName constraint -> IntegerSubtype (convertResFunc resFunc oldName newName) (convertName typeName oldName newName) constraint
   where convertResFunc (Just name) oldName newName = Just $ convertName name oldName newName
         convertResFunc Nothing _ _ = Nothing
         convertName (package,name) oldName newName
            | package == oldName = (newName,name)
            | otherwise = (package,name)
--convertConstantNames
--convertSignalNames
--convertProcessNames

convertValueName :: Value -> NetlistName -> NetlistName -> Value
convertValueName val oldName newName =
   case val of
      Value_Enum (package,name) enum | package == oldName -> Value_Enum (newName,name) enum
      other -> other
      -- ?? Array
