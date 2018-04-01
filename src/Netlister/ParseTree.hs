{-|
   Module      : Netlister.ParseTree
   Description : Navigate through parse tree.
-}
module Netlister.ParseTree 
   ( convertTree
   ) where

import Control.Monad.Except
         ( liftEither
         , lift
         , withExceptT
         )
import Control.Monad.Trans.State (gets)
import qualified Data.Map.Strict as MapS

import Netlister.Types.Scope
         ( Scope
         , WrappedScopeConverterError
         , UnitScope
         )
import Netlister.Types.Top
         ( ConversionStack
         , ConverterError(ConverterError_Scope)
         )
import Netlister.Types.Stores
         ( NetlistStore(..)
         , NetlistName(..)
         )
import Parser.Happy.Types
         ( DesignFile(..)
         , DesignUnit(..)
         , WrappedDesignUnit
         , WrappedLibraryUnit
         )
import Parser.PositionWrapper (PosnWrapper(unPos))
import Netlister.Convert.Scope (convertScope)

convertTree :: (String -> String -> ConversionStack ()) -> String -> DesignFile -> ConversionStack ()
convertTree create library (DesignFile designUnits) =
   let convertTree' :: [WrappedDesignUnit] -> ConversionStack ()
       convertTree' (designUnit:otherUnits) = do
         (scope,library) <- lift $ withExceptT (ConverterError_Scope) $ liftEither $ getScopeAndLibraryUnit designUnit
         evalScope $ MapS.toList scope


         -- ?? Link scope, parse library
         convertTree' otherUnits
       convertTree' [] = return ()
       evalScope :: [(String,UnitScope)] -> ConversionStack ()
       evalScope ((libName,unitScope):libScope) =
         let evalUnitScope :: [String] -> ConversionStack ()
             evalUnitScope (unitName:otherUnitNames) = do
               let isInScope :: NetlistStore -> Bool
                   isInScope store = let netlistName = NetlistName libName unitName
                                     in MapS.member netlistName $ packages store
                                     -- ?? Add other checks
               inScope <- gets isInScope
               if not inScope then do
                  create libName unitName
                  evalUnitScope otherUnitNames
               else
                  evalUnitScope otherUnitNames -- ?? How to write this more concisely
             evalUnitScope [] = return ()
         in do
               evalUnitScope $ MapS.keys unitScope
               evalScope libScope
       evalScope [] = return ()
   in convertTree' designUnits

getScopeAndLibraryUnit :: WrappedDesignUnit -> Either WrappedScopeConverterError (Scope,WrappedLibraryUnit)
getScopeAndLibraryUnit designUnit = do
   let (DesignUnit context library) = unPos designUnit
   scope <- convertScope context
   return (scope,library)
