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
import Parser.Happy.Types
         ( DesignFile(..)
         , DesignUnit(..)
         , WrappedDesignUnit
         , WrappedLibraryUnit
         )
import Parser.PositionWrapper (PosnWrapper(unPos))
import Netlister.Convert.Scope
         ( convertScope
         , evalScope
         )

convertTree :: (String -> String -> ConversionStack ()) -> String -> DesignFile -> ConversionStack ()
convertTree create libraryName (DesignFile designUnits) =
   let convertTree' :: [WrappedDesignUnit] -> ConversionStack ()
       convertTree' (designUnit:otherUnits) = do
         (scope,library) <- lift $ withExceptT (ConverterError_Scope) $ liftEither $ getScopeAndLibraryUnit designUnit
         evalScope create scope


         -- ?? Link scope, parse library
         convertTree' otherUnits
       convertTree' [] = return ()
   in convertTree' designUnits

getScopeAndLibraryUnit :: WrappedDesignUnit -> Either WrappedScopeConverterError (Scope,WrappedLibraryUnit)
getScopeAndLibraryUnit designUnit = do
   let (DesignUnit context library) = unPos designUnit
   scope <- convertScope context
   return (scope,library)
