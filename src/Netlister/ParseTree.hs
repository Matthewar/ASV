{-|
   Module      : Netlister.ParseTree
   Description : Navigate through parse tree.
-}
module Netlister.ParseTree 
   ( convertTree
   ) where

import Netlister.Types.Scope
         ( Scope
         , WrappedScopeConverterError
         )
import Netlister.Types.Top (ConversionStack)
import Parser.Happy.Types
         ( DesignFile(..)
         , DesignUnit(..)
         , WrappedDesignUnit
         , WrappedLibraryUnit
         )
import Parser.PositionWrapper (PosnWrapper(unPos))
import Netlister.Convert.Scope (convertScope)

convertTree :: (String -> String -> ConversionStack ()) -> DesignFile -> ConversionStack ()
convertTree create (DesignFile designUnits) =
   let convertTree' (designUnit:otherUnits) = do
         --(scope,library) <- getScopeAndLibraryUnit designUnit
         
         --create
         return ()
       convertTree' [] = return ()
   in convertTree' designUnits

getScopeAndLibraryUnit :: WrappedDesignUnit -> Either WrappedScopeConverterError (Scope,WrappedLibraryUnit)
getScopeAndLibraryUnit designUnit = do
   let (DesignUnit context library) = unPos designUnit
   scope <- convertScope context
   return (scope,library)
