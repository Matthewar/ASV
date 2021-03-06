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

import Lexer.Types.PositionWrapper (PosnWrapper(unPos))
import Parser.Happy.Types
         ( DesignFile(..)
         , DesignUnit(..)
         , WrappedDesignUnit
         , WrappedLibraryUnit
         )
import Netlister.Types.Scope
         ( Scope
         , WrappedScopeConverterError
         , UnitScope
         )
import Netlister.Types.Stores (NetlistName(..))
import Netlister.Types.Top
         ( ConversionStack
         , ConverterError(ConverterError_Scope)
         )
import Netlister.Convert.Scope
         ( convertScope
         , evalScope
         )
import Netlister.Parse.Library (convertLibrary)

convertTree :: (String -> String -> ConversionStack ()) -> String -> [NetlistName] -> DesignFile -> ConversionStack ()
convertTree create libraryName dependencies (DesignFile designUnits) =
   let convertTree' :: [WrappedDesignUnit] -> ConversionStack ()
       convertTree' (designUnit:otherUnits) = do
         (scope,library) <- lift $ withExceptT (ConverterError_Scope) $ liftEither $ getScopeAndLibraryUnit designUnit
         realScope <- evalScope create scope dependencies
         convertLibrary realScope libraryName library


         -- ?? Link scope, parse library
         convertTree' otherUnits
       convertTree' [] = return ()
   in convertTree' designUnits

getScopeAndLibraryUnit :: WrappedDesignUnit -> Either WrappedScopeConverterError (Scope,WrappedLibraryUnit)
getScopeAndLibraryUnit designUnit = do
   let (DesignUnit context library) = unPos designUnit
   scope <- convertScope context
   return (scope,library)
