{-|
   Module      : Netlister.Parse.Library
   Description : Navigate and convert library section of tree
-}
module Netlister.Parse.Library
   ( convertLibrary
   ) where

import Control.Monad.Except (throwError)

import Lexer.Types.PositionWrapper (PosnWrapper(..))
import Parser.Happy.Types
         ( WrappedLibraryUnit
         , LibraryUnit(..)
         , PrimaryUnit(..)
         )
import Netlister.Types.Top
         ( ConversionStack
         , ConverterError(ConverterError_NotImplemented)
         )
import Netlister.Types.Stores
         ( ScopeStore
         , NetlistName
         )
import Netlister.Parse.Package (convertPackage)

convertLibrary :: ScopeStore -> String -> WrappedLibraryUnit -> ConversionStack ()
convertLibrary scope libraryName (PosnWrapper pos (Library_PrimaryUnit primaryUnit)) =
   case primaryUnit of
      PrimaryUnit_EntityDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Entity declaration"
      PrimaryUnit_ConfigurationDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Configuration declaration"
      PrimaryUnit_PackageDeclaration packageDecl -> convertPackage scope libraryName $ PosnWrapper pos packageDecl
convertLibrary _ _ (PosnWrapper pos (Library_SecondaryUnit _)) =
   throwError $ ConverterError_NotImplemented $ PosnWrapper pos "Alias"
   -- Add scope from entity/architecture
   -- Convert unit if necessary
