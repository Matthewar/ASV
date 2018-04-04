{-|
   Module      : Netlister.Parse.Package
   Description : Navigate and convert package part of tree
-}
module Netlister.Parse.Package
   ( convertPackage
   ) where

import Control.Monad.Except
         ( ExceptT
         , throwError
         )
import Control.Monad.Trans.State
         ( StateT
         , execStateT
         , modify
         )
import qualified Data.Map.Strict as MapS

import Parser.Alex.BaseTypes (AlexPosn)
import Parser.Happy.Types
         ( PackageDeclaration(..)
         , WrappedPackageDeclaration
         , PackageDeclarativePart
         , PackageDeclarativeItem(..)
         )
import Parser.PositionWrapper
import Netlister.Types.Top
         ( ConversionStack
         , ConverterError 
            ( ConverterError_Netlist
            , ConverterError_NotImplemented
            )
         , NetlistError(NetlistError_UnmatchedPackageName)
         )
import Netlister.Types.Scope (Scope)
import Netlister.Types.Stores
         ( NetlistStore(..)
         , NetlistName(..)
         , ScopeStore
         , Package
         , newPackage
         )

-- |Convert package header unit
convertPackage :: ScopeStore -> String -> WrappedPackageDeclaration -> ConversionStack ()
convertPackage _ _ (PosnWrapper pos (PackageDeclaration name1 _ (Just name2)))
   | (unPos name1) /= (unPos name2) = throwError $ ConverterError_Netlist $ PosnWrapper pos $ NetlistError_UnmatchedPackageName name1 name2
convertPackage scope libraryName (PosnWrapper packagePos (PackageDeclaration (PosnWrapper namePos packageName) packageDeclares _)) = do
   let netlistName = NetlistName libraryName packageName
       basePackage = newPackage scope
   newPackage <- execStateT (convertPackageDeclares $ reverse packageDeclares) basePackage
   let addPackage scope = scope { packages = MapS.insert netlistName newPackage $ packages scope }
   modify addPackage

-- |Convert package declares
convertPackageDeclares :: PackageDeclarativePart -> StateT Package (StateT NetlistStore (ExceptT ConverterError IO)) ()
convertPackageDeclares (PosnWrapper declarePos declare:packageDeclares) =
   case declare of
      PackageDeclarativeItem_SubprogramDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Subprogram declaration"
      PackageDeclarativeItem_TypeDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Type declaration"
      PackageDeclarativeItem_SubtypeDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Subtype declaration"
      PackageDeclarativeItem_ConstantDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Constant declaration"
      PackageDeclarativeItem_SignalDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Signal declaration"
      PackageDeclarativeItem_FileDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "File declaration"
      PackageDeclarativeItem_AliasDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Alias"
      PackageDeclarativeItem_ComponentDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "component declaration"
      PackageDeclarativeItem_AttributeDeclaration _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Attribute declartion"
      PackageDeclarativeItem_AttributeSpecification _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Attribute specification"
      PackageDeclarativeItem_DisconnectionSpecification _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Disconnection specification"
      PackageDeclarativeItem_UseClause _ -> throwError $ ConverterError_NotImplemented $ PosnWrapper declarePos "Use clause in package"
convertPackageDeclares [] = return ()
