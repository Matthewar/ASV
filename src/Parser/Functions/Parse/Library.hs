module Parser.Functions.Parse.Library
   ( parseLibrary
   ) where

import Control.Monad.Except (throwError)

import Lexer.Types.Token (WrappedToken)
import Lexer.Types.Error (ParserError(..))
import Lexer.Functions.PositionWrapper
         ( raisePosition
         , passPosition
         )
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Functions.IdentifyToken
         ( isKeywordArchitecture
         , isKeywordBody
         , isKeywordConfiguration
         , isKeywordEntity
         , isKeywordPackage
         , isIdentifier
         )
import Parser.Functions.Parse.Package (parsePackage)
import Parser.Functions.Parse.Entity (parseEntity)
import Parser.Netlist.Types.Stores (ScopeStore)
import Manager.Types.Error (ConverterError(..))

parseLibrary :: ScopeStore -> String -> ParserStack ()
parseLibrary scope libraryName = do
   token <- getToken
   library <- matchLibraryUnit token
   case library of
      Primary_Entity -> parseEntity scope libraryName
      Primary_Configuration -> throwError $ ConverterError_NotImplemented $ passPosition "Configuration declaration" token
      Primary_Package -> parsePackage scope libraryName
      Secondary_Architecture -> throwError $ ConverterError_NotImplemented $ passPosition "Architecture body" token
      Secondary_PackageBody -> throwError $ ConverterError_NotImplemented $ passPosition "Package body" token
   -- For secondaries:
   -- Add scope from entity/architecture
   -- Convert unit if necessary

-- |Identification of library unit
data LibraryUnitType =
   -- |Entity declaration
   Primary_Entity
   -- |Configuration declaration
   | Primary_Configuration
   -- |Package declaration
   | Primary_Package
   -- |Architecture body
   | Secondary_Architecture
   -- |Package body
   | Secondary_PackageBody

-- |Find which library unit this is
matchLibraryUnit :: WrappedToken -> ParserStack LibraryUnitType
matchLibraryUnit token
   | isKeywordEntity token = return Primary_Entity
   | isKeywordConfiguration token = return Primary_Configuration
   | isKeywordArchitecture token = return Secondary_Architecture
   | isKeywordPackage token = do
      token2 <- getToken
      matchLibraryUnit' token2
   | otherwise = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedFirstKeywordInLibraryUnit token
   where matchLibraryUnit' token
            | isKeywordBody token = return Secondary_PackageBody
            | isIdentifier token = do
               saveToken token
               return Primary_Package
            | otherwise = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPackageBodyKeywordInLibraryUnit token
