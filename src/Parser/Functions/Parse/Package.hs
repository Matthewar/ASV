module Parser.Functions.Parse.Package
   ( parsePackage
   ) where

import qualified Data.Map.Strict as MapS
import Data.Maybe (fromJust)
import Data.Char (toUpper)
import Control.Monad.Trans.State
         ( StateT
         , execStateT
         , gets
         , modify
         )
import Control.Monad.Except
         ( ExceptT
         , throwError
         , lift
         )
import Control.Monad
         ( unless
         , replicateM
         )

import Lexer.Types.Token (WrappedToken)
import Lexer.Types.Error (ParserError(..))
import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper
         ( raisePosition
         , passPosition
         )
import Lexer.Alex.Types (AlexState)
import Parser.Types.Monad
         ( ParserStack
         , ParserState
         )
import Parser.Functions.Monad
         ( getToken
         , modifyNetlist
         )
import Parser.Functions.IdentifyToken
         ( matchIdentifier
         , isSemicolon
         , isKeywordAlias
         , isKeywordAttribute
         , isKeywordComponent
         , isKeywordConstant
         , isKeywordDisconnect
         , isKeywordEnd
         , isKeywordFile
         , isKeywordFunction
         , isKeywordIs
         , isKeywordSignal
         , isKeywordSubtype
         , isKeywordProcedure
         , isKeywordType
         , isKeywordUse
         )
import Parser.Functions.Parse.Type (parseType)
import Parser.Functions.Parse.Subtype (parseSubtype)
import Parser.Functions.Parse.Objects (parseConstant)
import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , NetlistStore(packages)
         , UnitStore
         , Package(..)
         )
import Parser.Netlist.Functions.Stores
         ( newPackage
         , convertPackageToGenericUnit
         )
import Manager.Types.Error (ConverterError(..))

parsePackage :: ScopeStore -> String -> ParserStack ()
parsePackage scope libraryName = do
   [nameToken,contToken] <- replicateM 2 getToken
   let name1Info = fromJust $ matchIdentifier nameToken -- ?? Would that throw an error if name isn't name
       basePackage = newPackage scope
       getName = (map toUpper) . unPos
       name1 = getName name1Info
       netlistName = NetlistName libraryName name1
   unless (isKeywordIs contToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordIsInPackage contToken
   package <- execStateT (parsePackageDeclares netlistName) basePackage
   possibleNameToken <- getToken
   case matchIdentifier possibleNameToken of
      Just name2Info -> do
         let name2 = getName name2Info
         unless (name1 == name2) $ throwError $ ConverterError_Parse $ PosnWrapper (getPos name2Info) $ ParseErr_PackageNamesNoMatch name1Info name2Info
         semiColonToken <- getToken
         unless (isSemicolon semiColonToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInPackage semiColonToken
         let addPackage scope = scope { packages = MapS.insert netlistName package $ packages scope }
         modifyNetlist addPackage
      Nothing -> unless (isSemicolon possibleNameToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPackageEndOfDec possibleNameToken

-- |Monad stack for package building
type PackageBuildStack = StateT Package (StateT ParserState (StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO)))) ()

parsePackageDeclares :: NetlistName -> PackageBuildStack
parsePackageDeclares packageName = do
   (scopeStore,unitStore) <- gets convertPackageToGenericUnit -- ?? Is it better to repeat this each time or transfer generalised unit back to package at end
   parsePackageDeclares' scopeStore unitStore packageName

parsePackageDeclares' :: ScopeStore -> UnitStore -> NetlistName -> PackageBuildStack
parsePackageDeclares' scopeStore unitStore packageName = do
   token <- lift $ getToken
   case token of
      _ | isKeywordProcedure token -> throwError $ ConverterError_NotImplemented $ passPosition "Procedure declaration" token
      _ | isKeywordFunction token -> throwError $ ConverterError_NotImplemented $ passPosition "Function declaration" token
      _ | isKeywordType token -> do
            (modType,modSubtype) <- lift $ parseType scopeStore unitStore packageName
            let insertPackageType package =
                  package
                     { packageTypes = modType $ packageTypes package
                     , packageSubtypes = modSubtype $ packageSubtypes package
                     }
            modify insertPackageType
      _ | isKeywordSubtype token -> do
         modSubtype <- lift $ parseSubtype scopeStore unitStore packageName
         let insertPackageSubtype package = package { packageSubtypes = modSubtype $ packageSubtypes package }
         modify insertPackageSubtype
      _ | isKeywordConstant token -> do
            (constStore,modSubtype) <- lift $ parseConstant scopeStore unitStore packageName
            let insertPackageConsts package =
                  package
                     { packageConstants = MapS.union constStore $ packageConstants package
                     , packageSubtypes = modSubtype $ packageSubtypes package
                     }
            modify insertPackageConsts
      _ | isKeywordSignal token -> throwError $ ConverterError_NotImplemented $ passPosition "Signal declaration" token
      _ | isKeywordFile token -> throwError $ ConverterError_NotImplemented $ passPosition "File declaration" token
      _ | isKeywordAlias token -> throwError $ ConverterError_NotImplemented $ passPosition "Alias declaration" token
      _ | isKeywordComponent token -> throwError $ ConverterError_NotImplemented $ passPosition "Component declaration" token
      _ | isKeywordAttribute token -> throwError $ ConverterError_NotImplemented $ passPosition "Attribute declaration/specification" token
      _ | isKeywordDisconnect token -> throwError $ ConverterError_NotImplemented $ passPosition "Disconnection specification" token
      _ | isKeywordUse token -> throwError $ ConverterError_NotImplemented $ passPosition "Use" token
      _ | isKeywordEnd token -> return ()
      _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPackageDeclItemOrEnd token
   parsePackageDeclares packageName
