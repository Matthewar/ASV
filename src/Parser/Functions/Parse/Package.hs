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
         )
import Control.Monad.Except
         ( ExceptT
         , throwError
         , lift
         )
import Control.Monad (unless)

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
         , isKeywordSignal
         , isKeywordSubtype
         , isKeywordProcedure
         , isKeywordType
         , isKeywordUse
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , NetlistStore(packages)
         , UnitStore
         , Package
         , NetlistName(..)
         )
import Parser.Netlist.Functions.Stores
         ( newPackage
         , convertPackageToGenericUnit
         )
import Manager.Types.Error (ConverterError(..))

parsePackage :: ScopeStore -> String -> ParserStack ()
parsePackage scope libraryName = do
   nameToken <- getToken
   let name1Info = fromJust $ matchIdentifier nameToken
       basePackage = newPackage scope
   package <- execStateT parsePackageDeclares basePackage
   possibleNameToken <- getToken
   case matchIdentifier possibleNameToken of
      Just name2Info -> do
         let getName = (map toUpper) . unPos
             name1 = getName name1Info
             name2 = getName name2Info
         unless (name1 == name2) $ throwError $ ConverterError_Parse $ PosnWrapper (getPos name2Info) $ ParseErr_PackageNamesNoMatch name1Info name2Info
         semiColonToken <- getToken
         unless (isSemicolon semiColonToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInPackage semiColonToken
         let netlistName = NetlistName libraryName name1
         let addPackage scope = scope { packages = MapS.insert netlistName package $ packages scope }
         modifyNetlist addPackage
      Nothing -> unless (isSemicolon possibleNameToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPackageEndOfDec possibleNameToken

-- |Monad stack for package building
type PackageBuildStack = StateT Package (StateT ParserState (StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO)))) ()

parsePackageDeclares :: PackageBuildStack
parsePackageDeclares = do
   (scopeStore,unitStore) <- gets convertPackageToGenericUnit
   nextToken <- lift getToken
   parsePackageDeclares' nextToken scopeStore unitStore

parsePackageDeclares' :: WrappedToken -> ScopeStore -> UnitStore -> PackageBuildStack
parsePackageDeclares' token scopeStore unitStore
   | isKeywordProcedure token = throwError $ ConverterError_NotImplemented $ passPosition "Procedure declaration" token
   | isKeywordFunction token = throwError $ ConverterError_NotImplemented $ passPosition "Function declaration" token
   | isKeywordType token = throwError $ ConverterError_NotImplemented $ passPosition "Type declaration" token
         --(typeName,newType) <- lift $ convertType scopeStore unitStore $ PosnWrapper declarePos typeDeclare
         --let insertPackageType package = package { packageTypes = MapS.insert typeName newType $ packageTypes package }
         --modify insertPackageType
   | isKeywordSubtype token = throwError $ ConverterError_NotImplemented $ passPosition "Subtype declaration" token
   | isKeywordConstant token = throwError $ ConverterError_NotImplemented $ passPosition "Constant declaration" token
         --constStore <- lift $ convertConstant scopeStore unitStore $ PosnWrapper declarePos constDeclare
         --let insertPackageConsts package = package { packageConstants = MapS.union constStore $ packageConstants package }
         --modify insertPackageConsts
   | isKeywordSignal token = throwError $ ConverterError_NotImplemented $ passPosition "Signal declaration" token
   | isKeywordFile token = throwError $ ConverterError_NotImplemented $ passPosition "File declaration" token
   | isKeywordAlias token = throwError $ ConverterError_NotImplemented $ passPosition "Alias declaration" token
   | isKeywordComponent token = throwError $ ConverterError_NotImplemented $ passPosition "Component declaration" token
   | isKeywordAttribute token = throwError $ ConverterError_NotImplemented $ passPosition "Attribute declaration/specification" token
   | isKeywordDisconnect token = throwError $ ConverterError_NotImplemented $ passPosition "Disconnection specification" token
   | isKeywordUse token = throwError $ ConverterError_NotImplemented $ passPosition "Use" token
   | isKeywordEnd token = return ()
   | otherwise = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPackageDeclItemOrEnd token
