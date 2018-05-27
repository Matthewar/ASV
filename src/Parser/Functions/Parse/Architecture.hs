module Parser.Functions.Parse.Architecture
   ( parseArchitecture
   ) where

import qualified Data.Map.Strict as MapS
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
         , saveToken
         , accessNetlist
         , modifyNetlist
         )
import Parser.Functions.IdentifyToken
         ( matchIdentifier
         , isColon
         , isIdentifier
         , isKeywordAlias
         , isKeywordAssert
         , isKeywordAttribute
         , isKeywordBegin
         , isKeywordBlock
         , isKeywordComponent
         , isKeywordConfiguration
         , isKeywordConstant
         , isKeywordDisconnect
         , isKeywordEnd
         , isKeywordFile
         , isKeywordFor
         , isKeywordFunction
         , isKeywordIf
         , isKeywordIs
         , isKeywordOf
         , isKeywordProcedure
         , isKeywordProcess
         , isKeywordSignal
         , isKeywordSubtype
         , isKeywordType
         , isKeywordUse
         , isKeywordWith
         , isLeftParen
         , isSemicolon
         )
import Parser.Functions.Parse.Type (parseType)
import Parser.Functions.Parse.Subtype (parseSubtype)
import Parser.Functions.Parse.Objects
         ( parseConstant
         , parseSignal
         )
import Parser.Functions.Parse.Process (parseProcess)
import Parser.Netlist.Types.Representation (NetlistName(..))
import Parser.Netlist.Types.Stores
         ( Architecture(..)
         , ScopeStore
         , NetlistStore(..)
         )
import Parser.Netlist.Functions.Stores
         ( newArchitecture
         , convertArchitectureToGenericUnit
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Manager.Types.Error (ConverterError(..))

parseArchitecture :: ScopeStore -> String -> ParserStack ()
parseArchitecture scope libraryName = do
   [nameToken,ofTok,nameToken2,isTok] <- replicateM 4 getToken
   name1Info <- case matchIdentifier nameToken of
      Just name -> return name
      Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedArchitectureSimpleName nameToken
   unless (isKeywordOf ofTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordOfInArch ofTok
   entityNameInfo <- case matchIdentifier nameToken2 of
      Just name -> return name
      Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEntityNameInArch nameToken2
   unless (isKeywordIs isTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordIsInArch isTok
   let entityName = NetlistName libraryName $ unPos entityNameInfo
       findEntity netlist = MapS.lookup entityName $ entities netlist
   wrappedEntity <- accessNetlist $ gets findEntity
   entity <- case wrappedEntity of
      Just entityData -> return entityData
      Nothing -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_UnableToFindEntityForArch entityName) entityNameInfo
   let getName = (map toUpper) . unPos
       name1 = getName name1Info
       netlistName = NetlistName libraryName name1
       baseArchitecture = newArchitecture entity
   initialArch <- execStateT (parseArchitectureDeclares netlistName) baseArchitecture
   architecture <- execStateT (parseArchitectureStatements netlistName) initialArch
   possibleNameToken <- getToken
   case matchIdentifier possibleNameToken of
      Just name2Info -> do
         let name2 = getName name2Info
         unless (name1 == name2) $ throwError $ ConverterError_Parse $ passPosition (ParseErr_ArchitectureNamesNoMatch name1Info name2Info) name2Info
         semiColonToken <- getToken
         unless (isSemicolon semiColonToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInArchitecture semiColonToken
         let addArchitecture scope = scope { architectures = MapS.insert netlistName architecture $ architectures scope }
         modifyNetlist addArchitecture
      Nothing -> unless (isSemicolon possibleNameToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedArchitectureEndOfDec possibleNameToken

-- |Monad stack for entity building
type ArchBuildStack = StateT Architecture (StateT ParserState (StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO)))) ()

parseArchitectureDeclares :: NetlistName -> ArchBuildStack
parseArchitectureDeclares architectureName = do
   (scopeStore,unitStore) <- gets convertArchitectureToGenericUnit
   token <- lift getToken
   case token of
      _ | isKeywordProcedure token -> throwError $ ConverterError_NotImplemented $ passPosition "Procedure declaration" token
      _ | isKeywordFunction token -> throwError $ ConverterError_NotImplemented $ passPosition "Function declaration" token
      _ | isKeywordType token -> do
            (modType,modSubtype) <- lift $ parseType scopeStore unitStore architectureName
            let insertArchType arch =
                  arch
                     { archTypes = modType $ archTypes arch
                     , archSubtypes = modSubtype $ archSubtypes arch
                     }
            modify insertArchType
      _ | isKeywordSubtype token -> do
         modSubtype <- lift $ parseSubtype scopeStore unitStore architectureName
         let insertArchSubtype arch = arch { archSubtypes = modSubtype $ archSubtypes arch }
         modify insertArchSubtype
      _ | isKeywordConstant token -> do
            (constStore,modSubtype) <- lift $ parseConstant scopeStore unitStore architectureName
            let insertArchConsts arch =
                  arch
                     { archConstants = MapS.union constStore $ archConstants arch
                     , archSubtypes = modSubtype $ archSubtypes arch
                     }
            modify insertArchConsts
      _ | isKeywordSignal token -> do
            (signalStore,modSubtype) <- lift $ parseSignal scopeStore unitStore architectureName
            let insertArchSignals arch =
                  arch
                     { archSignals = MapS.union signalStore $ archSignals arch
                     , archSubtypes = modSubtype $ archSubtypes arch
                     }
            modify insertArchSignals
      _ | isKeywordFile token -> throwError $ ConverterError_NotImplemented $ passPosition "File declaration" token
      _ | isKeywordAlias token -> throwError $ ConverterError_NotImplemented $ passPosition "Alias declaration" token
      _ | isKeywordComponent token -> throwError $ ConverterError_NotImplemented $ passPosition "Component declaration" token
      _ | isKeywordAttribute token -> throwError $ ConverterError_NotImplemented $ passPosition "Attribute declaration/specification" token
      _ | isKeywordConfiguration token -> throwError $ ConverterError_NotImplemented $ passPosition "Configuration specification" token
      _ | isKeywordDisconnect token -> throwError $ ConverterError_NotImplemented $ passPosition "Disconnection specification" token
      _ | isKeywordUse token -> throwError $ ConverterError_NotImplemented $ passPosition "Use" token
      _ | isKeywordBegin token -> return ()
      _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedArchDeclItemOrEnd token
   unless (isKeywordBegin token) $ parseArchitectureDeclares architectureName

parseArchitectureStatements :: NetlistName -> ArchBuildStack
parseArchitectureStatements archName = do
   (scopeStore,unitStore) <- gets convertArchitectureToGenericUnit
   token <- lift getToken
   case matchIdentifier token of
      Just (PosnWrapper pos label) -> do
         let upperLabel = PosnWrapper pos $ map toUpper label
         nextTok <- lift getToken
         case nextTok of
            token | isColon token -> do
               keywordTok <- lift getToken
               case keywordTok of
                  token | isKeywordBlock token -> throwError $ ConverterError_NotImplemented $ passPosition "Block statement" token
                  token | isKeywordProcess token -> do
                     (label,newProcess) <- lift $ parseProcess scopeStore unitStore archName True $ Just upperLabel
                     let insertProcess arch = arch { archProcesses = MapS.insert label newProcess $ archProcesses arch }
                     modify insertProcess
                  token | isIdentifier token -> throwError $ ConverterError_NotImplemented $ passPosition "Concurrent procedure call or concurrent signal assignment statement or component instantiation statement" token
                  token | isKeywordAssert token -> throwError $ ConverterError_NotImplemented $ passPosition "Concurrent assertion statement" token
                  token | isLeftParen token -> throwError $ ConverterError_NotImplemented $ passPosition "Concurrent conditional signal assignment statement" token
                  token | isKeywordWith token -> throwError $ ConverterError_NotImplemented $ passPosition "Concurrent selected signal assignment" token
                  token | isKeywordFor token || isKeywordIf token -> throwError $ ConverterError_NotImplemented $ passPosition "Generate statement" token
                  token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedArchStatementItem token
            _ | isLeftParen nextTok || isSemicolon nextTok -> do
               lift $ saveToken nextTok
               lift $ saveToken token
               throwError $ ConverterError_NotImplemented $ passPosition "Passive concurrent procedure call" token
            _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedColonInArchStatement nextTok -- ?? This error could also be malformed procedure call (etc.) with no label
      Nothing | isKeywordProcess token -> do
         (autoLabel,newProcess) <- lift $ parseProcess scopeStore unitStore archName True Nothing
         let insertProcess arch = arch { archProcesses = MapS.insert autoLabel newProcess $ archProcesses arch }
         modify insertProcess
      Nothing | isKeywordAssert token -> throwError $ ConverterError_NotImplemented $ passPosition "Concurrent assertion statement" token
      Nothing | isLeftParen token -> throwError $ ConverterError_NotImplemented $ passPosition "Aggregate target in conditional signal assignment" token
      Nothing | isKeywordWith token  -> throwError $ ConverterError_NotImplemented $ passPosition "Selected signal assignment" token
      Nothing | isKeywordEnd token -> return ()
      _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedArchStatementItemOrEnd token
   unless (isKeywordEnd token) $ parseArchitectureStatements archName
