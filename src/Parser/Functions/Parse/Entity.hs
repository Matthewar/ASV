module Parser.Functions.Parse.Entity
   ( parseEntity
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
         , isKeywordBuffer
         , isKeywordBus
         , isKeywordConstant
         , isKeywordDisconnect
         , isKeywordEnd
         , isKeywordFile
         , isKeywordFunction
         , isKeywordGeneric
         , isKeywordIn
         , isKeywordInout
         , isKeywordIs
         , isKeywordLinkage
         , isKeywordOut
         , isKeywordSignal
         , isKeywordSubtype
         , isKeywordPort
         , isKeywordProcedure
         , isKeywordProcess
         , isKeywordType
         , isKeywordUse
         , isLeftParen
         , isRightParen
         , isSemicolon
         , isVarAssign
         )
import Parser.Functions.Parse.Type (parseType)
import Parser.Functions.Parse.Subtype
         ( parseSubtype
         , parseSubtypeIndication
         )
import Parser.Functions.Parse.Objects
         ( parseConstant
         , parseSignal
         , parseIdentifierList
         )
import Parser.Functions.Parse.Expression
         ( parseExpression
         , Staticity(..)
         , staticTypeCompare
         )
import Parser.Functions.Parse.Process (parseProcess)
import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , Subtype(..)
         , IntegerRange(..)
         , FloatRange(..)
         , Generic(..)
         , Port(..)
         , Mode(..)
         , Value(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , NetlistStore(entities)
         , UnitStore
         , Entity(..)
         , SubtypeStore
         )
import Parser.Netlist.Functions.Stores
         ( newEntity
         , convertEntityToGenericUnit
         )
import Manager.Types.Error (ConverterError(..))

parseEntity :: ScopeStore -> String -> ParserStack ()
parseEntity scope libraryName = do
   [nameToken,contToken] <- replicateM 2 getToken
   let baseEntity = newEntity scope
       getName = (map toUpper) . unPos
   name1Info <- case matchIdentifier nameToken of
                  Just info -> return info
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEntityName nameToken
   let name1 = getName name1Info
       unitName = NetlistName libraryName name1
   unless (isKeywordIs contToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordIsInEntity contToken
   entity <- execStateT (parseEntityInner unitName) baseEntity
   possibleNameToken <- getToken
   case matchIdentifier possibleNameToken of
      Just name2Info -> do
         let name2 = getName name2Info
         unless (name1 == name2) $ throwError $ ConverterError_Parse $ PosnWrapper (getPos name2Info) $ ParseErr_EntityNamesNoMatch name1Info name2Info
         semiColonToken <- getToken
         unless (isSemicolon semiColonToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInEntity semiColonToken
         let addEntity scope = scope { entities = MapS.insert unitName entity $ entities scope }
         modifyNetlist addEntity
      Nothing -> unless (isSemicolon possibleNameToken) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEntityEndOfDec possibleNameToken

-- |Monad stack for entity building
type EntityBuildStack = StateT Entity (StateT ParserState (StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO)))) ()

parseEntityInner :: NetlistName -> EntityBuildStack
parseEntityInner entityName = do
   parseEntityHeader entityName
   parseEntityDeclares entityName -- ?? Must not absorb end token
   beginTok <- lift getToken
   case beginTok of
      token | isKeywordBegin token -> do
         parseEntityStatements entityName -- ?? Must absorb end token
      token | isKeywordEnd token -> return ()
      token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordBeginOrEndInEntity token

parseEntityHeader :: NetlistName -> EntityBuildStack
parseEntityHeader entityName = do
   parseEntityGenerics entityName
   parseEntityPorts entityName

parseEntityGenerics :: NetlistName -> EntityBuildStack
parseEntityGenerics entityName = do
   [genericTok,leftParenTok] <- lift $ replicateM 2 getToken
   case (genericTok,leftParenTok) of
      (token,_) | not $ isKeywordGeneric token -> lift $ do
         saveToken leftParenTok
         saveToken genericTok
      _ | isKeywordGeneric genericTok && not (isLeftParen leftParenTok) -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedLeftParenInEntityGenericHeader leftParenTok
      _ | isKeywordGeneric genericTok && isLeftParen leftParenTok -> parseGenericList entityName

parseGenericList :: NetlistName -> EntityBuildStack
parseGenericList entityName = do
   (scopeStore,unitStore) <- gets convertEntityToGenericUnit
   (generics,modSubtypes) <- lift $ parseGenericList' scopeStore unitStore entityName [] MapS.empty -- ?? Add generic mode arg
   let insertEntityGenerics entity =
         entity
            { entityGenerics = generics
            , entitySubtypes = modSubtypes $ entitySubtypes entity
            }
   modify insertEntityGenerics
   endTok <- lift getToken
   unless (isSemicolon endTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInEntityGenericHeaderEnd endTok

parseGenericList' :: ScopeStore -> UnitStore -> NetlistName -> [Generic] -> SubtypeStore -> ParserStack ([Generic],SubtypeStore -> SubtypeStore)
parseGenericList' scope unit entityName generics subtypes = do
   firstTok <- getToken
   case firstTok of
      token | isKeywordConstant token -> return ()
      token -> saveToken token
   genericNames <- parseIdentifierList scope unit
   [colonTok,potKeywordInTok] <- replicateM 2 getToken
   unless (isColon colonTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedColonInInterfaceDecl colonTok
   case potKeywordInTok of
      token | isKeywordIn token -> return ()
      token -> saveToken token
   (subtypePackage,subtypeName,subtypeData,modSubtype) <- parseSubtypeIndication False scope unit entityName
   assignTok <- getToken
   defaultValue <- case assignTok of
      token | isVarAssign token -> do
         values <- parseExpression LocallyStatic scope unit entityName
         value <- staticTypeCompare subtypeData values $ getPos token
         return $ Just value
      token -> do
         saveToken token
         return Nothing
   let newGeneric =
         Generic
            ""
            (subtypePackage,subtypeName)
            subtypeData
            defaultValue
       newGenerics = map (\name -> newGeneric { generic_name = name }) genericNames
       totalGenerics = generics ++ newGenerics
       newSubtypes = modSubtype subtypes
   nextTok <- getToken
   case nextTok of
      token | isSemicolon token -> parseGenericList' scope unit entityName totalGenerics newSubtypes
      token | isRightParen token -> return (totalGenerics,MapS.union newSubtypes)
      token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedInterfaceContOrEnd token

parseEntityPorts :: NetlistName -> EntityBuildStack
parseEntityPorts entityName = do
   [portTok,leftParenTok] <- lift $ replicateM 2 getToken
   case (portTok,leftParenTok) of
      (token,_) | not $ isKeywordPort token -> lift $ do
         saveToken leftParenTok
         saveToken portTok
      _ | isKeywordPort portTok && not (isLeftParen leftParenTok) -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedLeftParenInEntityPortHeader leftParenTok
      _ | isKeywordPort portTok && isLeftParen leftParenTok -> parsePortList entityName

parsePortList :: NetlistName -> EntityBuildStack
parsePortList entityName = do
   (scopeStore,unitStore) <- gets convertEntityToGenericUnit
   (ports,modSubtypes) <- lift $ parsePortList' scopeStore unitStore entityName [] MapS.empty -- ?? Add generic mode arg
   let insertEntityPorts entity =
         entity
            { entityPorts = ports
            , entitySubtypes = modSubtypes $ entitySubtypes entity
            }
   modify insertEntityPorts
   endTok <- lift getToken
   unless (isSemicolon endTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInEntityPortHeaderEnd endTok

parsePortList' :: ScopeStore -> UnitStore -> NetlistName -> [Port] -> SubtypeStore -> ParserStack ([Port],SubtypeStore -> SubtypeStore)
parsePortList' scope unit entityName ports subtypes = do
   firstTok <- getToken
   case firstTok of
      token | isKeywordSignal token -> return ()
      token -> saveToken token
   portNames <- parseIdentifierList scope unit
   [colonTok,potKeywordInTok] <- replicateM 2 getToken
   unless (isColon colonTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedColonInInterfaceDecl colonTok
   mode <- case potKeywordInTok of
      token | isKeywordIn token -> return Mode_In
      token | isKeywordOut token -> return Mode_Out
      token | isKeywordInout token -> throwError $ ConverterError_NotImplemented $ passPosition "Inout mode port" token
      token | isKeywordLinkage token -> throwError $ ConverterError_NotImplemented $ passPosition "Linkage mode port" token
      token | isKeywordBuffer token -> throwError $ ConverterError_NotImplemented $ passPosition "Buffer mode port" token
      token -> do
         saveToken token
         return Mode_In
   (subtypePackage,subtypeName,subtypeData,modSubtype) <- parseSubtypeIndication False scope unit entityName -- ?? Should be true
   busTok <- getToken
   case busTok of
      token | isKeywordBus token -> throwError $ ConverterError_NotImplemented $ passPosition "Bus guarded interface list signal" token
      token -> saveToken token
   assignTok <- getToken
   defaultValue <- case assignTok of
      token | isVarAssign token -> do
         values <- parseExpression LocallyStatic scope unit entityName
         staticTypeCompare subtypeData values $ getPos token
      token -> do
         saveToken token
         case subtypeData of
            EnumerationSubtype _ baseTypeName _ (left,_) -> return $ Value_Enum baseTypeName left
            IntegerSubtype _ _ (IntegerRange left _ _) -> return $ Value_Int $ toInteger left
            FloatingSubtype _ _ (FloatRange left _ _) -> return $ Value_Float left
            PhysicalSubtype _ _ _ _ (IntegerRange left _ _) -> return $ Value_Physical $ toInteger left
            ArraySubtype _ _ _ _ _ -> throwError $ ConverterError_NotImplemented $ passPosition "Array type" token
   let newPort =
         Port
            ""
            mode
            (subtypePackage,subtypeName)
            subtypeData
            defaultValue
       newPorts = map (\name -> newPort { port_name = name }) portNames
       totalPorts = ports ++ newPorts
       newSubtypes = modSubtype subtypes
   nextTok <- getToken
   case nextTok of
      token | isSemicolon token -> parsePortList' scope unit entityName totalPorts newSubtypes
      token | isRightParen token -> return (totalPorts,MapS.union newSubtypes)
      token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedInterfaceContOrEnd token

parseEntityDeclares :: NetlistName -> EntityBuildStack
parseEntityDeclares entityName = do
   (scopeStore,unitStore) <- gets convertEntityToGenericUnit
   token <- lift getToken
   case token of
      _ | isKeywordProcedure token -> throwError $ ConverterError_NotImplemented $ passPosition "Procedure declaration" token
      _ | isKeywordFunction token -> throwError $ ConverterError_NotImplemented $ passPosition "Function declaration" token
      _ | isKeywordType token -> do
            (modType,modSubtype) <- lift $ parseType scopeStore unitStore entityName
            let insertEntityType entity =
                  entity
                     { entityTypes = modType $ entityTypes entity
                     , entitySubtypes = modSubtype $ entitySubtypes entity
                     }
            modify insertEntityType
      _ | isKeywordSubtype token -> do
         modSubtype <- lift $ parseSubtype scopeStore unitStore entityName
         let insertEntitySubtype entity = entity { entitySubtypes = modSubtype $ entitySubtypes entity }
         modify insertEntitySubtype
      _ | isKeywordConstant token -> do
            (constStore,modSubtype) <- lift $ parseConstant scopeStore unitStore entityName
            let insertEntityConsts entity =
                  entity
                     { entityConstants = MapS.union constStore $ entityConstants entity
                     , entitySubtypes = modSubtype $ entitySubtypes entity
                     }
            modify insertEntityConsts
      _ | isKeywordSignal token -> do
            (signalStore,modSubtype) <- lift $ parseSignal scopeStore unitStore entityName
            let insertEntitySignals entity =
                  entity
                     { entitySignals = MapS.union signalStore $ entitySignals entity
                     , entitySubtypes = modSubtype $ entitySubtypes entity
                     }
            modify insertEntitySignals
      _ | isKeywordFile token -> throwError $ ConverterError_NotImplemented $ passPosition "File declaration" token
      _ | isKeywordAlias token -> throwError $ ConverterError_NotImplemented $ passPosition "Alias declaration" token
      _ | isKeywordAttribute token -> throwError $ ConverterError_NotImplemented $ passPosition "Attribute declaration/specification" token
      _ | isKeywordDisconnect token -> throwError $ ConverterError_NotImplemented $ passPosition "Disconnection specification" token
      _ | isKeywordUse token -> throwError $ ConverterError_NotImplemented $ passPosition "Use" token
      _ | isKeywordBegin token || isKeywordEnd token -> lift $ saveToken token
      _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEntityDeclItemOrEnd token
   unless (isKeywordEnd token || isKeywordBegin token) $ parseEntityDeclares entityName

parseEntityStatements :: NetlistName -> EntityBuildStack
parseEntityStatements entityName = do
   (scopeStore,unitStore) <- gets convertEntityToGenericUnit
   token <- lift getToken
   case matchIdentifier token of
      Just (PosnWrapper pos label) -> do
         let upperLabel = PosnWrapper pos $ map toUpper label
         nextTok <- lift getToken
         case nextTok of
            token | isColon token -> do
               keywordTok <- lift getToken
               case keywordTok of
                  token | isKeywordAssert token -> throwError $ ConverterError_NotImplemented $ passPosition "Concurrent assertion statement" token
                  token | isIdentifier token -> throwError $ ConverterError_NotImplemented $ passPosition "Passive concurrent procedure call" token
                  token | isKeywordProcess token -> do
                     (label,newProcess) <- lift $ parseProcess scopeStore unitStore entityName True $ Just upperLabel
                     let insertProcess entity = entity { entityProcesses = MapS.insert label newProcess $ entityProcesses entity }
                     modify insertProcess
                  token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEntityStatementItem token
            _ | isLeftParen nextTok || isSemicolon nextTok -> do
               lift $ saveToken nextTok
               lift $ saveToken token
               throwError $ ConverterError_NotImplemented $ passPosition "Passive concurrent procedure call" token
            _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedColonInEntityStatement nextTok -- ?? This error could also be malformed procedure call with no label
      Nothing | isKeywordAssert token -> throwError $ ConverterError_NotImplemented $ passPosition "Concurrent assertion statement" token
      Nothing | isKeywordProcess token -> do
         (autoLabel,newProcess) <- lift $ parseProcess scopeStore unitStore entityName True Nothing
         let insertProcess entity = entity { entityProcesses = MapS.insert autoLabel newProcess $ entityProcesses entity }
         modify insertProcess
      Nothing | isKeywordEnd token -> return ()
      _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedEntityStatementItemOrEnd token
   unless (isKeywordEnd token) $ parseEntityStatements entityName
