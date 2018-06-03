module Parser.Functions.Parse.Process
   ( parseProcess
   ) where

import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Data.Maybe
         ( fromJust
         , isNothing
         , isJust
         )
import Control.Monad.Trans.State
         ( StateT
         , modify
         , gets
         , execStateT
         )
import Control.Monad.Except
         ( ExceptT
         , throwError
         , lift
         )
import Control.Monad
         ( replicateM
         , unless
         , when
         )

import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper
         ( raisePosition
         , passPosition
         )
import Lexer.Types.Error (ParserError(..))
import Lexer.Alex.Types (AlexState)
import Parser.Types.Monad
         ( ParserStack
         , ParserState
         )
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Functions.IdentifyToken
         ( matchIdentifier
         , isIdentifier
         , isKeywordAlias
         , isKeywordAttribute
         , isKeywordAssert
         , isKeywordBegin
         , isKeywordCase
         , isKeywordConstant
         , isKeywordEnd
         , isKeywordExit
         , isKeywordFile
         , isKeywordFunction
         , isKeywordIf
         , isKeywordNext
         , isKeywordNull
         , isKeywordProcedure
         , isKeywordProcess
         , isKeywordReturn
         , isKeywordSubtype
         , isKeywordType
         , isKeywordUse
         , isKeywordVariable
         , isKeywordWait
         , isLeftParen
         , isRightParen
         , isSemicolon
         )
import Parser.Functions.Parse.Type (parseType)
import Parser.Functions.Parse.Subtype (parseSubtype)
import Parser.Functions.Parse.Objects (parseConstant)
import Parser.Functions.Parse.SequentialStatements
         ( parseWaitStatement
         , parseAssertionStatement
         , parseSensitivityList
         , parseSignalAssignment
         )
import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , SequentialStatement(..)
         , Value(..)
         , Calculation(..)
         , Enumerate(..)
         , AllTypes(..)
         , Signal(..)
         , SignalType(..)
         , Port(..)
         , Mode(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , UnitStore(..)
         , NetlistStore
         , Process(..)
         , emptyProcess
         , ProcessStore
         , Package(..)
         )
import Parser.Netlist.Functions.Stores
         ( convertProcessToGenericUnit
         , mergeUnits
         , matchSignalNameInScope
         , matchPortNameInScope
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Netlist.Builtin.Standard (standardPackage)
import Manager.Types.Error (ConverterError(..))

parseProcess :: ScopeStore -> UnitStore -> NetlistName -> Bool -> Maybe (PosnWrapper String) -> ParserStack (String,Process)
parseProcess scope unit unitName isPassive potentialLabel = do
   label <- case potentialLabel of
      Just (PosnWrapper _ label) -> return label
      Nothing -> let value = MapS.size $ unitProcesses unit
                 in return $ "PROCESS'ANON'" ++ show value
   fstTok <- getToken
   (isSensitive,sensitivityList) <- case fstTok of
                                       token | isLeftParen token -> do
                                          list <- parseSensitivityList scope unit unitName []
                                          endTok <- getToken
                                          unless (isRightParen endTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedRightParenToEndProcessSensitivityList endTok
                                          return (True,list)
                                       token -> do
                                          saveToken token
                                          return (False,[])
   initialProcess <- execStateT (parseProcessDeclares scope unit unitName) emptyProcess -- ?? Need check for name interfering with label
   let processUnit = convertProcessToGenericUnit initialProcess
       newUnitStore = mergeUnits processUnit unit
   sequentialStatements <- parseSequentialStatements scope newUnitStore unitName isPassive (not isSensitive) []
   [processTok,potLabelTok,potEndTok] <- replicateM 3 getToken
   case (processTok,matchIdentifier potLabelTok,potEndTok) of
      (token,_,_) | not $ isKeywordProcess token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordProcessInEndProcess token
      (_,Just _,token) | not $ isSemicolon token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInEndProcess token
      (_,Nothing,_) | not $ isSemicolon potLabelTok -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInEndProcess potLabelTok
      (_,Just _,_) | isNothing potentialLabel -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedNoLabelInEndProcess potLabelTok
      (_,Just (PosnWrapper pos2 label2),_) ->
         let upperLabel2 = map toUpper label2
         in if upperLabel2 == label
               then return ()
               else throwError $ ConverterError_Parse $ passPosition (ParseErr_ProcessLabelsNoMatch (fromJust potentialLabel) $ PosnWrapper pos2 upperLabel2) potLabelTok
      (_,Nothing,_) -> saveToken potEndTok
   let finalStatement = WaitStatement
                           sensitivityList
                           (Calc_Value (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") $ Enum_Identifier "TRUE") $ Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") $ (packageTypes standardPackage) MapS.! "ANON'BOOLEAN")
                           Nothing
       allStatements = if isSensitive
                        then sequentialStatements ++ [finalStatement]
                        else sequentialStatements
   return (label,initialProcess { processStatements = allStatements })

-- |Monad stack for process building
type ProcessBuildStack = StateT Process (StateT ParserState (StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO)))) ()

parseProcessDeclares :: ScopeStore -> UnitStore -> NetlistName -> ProcessBuildStack
parseProcessDeclares scopeStore baseUnit processName = do
   processUnit <- gets convertProcessToGenericUnit
   let unitStore = mergeUnits processUnit baseUnit
   token <- lift getToken
   case token of
      _ | isKeywordProcedure token -> throwError $ ConverterError_NotImplemented $ passPosition "Procedure declaration" token
      _ | isKeywordFunction token -> throwError $ ConverterError_NotImplemented $ passPosition "Function declaration" token
      _ | isKeywordType token -> do
         (modType,modSubtype) <- lift $ parseType scopeStore unitStore processName
         let insertProcessType process =
               process
                  { processTypes = modType $ processTypes process
                  , processSubtypes = modSubtype $ processSubtypes process
                  }
         modify insertProcessType
      _ | isKeywordSubtype token -> do
         modSubtype <- lift $ parseSubtype scopeStore unitStore processName
         let insertProcessSubtype process = process { processSubtypes = modSubtype $ processSubtypes process }
         modify insertProcessSubtype
      _ | isKeywordConstant token -> do
         (constStore,modSubtype) <- lift $ parseConstant scopeStore unitStore processName
         let insertProcessConsts process =
               process
                  { processConstants = MapS.union constStore $ processConstants process
                  , processSubtypes = modSubtype $ processSubtypes process
                  }
         modify insertProcessConsts
      _ | isKeywordVariable token -> throwError $ ConverterError_NotImplemented $ passPosition "Variable declaration" token
      _ | isKeywordFile token -> throwError $ ConverterError_NotImplemented $ passPosition "File declaration" token
      _ | isKeywordAlias token -> throwError $ ConverterError_NotImplemented $ passPosition "Alias declaration" token
      _ | isKeywordAttribute token -> throwError $ ConverterError_NotImplemented $ passPosition "Attribute declaration/specification" token
      _ | isKeywordUse token -> throwError $ ConverterError_NotImplemented $ passPosition "Use" token
      _ | isKeywordBegin token -> return ()
      _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedProcessDeclItemOrEnd token
   unless (isKeywordBegin token) $ parseProcessDeclares scopeStore baseUnit processName

parseSequentialStatements :: ScopeStore -> UnitStore -> NetlistName -> Bool -> Bool -> [SequentialStatement] -> ParserStack [SequentialStatement]
parseSequentialStatements scope unit unitName isPassive waitAllowed seqStatements = do
   token <- getToken
   newStatement <- case token of
      _ | isKeywordWait token && waitAllowed -> do
         (sensitivity,condition,timeout) <- parseWaitStatement scope unit unitName
         return $ Just $ WaitStatement sensitivity condition timeout
      _ | isKeywordWait token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_WaitSeqStatementNotAllowedWithSensitivityList token
      _ | isKeywordAssert token -> do
         (condition,report,severity) <- parseAssertionStatement scope unit unitName
         return $ Just $ AssertStatement condition report severity
      _ | isIdentifier token ->
         let name = map toUpper $ unPos $ fromJust $ matchIdentifier token
             checkSignals = case matchSignalNameInScope scope unit name of
               Just _ | isPassive -> throwError $ ConverterError_Netlist $ passPosition NetlistError_SignalAssignmentInPassiveProcess token
               Just (signal,signalPackage) -> do
                  when (isJust signalPackage) $ throwError $ ConverterError_Netlist $ passPosition NetlistError_PackageSignalInSignalAssign token
                  calc <- parseSignalAssignment scope unit unitName signal
                  return $ Just $ SignalAssignStatement name InternalSignal calc
               Nothing -> checkPorts
             checkPorts = case matchPortNameInScope unit name of
               Just port -> do
                  portType <- case port_mode port of
                     Mode_In -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_CannotAssignToInputPort name) token
                     Mode_Out -> return PortOut
                     _ -> throwError $ ConverterError_NotImplemented $ passPosition "Other port mode types in assign" token
                  let portAsSignal = Signal (port_subtypeName port) (port_subtypeData port) (port_default port)
                  calc <- parseSignalAssignment scope unit unitName portAsSignal
                  return $ Just $ SignalAssignStatement name portType calc
               Nothing -> throwError $ ConverterError_NotImplemented $ passPosition "procedure call or variable assignment" token
         in checkSignals
      _ | isLeftParen token -> throwError $ ConverterError_NotImplemented $ passPosition "Signal/variable assignment (aggregate)" token
      _ | isKeywordIf token -> throwError $ ConverterError_NotImplemented $ passPosition "If statement" token
      _ | isKeywordCase token -> throwError $ ConverterError_NotImplemented $ passPosition "Case statement" token
      --loop statement -- [ identifier : ] [ while or for ] loop
      _ | isKeywordNext token -> throwError $ ConverterError_NotImplemented $ passPosition "Loop statement" token
      _ | isKeywordExit token -> throwError $ ConverterError_NotImplemented $ passPosition "Exit statement" token
      _ | isKeywordReturn token -> throwError $ ConverterError_NotImplemented $ passPosition "Return statement" token
      _ | isKeywordNull token -> return $ Just NullStatement
      _ | isKeywordEnd token -> return Nothing
   let newSeqStatements = case newStatement of
                           Just statement -> (statement:seqStatements)
                           Nothing -> reverse seqStatements
   if isKeywordEnd token
      then return newSeqStatements
      else parseSequentialStatements scope unit unitName isPassive waitAllowed newSeqStatements
