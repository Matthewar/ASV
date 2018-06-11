module Parser.Functions.Parse.SequentialStatements
   ( parseWaitStatement
   , parseAssertionStatement
   , parseSensitivityList
   , parseSignalAssignment
   , parseIfStatement
   ) where

import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe
         ( isJust
         , fromJust
         )
import Control.Monad.Except (throwError)
import Control.Monad
         ( unless
         , when
         , replicateM
         )

import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper
         ( raisePosition
         , passPosition
         )
import Lexer.Types.Error (ParserError(..))
import Lexer.Alex.Types (AlexPosn)
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Types.Expressions
         ( AllTypes(..)
         , Staticity(..)
         )
import Parser.Functions.IdentifyToken
         ( matchIdentifier
         , isComma
         , isIdentifier
         , isKeywordAfter
         , isKeywordAssert
         , isKeywordCase
         , isKeywordElse
         , isKeywordElsif
         , isKeywordEnd
         , isKeywordExit
         , isKeywordFor
         , isKeywordIf
         , isKeywordNext
         , isKeywordNull
         , isKeywordOn
         , isKeywordReport
         , isKeywordReturn
         , isKeywordSeverity
         , isKeywordThen
         , isKeywordTransport
         , isKeywordUntil
         , isKeywordWait
         , isLeftParen
         , isSemicolon
         , isSignAssign
         )
import Parser.Functions.Parse.Expression
         ( parseExpression
         , parseWithExpectedType
         )
import Parser.Netlist.Types.Representation
         ( NetlistName(..)
         , SignalType(..)
         , Port(..)
         , Mode(..)
         , Value(..)
         , Calculation(..)
         , Enumerate(..)
         , Signal(..)
         , Waveform(..)
         , Subtype
         , SequentialStatement(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , UnitStore(..)
         , Package(..)
         )
import Parser.Netlist.Functions.Stores
         ( matchPortNameInScope
         , matchSignalNameInScope
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Netlist.Builtin.Standard (standardPackage)
import Manager.Types.Error (ConverterError(..))

parseWaitStatement :: ScopeStore -> UnitStore -> NetlistName -> ParserStack ([(SignalType,String)],Calculation,Maybe Calculation)
parseWaitStatement scope unit unitName = do
   onTok <- getToken
   initialSensitivity <- case onTok of
      token | isKeywordOn token -> do
         list <- parseSensitivityList scope unit unitName []
         return $ Just list
      token -> do
         saveToken token
         return Nothing
   untilTok <- getToken
   condition <- case untilTok of
      token | isKeywordUntil token -> do
         expressions <- parseExpression NotStatic scope unit unitName
         let findCondition (_,Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") _) = True
             findCondition _ = False
         case find findCondition expressions of
            Just (exp,_) -> return exp
            Nothing -> throwError $ ConverterError_Netlist $ passPosition NetlistError_NoConditionFound token
      token -> do
         saveToken token
         return $ Calc_Value (Value_Enum (NetlistName "STD" "STANDARD","ANON'BOOLEAN") $ Enum_Identifier "TRUE") $ Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") $ (packageTypes standardPackage) MapS.! "ANON'BOOLEAN"
   sensitivity <- case initialSensitivity of
      Nothing -> throwError $ ConverterError_NotImplemented $ passPosition "Auto sensitivity list" onTok -- searchCalcForSignals 
      Just list -> return list
   forTok <- getToken
   timeout <- case forTok of
      token | isKeywordFor token -> do
         timeExpressions <- parseExpression NotStatic scope unit unitName
         let findCondition (_,Type_Type (NetlistName "STD" "STANDARD","ANON'TIME") _) = True
             findCondition _ = False
         case find findCondition timeExpressions of
            Just (exp,_) -> return $ Just exp
            Nothing -> throwError $ ConverterError_Netlist $ passPosition NetlistError_NoTimeResultExpressionFound token
      token -> do
         saveToken token
         return Nothing
   endTok <- getToken
   unless (isSemicolon endTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInWaitStatement endTok
   return (sensitivity,condition,timeout)

parseAssertionStatement :: ScopeStore -> UnitStore -> NetlistName -> ParserStack (Calculation,Calculation,Calculation)
parseAssertionStatement scope unit unitName = do
   firstToken <- getToken
   saveToken firstToken -- ?? Done for position
   potentialConditions <- parseExpression NotStatic scope unit unitName
   let findCondition (_,Type_Type (NetlistName "STD" "STANDARD","ANON'BOOLEAN") _) = True
       findCondition _ = False
   condition <- case find findCondition potentialConditions of
      Just (exp,_) -> return exp
      Nothing -> throwError $ ConverterError_Netlist $ passPosition NetlistError_NoConditionFound firstToken
   let reportEnum :: Char -> Value
       reportEnum = (Value_Enum (NetlistName "STD" "STANDARD","ANON'CHARACTER")) . Enum_Char
       defaultReport = Calc_Value (Value_Array $ map reportEnum "Assertion violation.") $ Type_Type (NetlistName "STD" "STANDARD","ANON'STRING") $ (packageTypes standardPackage) MapS.! "ANON'STRING"
       defaultSeverity = Calc_Value (Value_Enum (NetlistName "STD" "STANDARD","ANON'SEVERITY_LEVEL") $ Enum_Identifier "ERROR") $ Type_Type (NetlistName "STD" "STANDARD","ANON'SEVERITY_LEVEL") $ (packageTypes standardPackage) MapS.! "ANON'SEVERITY_LEVEL"
   nextTok <- getToken
   case nextTok of
      token | isKeywordReport token -> throwError $ ConverterError_NotImplemented $ passPosition "String expressions" token
      token | isKeywordSeverity token -> do
         potentialSeverities <- parseExpression NotStatic scope unit unitName
         let findSeverity (_,Type_Type (NetlistName "STD" "STANDARD","ANON'SEVERITY_LEVEL") _) = True
             findSeverity _ = False
         severity <- case find findSeverity potentialSeverities of
            Just (calc,_) -> return calc
            Nothing -> throwError $ ConverterError_Netlist $ passPosition NetlistError_NoSeverityResultExpressionFound token
         endTok <- getToken
         unless (isSemicolon endTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInAssert endTok
         return (condition,defaultReport,severity)
      token | isSemicolon token -> return (condition,defaultReport,defaultSeverity)
      token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedReportOrSeverityOrEndInAssert token

parseSignalAssignment :: ScopeStore -> UnitStore -> NetlistName -> Signal -> ParserStack [Waveform]
parseSignalAssignment scope unit unitName signal = do
   [assignTok,transportTok] <- replicateM 2 getToken
   unless (isSignAssign assignTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSignalAssignInSignalAssignment assignTok
   case transportTok of
      token | isKeywordTransport token -> throwError $ ConverterError_NotImplemented $ passPosition "Transport signals" token
      token -> saveToken token
   waveforms <- parseWaveforms scope unit unitName (signal_typeName signal) $ signal_typeData signal
   finalTok <- getToken
   unless (isSemicolon finalTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInSignalAssignment finalTok
   return waveforms

parseIfStatement :: ScopeStore -> UnitStore -> NetlistName -> Bool -> Bool -> AlexPosn -> ParserStack ([(Calculation,[SequentialStatement])],[SequentialStatement])
parseIfStatement scope unit unitName isPassive waitAllowed ifPos = do
   condition <- parseWithExpectedType NotStatic parseExpression scope unit unitName (NetlistName "STD" "STANDARD","BOOLEAN") ((packageSubtypes standardPackage) MapS.! "BOOLEAN") ifPos
   endConditionTok <- getToken
   unless (isKeywordThen endConditionTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordThenInIfStatement endConditionTok
   ifStatements <- parseIfStatements []
   allStatements <- parseElsifStatements [(condition,ifStatements)]
   [ifTok,semiTok] <- replicateM 2 getToken
   unless (isKeywordIf ifTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordIfInEndIfStatement ifTok
   unless (isSemicolon semiTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInEndIfStatement semiTok
   return allStatements
   where parseIfStatements statements = do
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
               _ | isKeywordIf token -> do
                  (ifStatements,elseStatements) <- parseIfStatement scope unit unitName isPassive waitAllowed $ getPos token
                  return $ Just $ IfStatement ifStatements elseStatements
               _ | isKeywordCase token -> throwError $ ConverterError_NotImplemented $ passPosition "Case statement" token
               --loop statement -- [ identifier : ] [ while or for ] loop
               _ | isKeywordNext token -> throwError $ ConverterError_NotImplemented $ passPosition "Loop statement" token
               _ | isKeywordExit token -> throwError $ ConverterError_NotImplemented $ passPosition "Exit statement" token
               _ | isKeywordReturn token -> throwError $ ConverterError_NotImplemented $ passPosition "Return statement" token
               _ | isKeywordNull token -> return $ Just NullStatement
               _ | isKeywordElsif token || isKeywordElse token || isKeywordEnd token -> do
                  saveToken token
                  return Nothing
            case newStatement of
               Just statement -> parseIfStatements (statement:statements)
               Nothing -> return $ reverse statements
         parseElsifStatements elsifs = do
            firstToken <- getToken
            case firstToken of
               token | isKeywordElsif token -> do
                  condition <- parseWithExpectedType NotStatic parseExpression scope unit unitName (NetlistName "STD" "STANDARD","BOOLEAN") ((packageSubtypes standardPackage) MapS.! "BOOLEAN") ifPos
                  endConditionTok <- getToken
                  unless (isKeywordThen endConditionTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordThenInIfStatement endConditionTok
                  ifStatements <- parseIfStatements []
                  parseElsifStatements ((condition,ifStatements):elsifs)
               token | isKeywordElse token -> do
                  elseStatements <- parseElseStatements []
                  return (reverse elsifs,elseStatements)
               token | isKeywordEnd token -> return (reverse elsifs,[])
               token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedIfStatmentContinuation token
         parseElseStatements statements = do
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
               _ | isKeywordIf token -> do
                  (ifStatements,elseStatements) <- parseIfStatement scope unit unitName isPassive waitAllowed $ getPos token
                  return $ Just $ IfStatement ifStatements elseStatements
               _ | isKeywordCase token -> throwError $ ConverterError_NotImplemented $ passPosition "Case statement" token
               --loop statement -- [ identifier : ] [ while or for ] loop
               _ | isKeywordNext token -> throwError $ ConverterError_NotImplemented $ passPosition "Loop statement" token
               _ | isKeywordExit token -> throwError $ ConverterError_NotImplemented $ passPosition "Exit statement" token
               _ | isKeywordReturn token -> throwError $ ConverterError_NotImplemented $ passPosition "Return statement" token
               _ | isKeywordNull token -> return $ Just NullStatement
               _ | isKeywordEnd token -> return Nothing
            case newStatement of
               Just statement -> parseElseStatements (statement:statements)
               Nothing -> return $ reverse statements

parseSensitivityList :: ScopeStore -> UnitStore -> NetlistName -> [(SignalType,String)] -> ParserStack [(SignalType,String)]
parseSensitivityList scope unit unitName currentList = do
   signalTok <- getToken
   signalName <- case matchIdentifier signalTok of
                  Just (PosnWrapper _ name) -> return $ map toUpper name
                  _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSignalNameInSensitivityList signalTok
   signalType <- if MapS.member signalName $ unitSignals unit
                  then return InternalSignal
                  else let ports = unitPorts unit
                           foundPort = find (\(Port name _ _ _ _) -> name == signalName) ports
                       in case foundPort of
                           Nothing -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_ExpectedSignalNameInSensitivityList signalName) signalTok
                           Just port | Mode_In == port_mode port -> return PortIn
                           Just port | Mode_Out == port_mode port -> throwError $ ConverterError_Netlist $ passPosition NetlistError_ExpectedReadableSignalInSensitivityList signalTok
                           -- ?? Other modes
   let signalData = (signalType,signalName)
       newList = (signalData:currentList) -- ?? Check for duplicate names
   nextTok <- getToken
   case nextTok of
      token | isComma token -> parseSensitivityList scope unit unitName newList
      token -> do
         saveToken token
         return newList

parseWaveforms :: ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> ParserStack [Waveform]
parseWaveforms scope unit unitName subtypeName subtype = do
   waveforms <- parseWaveforms' scope unit unitName subtypeName subtype []
   case waveforms of
      [] -> do
         fstTok <- getToken
         throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedNullOrValueExpressionInWaveform fstTok -- ?? Does this occur
      nonEmpty -> return nonEmpty

parseWaveforms' :: ScopeStore -> UnitStore -> NetlistName -> (NetlistName,String) -> Subtype -> [Waveform] -> ParserStack [Waveform]
parseWaveforms' scope unit unitName subtypeName subtype waveforms = do
   fstTok <- getToken
   newWaveform <- case fstTok of
      token | isKeywordNull token -> throwError $ ConverterError_NotImplemented $ passPosition "Null waveform element" token
      token -> do
         saveToken token
         value <- parseWithExpectedType NotStatic parseExpression scope unit unitName subtypeName subtype $ getPos token
         return $ ValueWaveform value
   nextTok <- getToken
   time <- case nextTok of
      token | isKeywordAfter token -> parseWithExpectedType NotStatic parseExpression scope unit unitName (NetlistName "STD" "STANDARD","TIME") ((packageSubtypes standardPackage) MapS.! "TIME") $ getPos token
      token -> do
         saveToken token
         return $ Calc_Value (Value_Physical 0) $ Type_Type (NetlistName "STD" "STANDARD","ANON'TIME") $ (packageTypes standardPackage) MapS.! "ANON'TIME"
   let waveform = newWaveform time
   finalTok <- getToken
   let newWaveforms = (waveform:waveforms)
   case finalTok of
      token | isComma token -> parseWaveforms' scope unit unitName subtypeName subtype newWaveforms
      token -> do
         saveToken token
         return $ reverse newWaveforms
