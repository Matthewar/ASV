module Parser.Functions.Parse.SequentialStatements
   ( parseWaitStatement
   , parseAssertionStatement
   , parseSensitivityList
   , parseSignalAssignment
   ) where

import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Data.List (find)
import Control.Monad.Except (throwError)
import Control.Monad
         ( unless
         , replicateM
         )

import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper
         ( raisePosition
         , passPosition
         )
import Lexer.Types.Error (ParserError(..))
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
         , isKeywordAfter
         , isKeywordFor
         , isKeywordNull
         , isKeywordOn
         , isKeywordReport
         , isKeywordSeverity
         , isKeywordTransport
         , isKeywordUntil
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
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , UnitStore(..)
         , Package(..)
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
