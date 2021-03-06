module Sim.Builtin.ControlModule
   ( controlModule
   ) where

import Data.Text

controlModule :: Text
controlModule = pack
   "module Control\n\
   \   ( Control'Time(..)\n\
   \   , initialTime\n\
   \   , maxWaitTime\n\
   \   , Entity'State(..)\n\
   \   , Entity'Stack\n\
   \   , ProcessStatement(..)\n\
   \   , progressProcess\n\
   \   , progressComponent\n\
   \   , processTime\n\
   \   , Control'Signal(..)\n\
   \   , control'printInitial\n\
   \   , control'updateSignal\n\
   \   , control'readSignal\n\
   \   , control'updateSignal'inertial\n\
   \   , control'delayCheck\n\
   \   , Control'Stack\n\
   \   --, control'jumpTime\n\
   \   , Control'SEVERITY_TRACKER(..)\n\
   \   , Control'SEVERITY_FAILURE(..)\n\
   \   , control'assertNote\n\
   \   , control'assertWarning\n\
   \   , control'assertError\n\
   \   , control'assertFailure\n\
   \   ) where\n\
   \\n\
   \import System.Exit (exitFailure)\n\
   \import Control.Monad.Trans.State (StateT)\n\
   \import Control.Monad.Except\n\
   \         ( ExceptT\n\
   \         , throwError\n\
   \         , liftIO\n\
   \         , when\n\
   \         )\n\
   \import Data.Int (Int64)\n\
   \import Data.List\n\
   \         ( sortOn\n\
   \         , nubBy\n\
   \         )\n\
   \import qualified Data.Map.Strict as MapS\n\
   \\n\
   \import qualified STD.STANDARD\n\
   \\n\
   \data Control'Time =\n\
   \   Control'Time\n\
   \      { time'Real :: STD.STANDARD.Type'ANON'TIME\n\
   \      , time'Delta :: Int\n\
   \      }\n\
   \   deriving (Eq)\n\
   \instance Ord Control'Time where\n\
   \   compare val1 val2\n\
   \      | time'Real val1 < time'Real val2 = LT\n\
   \      | time'Real val1 > time'Real val2 = GT\n\
   \      | time'Real val1 == time'Real val2 = compare (time'Delta val1) (time'Delta val2)\n\
   \instance STD.STANDARD.SignalOutput Control'Time where\n\
   \   sigOut (Control'Time real _) = STD.STANDARD.sigOut real\n\
   \\n\
   \initialTime :: Control'Time\n\
   \initialTime = Control'Time (STD.STANDARD.mkType'ANON'TIME 0) 0\n\
   \\n\
   \maxWaitTime :: STD.STANDARD.Type'ANON'TIME\n\
   \maxWaitTime = STD.STANDARD.mkType'ANON'TIME $ toInteger (maxBound :: Int64)\n\
   \\n\
   \data Entity'State portsInType stateType portsOutType =\n\
   \   Entity'State\n\
   \      { entity'portsIn :: portsInType\n\
   \      , entity'state :: stateType\n\
   \      , entity'portsOut :: portsOutType\n\
   \      }\n\
   \\n\
   \-- |Entity stack\n\
   \type Entity'Stack a b = StateT a (StateT Control'Time (StateT Control'SEVERITY_TRACKER (ExceptT Control'SEVERITY_FAILURE IO))) b\n\
   \\n\
   \data ProcessStatement a b c =\n\
   \   Control'Wait\n\
   \      (Entity'State a b c -> Bool) -- Sensitivity List\n\
   \      (Entity'State a b c -> Control'Time -> STD.STANDARD.Type'ANON'BOOLEAN) -- Condition\n\
   \      (Either\n\
   \         STD.STANDARD.Type'ANON'TIME -- Calculated wait time\n\
   \         (Entity'State a b c -> Control'Time -> Maybe STD.STANDARD.Type'ANON'TIME) -- Calculate new wait time\n\
   \      )\n\
   \   | Control'If\n\
   \      (Entity'State a b c -> Control'Time -> STD.STANDARD.Type'ANON'BOOLEAN) -- Condition\n\
   \      [ProcessStatement a b c] -- If statements\n\
   \      [ProcessStatement a b c] -- Else statements\n\
   \   | Control'Assert\n\
   \      String -- Component name\n\
   \      (Entity'State a b c -> Control'Time -> STD.STANDARD.Type'ANON'BOOLEAN) -- Condition\n\
   \      (Entity'State a b c -> Control'Time -> STD.STANDARD.Type'ANON'STRING) -- Report\n\
   \      (Entity'State a b c -> Control'Time -> STD.STANDARD.Type'ANON'SEVERITY_LEVEL) -- Severity\n\
   \   | Control'Statement (Entity'State a b c -> Control'Time -> Entity'State a b c)\n\
   \   | Control'End\n\
   \\n\
   \progressProcess :: Entity'State a b c -> Control'Time -> [ProcessStatement a b c] -> Control'SEVERITY_TRACKER -> IO (Entity'State a b c,[ProcessStatement a b c],Control'SEVERITY_TRACKER)\n\
   \progressProcess currentState currentTime all@(Control'Wait sensitivityCheck conditionCheck (Left waitTime):others) severity\n\
   \   | time'Real currentTime == waitTime = progressProcess currentState currentTime others severity\n\
   \   | (sensitivityCheck currentState) && (conditionCheck currentState currentTime) == STD.STANDARD.Type'ANON'BOOLEAN'Iden'TRUE =\n\
   \      progressProcess currentState currentTime others severity\n\
   \   | otherwise = return (currentState,all,severity)\n\
   \progressProcess currentState currentTime (Control'Wait sensitivityCheck conditionCheck (Right modifyTime):others) severity =\n\
   \   let newTime = case modifyTime currentState currentTime of\n\
   \                  Just delayTime -> STD.STANDARD.function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME (time'Real currentTime) (control'delayCheck delayTime)\n\
   \                  Nothing -> maxWaitTime\n\
   \   in return (currentState,(Control'Wait sensitivityCheck conditionCheck $ Left newTime):others,severity)\n\
   \progressProcess currentState currentTime (Control'If conditionCheck trueStatements falseStatements:others) severity\n\
   \   | conditionCheck currentState currentTime == STD.STANDARD.Type'ANON'BOOLEAN'Iden'TRUE = progressProcess currentState currentTime (trueStatements ++ others) severity\n\
   \   | otherwise = progressProcess currentState currentTime (falseStatements ++ others) severity\n\
   \progressProcess currentState currentTime (Control'Assert moduleName condition report severity:others) (Control'SEVERITY_TRACKER warnings errors) =\n\
   \   let reportStr = report currentState currentTime\n\
   \   in if (condition currentState currentTime == STD.STANDARD.Type'ANON'BOOLEAN'Iden'TRUE)\n\
   \         then case severity currentState currentTime of\n\
   \                 STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'NOTE -> do\n\
   \                    control'assertNote moduleName reportStr\n\
   \                    progressProcess currentState currentTime others (Control'SEVERITY_TRACKER warnings errors)\n\
   \                 STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'WARNING -> do\n\
   \                    control'assertWarning moduleName reportStr\n\
   \                    progressProcess currentState currentTime others (Control'SEVERITY_TRACKER (warnings+1) errors)\n\
   \                 STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'ERROR -> do\n\
   \                    control'assertError moduleName reportStr\n\
   \                    progressProcess currentState currentTime others (Control'SEVERITY_TRACKER warnings (errors+1))\n\
   \                 STD.STANDARD.Type'ANON'SEVERITY_LEVEL'Iden'FAILURE -> do\n\
   \                    control'assertFailure moduleName reportStr\n\
   \                    exitFailure\n\
   \         else progressProcess currentState currentTime others (Control'SEVERITY_TRACKER warnings errors)\n\
   \progressProcess currentState currentTime (Control'Statement modifyState:others) severity = progressProcess (modifyState currentState currentTime) currentTime others severity\n\
   \progressProcess currentState _ (Control'End:others) severity = return (currentState,others,severity)\n\
   \\n\
   \progressComponent :: Entity'State a b c -> Control'Time -> [[ProcessStatement a b c]] -> [[ProcessStatement a b c]] -> Control'SEVERITY_TRACKER -> IO (Entity'State a b c,[[ProcessStatement a b c]],Control'SEVERITY_TRACKER)\n\
   \progressComponent currentState currentTime (process:processes) runProcesses severity = do\n\
   \   (newState,runProcess,newSeverity) <- progressProcess currentState currentTime process severity\n\
   \   progressComponent newState currentTime processes (runProcess:runProcesses) newSeverity\n\
   \progressComponent finalState _ [] runProcesses severity = return (finalState,reverse runProcesses,severity)\n\
   \\n\
   \processTime :: [[ProcessStatement a b c]] -> Maybe (Either () STD.STANDARD.Type'ANON'TIME)\n\
   \processTime processes =\n\
   \   processTime' processes Nothing\n\
   \   where processTime' (process:processes) Nothing = processTime' processes $ processTime'' process\n\
   \         processTime' (process:processes) (Just (Left ())) = Just $ Left ()\n\
   \         processTime' (process:processes) (Just (Right time)) =\n\
   \            let newTime = processTime'' process\n\
   \            in processTime' processes $ case newTime of\n\
   \                                          Just (Left ()) -> Just $ Left ()\n\
   \                                          Just (Right newTime) | newTime < time -> Just $ Right newTime\n\
   \                                          _ -> Just $ Right time\n\
   \         processTime' [] time = time\n\
   \         processTime'' (Control'Wait _ _ (Left time):_) = Just $ Right $ time\n\
   \         processTime'' _ = Just $ Left ()\n\
   \\n\
   \data Control'Signal a =\n\
   \   Control'Signal\n\
   \      --{ control'signal'current :: (Control'Time,a)\n\
   \      { control'signal'transactions :: [(Control'Time,a)]\n\
   \      , control'signal'active :: Bool\n\
   \      , control'signal'event :: Bool\n\
   \      }\n\
   \\n\
   \control'printInitial :: STD.STANDARD.SignalOutput a => String -> Control'Time -> Control'Signal a -> IO ()\n\
   \control'printInitial signalName currentTime (Control'Signal [(_,val)] _ _) =\n\
   \   putStrLn $ \"SignalUpdate: \" ++ STD.STANDARD.sigOut currentTime ++ \": \" ++ signalName ++ \": \" ++ STD.STANDARD.sigOut val\n\
   \\n\
   \control'updateSignal :: (Eq a, STD.STANDARD.SignalOutput a) => String -> Control'Time -> Control'Signal a -> IO (Control'Signal a)\n\
   \control'updateSignal signalName currentTime (Control'Signal originalTrans@((_,oldVal):(newTime,newVal):transactions) _ _)\n\
   \   | currentTime == newTime = do\n\
   \      putStrLn $ \"SignalUpdate: \" ++ STD.STANDARD.sigOut currentTime ++ \": \" ++ signalName ++ \": \" ++ STD.STANDARD.sigOut newVal\n\
   \      let event = oldVal /= newVal\n\
   \      return (Control'Signal ((newTime,newVal):transactions) True event)\n\
   \   | otherwise = return (Control'Signal originalTrans False False)\n\
   \control'updateSignal _ _ (Control'Signal trans _ _) = return (Control'Signal trans False False)\n\
   \\n\
   \control'readSignal :: Control'Signal a -> a\n\
   \control'readSignal = snd . head . control'signal'transactions\n\
   \\n\
   \control'updateSignal'inertial :: Eq a => Control'Time -> [(STD.STANDARD.Type'ANON'TIME,a)] -> Control'Signal a -> Control'Signal a\n\
   \control'updateSignal'inertial currentTime waveforms signal =\n\
   \   let ((firstTime,firstValue):oldWaveforms) = control'signal'transactions signal\n\
   \       toAddWaveforms = map updateWaveformTimes $ sortOn fst waveforms\n\
   \       mergedWaveforms = mergeWaveforms oldWaveforms toAddWaveforms [(firstTime,firstValue,True)]\n\
   \       newWaveforms = filterWaveforms mergedWaveforms []\n\
   \   in if length (nubBy (\\a b -> fst a == fst b) waveforms) == length waveforms\n\
   \         then signal { control'signal'transactions = newWaveforms }\n\
   \         else error \"Waveforms with duplicate times in signal assignment\"\n\
   \   where updateWaveformTimes (time,val)\n\
   \            | time < STD.STANDARD.mkType'ANON'TIME 0 = error \"Time delay in signal assignment must be greater than 0\"\n\
   \            | time == STD.STANDARD.mkType'ANON'TIME 0 = (Control'Time (time'Real currentTime) (1 + time'Delta currentTime),val)\n\
   \            | otherwise =\n\
   \               ( Control'Time\n\
   \                  (STD.STANDARD.function'op'PLUS'in'STD'STANDARD'Type'ANON'TIME'_'STD'STANDARD'Type'ANON'TIME'out'STD'STANDARD'Type'ANON'TIME (time'Real currentTime) time)\n\
   \                  0\n\
   \               , val\n\
   \               )\n\
   \         mergeWaveforms :: [(Control'Time,a)] -> [(Control'Time,a)] -> [(Control'Time,a,Bool)] -> [(Control'Time,a,Bool)]\n\
   \         mergeWaveforms allOldWaves@((oldTime,oldVal):oldWaves) allAddWaves@((toAddTime,toAddVal):toAddWaves) newWaves\n\
   \            | oldTime <= toAddTime = mergeWaveforms oldWaves allAddWaves ((oldTime,oldVal,False):newWaves)\n\
   \            | otherwise = mergeWaveforms allOldWaves toAddWaves ((toAddTime,toAddVal,True):newWaves)\n\
   \         mergeWaveforms [] ((toAddTime,toAddVal):toAddWaves) newWaves = mergeWaveforms [] toAddWaves ((toAddTime,toAddVal,True):newWaves)\n\
   \         mergeWaveforms (_:_) [] newWaves = newWaves\n\
   \         mergeWaveforms [] [] newWaves = newWaves\n\
   \         filterWaveforms :: Eq a => [(Control'Time,a,Bool)] -> [(Control'Time,a)] -> [(Control'Time,a)]\n\
   \         filterWaveforms ((time1,val1,True):(time2,val2,False):others) filteredWaves\n\
   \            | val1 == val2 = filterWaveforms ((time2,val2,True):others) ((time1,val1):filteredWaves)\n\
   \            | otherwise = filterWaveforms others ((time1,val1):filteredWaves)\n\
   \         filterWaveforms ((time,val,True):others) filteredWaves = filterWaveforms others ((time,val):filteredWaves)\n\
   \         filterWaveforms ((_,_,False):others) filteredWaves = filterWaveforms others filteredWaves\n\
   \         filterWaveforms [] filteredWaves = filteredWaves\n\
   \\n\
   \control'delayCheck :: STD.STANDARD.Type'ANON'TIME -> STD.STANDARD.Type'ANON'TIME\n\
   \control'delayCheck time\n\
   \   | time >= STD.STANDARD.mkType'ANON'TIME 0 = time\n\
   \   | otherwise = error \"Negative time in delay\"\n\
   \\n\
   \-- |Monad stack\n\
   \-- Access to IO for file access/stdin/stdout\n\
   \type Control'Stack a = StateT Control'Time (StateT Control'SEVERITY_TRACKER (ExceptT Control'SEVERITY_FAILURE IO)) a\n\
   \\n\
   \--control'jumpTime :: STD.STANDARD.Type'ANON'TIME -> Control'Stack ()\n\
   \--control'jumpTime time =\n\
   \--   let incDelta curTime =\n\
   \--         curTime\n\
   \--            { time'Real = time\n\
   \--            , time'Delta = 1 + (time'Delta curTime)\n\
   \--            }\n\
   \--   in modify incDelta\n\
   \\n\
   \-- |Track number of non-fatal asserts that have occurred\n\
   \data Control'SEVERITY_TRACKER =\n\
   \   Control'SEVERITY_TRACKER\n\
   \      { control'warnings :: Int\n\
   \      , control'errors :: Int\n\
   \      }\n\
   \instance Show Control'SEVERITY_TRACKER where\n\
   \   show (Control'SEVERITY_TRACKER warnings errors) =\n\
   \      \"Simulation completed with \" ++ show warnings ++ \" warnings and \" ++ show errors ++ \" errors.\"\n\
   \\n\
   \-- |Fail on SEVERITY_LEVEL'IDEN'FAILURE assert\n\
   \data Control'SEVERITY_FAILURE = Control'SEVERITY_FAILURE String\n\
   \\n\
   \control'assertNote :: String -> STD.STANDARD.Type'ANON'STRING -> IO ()\n\
   \control'assertNote unitName message = control'assert unitName $ \"NOTE: \" ++ anon'stringToString message\n\
   \\n\
   \control'assertWarning :: String -> STD.STANDARD.Type'ANON'STRING -> IO ()\n\
   \control'assertWarning unitName message = control'assert unitName $ \"WARNING: \" ++ anon'stringToString message\n\
   \\n\
   \control'assertError :: String -> STD.STANDARD.Type'ANON'STRING -> IO ()\n\
   \control'assertError unitName message = control'assert unitName $ \"ERROR: \" ++ anon'stringToString message\n\
   \\n\
   \control'assertFailure :: String -> STD.STANDARD.Type'ANON'STRING -> IO ()\n\
   \control'assertFailure unitName message = control'assert unitName $ \"FAILURE: \" ++ anon'stringToString message\n\
   \\n\
   \control'assert :: String -> String -> IO ()\n\
   \control'assert unitName message =\n\
   \   let completeMessage = \"ASSERT: \" ++ unitName ++ \": \" ++ message\n\
   \   in putStrLn completeMessage\n\
   \\n\
   \anon'stringToString :: STD.STANDARD.Type'ANON'STRING -> String -- ?? Need to add array extract function\n\
   \anon'stringToString (STD.STANDARD.Type'ANON'STRING str) = ((map anon'characterToChar) . MapS.elems) str\n\
   \\n\
   \anon'characterToChar :: STD.STANDARD.Type'ANON'CHARACTER -> Char\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'SPACE = ' '\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'EXCLAMATION = '!'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'QUOTE = '\"'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'HASH = '#'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'DOLLAR = '$'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'PERCENT = '%'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'AMPERSAND = '&'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'TICK = '\\''\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTPAREN = '('\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTPAREN = ')'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'STAR = '*'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'PLUS = '+'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'COMMA = ','\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'HYPHEN = '-'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'PERIOD = '.'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'FORWARDSLASH = '/'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'0 = '0'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'1 = '1'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'2 = '2'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'3 = '3'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'4 = '4'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'5 = '5'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'6 = '6'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'7 = '7'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'8 = '8'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'9 = '9'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'COLON = ':'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'SEMICOLON = ';'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'LESSTHAN = '<'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'EQUAL = '='\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'GREATERTHAN = '>'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'QUESTION = '?'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'AT = '@'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'A = 'A'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'B = 'B'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'C = 'C'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'D = 'D'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'E = 'E'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'F = 'F'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'G = 'G'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'H = 'H'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'I = 'I'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'J = 'J'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'K = 'K'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'L = 'L'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'M = 'M'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'N = 'N'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'O = 'O'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'P = 'P'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'Q = 'Q'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'R = 'R'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'S = 'S'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'T = 'T'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'U = 'U'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'V = 'V'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'W = 'W'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'X = 'X'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'Y = 'Y'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'Z = 'Z'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTSQUAREBRACE = '['\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'BACKSLASH = '\\\\'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTSQUAREBRACE = ']'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'CAROT = '^'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'UNDERSCORE = '_'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'BACKTICK = '`'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'a = 'a'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'b = 'b'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'c = 'c'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'd = 'd'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'e = 'e'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'f = 'f'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'g = 'g'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'h = 'h'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'i = 'i'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'j = 'j'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'k = 'k'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'l = 'l'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'm = 'm'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'n = 'n'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'o = 'o'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'p = 'p'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'q = 'q'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'r = 'r'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char's = 's'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char't = 't'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'u = 'u'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'v = 'v'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'w = 'w'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'x = 'x'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'y = 'y'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'z = 'z'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'LEFTBRACE = '{'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'BAR = '|'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'RIGHTBRACE = '}'\n\
   \anon'characterToChar STD.STANDARD.Type'ANON'CHARACTER'Char'TILDE = '~'\n\
   \anon'characterToChar _ = error \"Cannot print non-character enumerate\""
