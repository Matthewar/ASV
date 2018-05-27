module Sim.Builtin.ControlModule
   ( controlModule
   ) where

import Data.Text

controlModule :: Text
controlModule = pack
   "module Control\n\
   \   ( Control'Time(..)\n\
   \   , initialTime\n\
   \   , Entity'State(..)\n\
   \   , Entity'Stack\n\
   \   , Control'Signal(..)\n\
   \   , Control'Stack\n\
   \   --, control'jumpTime\n\
   \   , Control'SEVERITY_TRACKER(..)\n\
   \   , Control'SEVERITY_FAILURE(..)\n\
   \   , control'incrementWarnings\n\
   \   , control'incrementErrors\n\
   \   , control'assertNote\n\
   \   , control'assertWarning\n\
   \   , control'assertError\n\
   \   , control'assertFailure\n\
   \   , now\n\
   \   ) where\n\
   \\n\
   \import Control.Monad.Trans.State\n\
   \         ( StateT\n\
   \         , modify\n\
   \         , gets\n\
   \         )\n\
   \import Control.Monad.Except\n\
   \         ( ExceptT\n\
   \         , throwError\n\
   \         , lift\n\
   \         , liftIO\n\
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
   \\n\
   \initialTime :: Control'Time\n\
   \initialTime = Control'Time (STD.STANDARD.mkType'ANON'TIME 0) 0\n\
   \\n\
   \--data META'Events portsInType stateType =\n\
   \--   META'Events\n\
   \--      { entity'PortsIn :: portsInType\n\
   \--      , entity'State :: stateType\n\
   \--      --, queue :: [(Control'Time,\n\
   \--      }\n\
   \\n\
   \data Entity'State portsInType stateType portsOutType processesType =\n\
   \   Entity'State\n\
   \      { entity'portsIn :: portsInType\n\
   \      , entity'state :: stateType\n\
   \      , entity'portsOut :: portsOutType\n\
   \      , entity'processes :: processesType\n\
   \      }\n\
   \\n\
   \-- |Entity stack\n\
   \type Entity'Stack a b = StateT a (StateT Control'Time (StateT Control'SEVERITY_TRACKER (ExceptT Control'SEVERITY_FAILURE IO))) b\n\
   \\n\
   \data Control'Signal a =\n\
   \   Control'Signal\n\
   \      --{ control'signal'current :: (Control'Time,a)\n\
   \      { control'signal'transactions :: [(Control'Time,a)]\n\
   \      , control'signal'active :: Bool\n\
   \      }\n\
   \\n\
   \-- |Monad stack\n\
   \-- Access to IO for file access/stdin/stdout\n\
   \type Control'Stack a = StateT Control'Time (StateT Control'SEVERITY_TRACKER (ExceptT Control'SEVERITY_FAILURE IO)) a\n\
   \\n\
   \control'jumpTime :: STD.STANDARD.Type'ANON'TIME -> Control'Stack ()\n\
   \control'jumpTime time =\n\
   \   let incDelta curTime =\n\
   \         curTime\n\
   \            { time'Real = time\n\
   \            , time'Delta = 1 + (time'Delta curTime)\n\
   \            }\n\
   \   in modify incDelta\n\
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
   \control'incrementWarnings :: Control'Stack ()\n\
   \control'incrementWarnings =\n\
   \   let incWarning :: Control'SEVERITY_TRACKER -> Control'SEVERITY_TRACKER\n\
   \       incWarning trackers = trackers { control'warnings = 1 + (control'warnings trackers) }\n\
   \   in lift $ modify $ incWarning\n\
   \\n\
   \control'incrementErrors :: Control'Stack ()\n\
   \control'incrementErrors =\n\
   \   let incError :: Control'SEVERITY_TRACKER -> Control'SEVERITY_TRACKER\n\
   \       incError trackers = trackers { control'errors = 1 + (control'errors trackers) }\n\
   \   in lift $ modify $ incError\n\
   \\n\
   \control'assertNote :: String -> STD.STANDARD.Type'ANON'STRING -> Control'Stack ()\n\
   \control'assertNote unitName message = control'assert unitName $ \"NOTE: \" ++ anon'stringToString message\n\
   \\n\
   \control'assertWarning :: String -> STD.STANDARD.Type'ANON'STRING -> Control'Stack ()\n\
   \control'assertWarning unitName message = do\n\
   \   control'assert unitName $ \"WARNING: \" ++ anon'stringToString message\n\
   \   control'incrementWarnings\n\
   \\n\
   \control'assertError :: String -> STD.STANDARD.Type'ANON'STRING -> Control'Stack ()\n\
   \control'assertError unitName message = do\n\
   \   control'assert unitName $ \"ERROR: \" ++ anon'stringToString message\n\
   \   control'incrementErrors\n\
   \\n\
   \control'assertFailure :: String -> STD.STANDARD.Type'ANON'STRING -> Control'Stack ()\n\
   \control'assertFailure unitName message = do\n\
   \   control'assert unitName $ \"FAILURE: \" ++ anon'stringToString message\n\
   \   throwError $ Control'SEVERITY_FAILURE \"\" --message\n\
   \\n\
   \control'assert :: String -> String -> Control'Stack ()\n\
   \control'assert unitName message =\n\
   \   let completeMessage = \"ASSERT: \" ++ unitName ++ \": \" ++ message\n\
   \   in liftIO $ putStrLn completeMessage\n\
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
   \anon'characterToChar _ = error \"Cannot print non-character enumerate\"\n\
   \\n\
   \--now :: SimStack TIME\n\
   \--now = gets $ timeVal\n\
   \now :: Control'Stack STD.STANDARD.Type'ANON'TIME\n\
   \now = gets $ time'Real"
