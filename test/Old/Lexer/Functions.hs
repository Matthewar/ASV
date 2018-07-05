module Lexer.Functions
   ( getLexResult
   , compareBasicUnit
   , stringToKeyword
   , stringToDelimiter
   ) where

import Test.Tasty.HUnit
import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)

import Lexer.Lexer (lexerList)
import Lexer.Types.Token

import Manager.Types.Error (ConverterError(ConverterError_Parse))

-- |Run the lexer and get the result
getLexResult :: String -> IO (Either ConverterError [Token])
getLexResult = lexerList

-- |Compare a single unit input
compareBasicUnit :: String -> Token -> Assertion
compareBasicUnit input output = do
   result <- getLexResult input
   let expectedOutput = Right [output]
   result @?= expectedOutput

stringToKeyword :: String -> ReservedWord
stringToKeyword keyword = keywordMap MapS.! (map toUpper keyword)
   where keywordMap :: MapS.Map String ReservedWord
         keywordMap = MapS.fromList $
            [ ("ABS",Abs)
            , ("ACCESS",Access)
            , ("AFTER",After)
            , ("ALIAS",Alias)
            , ("ALL",All)
            , ("AND",And)
            , ("ARCHITECTURE",Architecture)
            , ("ARRAY",Array)
            , ("ASSERT",Assert)
            , ("ATTRIBUTE",Attribute)
            , ("BEGIN",Begin)
            , ("BLOCK",Block)
            , ("BODY",Body)
            , ("BUFFER",Buffer)
            , ("BUS",Bus)
            , ("CASE",Case)
            , ("COMPONENT",Component)
            , ("CONFIGURATION",Configuration)
            , ("CONSTANT",Constant)
            , ("DISCONNECT",Disconnect)
            , ("DOWNTO",Downto)
            , ("ELSE",Else)
            , ("ELSIF",Elsif)
            , ("END",End)
            , ("ENTITY",Entity)
            , ("EXIT",Exit)
            , ("FILE",File)
            , ("FOR",For)
            , ("FUNCTION",Function)
            , ("GENERATE",Generate)
            , ("GENERIC",Generic)
            , ("GUARDED",Guarded)
            , ("IF",If)
            , ("IN",In)
            , ("INOUT",Inout)
            , ("IS",Is)
            , ("LABEL",Label)
            , ("LIBRARY",Library)
            , ("LINKAGE",Linkage)
            , ("LOOP",Loop)
            , ("MAP",Map)
            , ("MOD",Mod)
            , ("NAND",Nand)
            , ("NEW",New)
            , ("NEXT",Next)
            , ("NOR",Nor)
            , ("NOT",Not)
            , ("NULL",Null)
            , ("OF",Of)
            , ("ON",On)
            , ("OPEN",Open)
            , ("OR",Or)
            , ("OTHERS",Others)
            , ("OUT",Out)
            , ("PACKAGE",Package)
            , ("PORT",Port)
            , ("PROCEDURE",Procedure)
            , ("PROCESS",Process)
            , ("RANGE",Range)
            , ("RECORD",Record)
            , ("REGISTER",Register)
            , ("REM",Rem)
            , ("REPORT",Report)
            , ("RETURN",Return)
            , ("SELECT",Select)
            , ("SEVERITY",Severity)
            , ("SIGNAL",Signal)
            , ("SUBTYPE",Subtype)
            , ("THEN",Then)
            , ("TO",To)
            , ("TRANSPORT",Transport)
            , ("TYPE",Type)
            , ("UNITS",Units)
            , ("UNTIL",Until)
            , ("USE",Use)
            , ("VARIABLE",Variable)
            , ("WAIT",Wait)
            , ("WHEN",When)
            , ("WHILE",While)
            , ("WITH",With)
            , ("XOR",Xor)
            ]

stringToDelimiter :: String -> Maybe OperatorType
stringToDelimiter op = MapS.lookup op delimiterMap
   where delimiterMap :: MapS.Map String OperatorType
         delimiterMap = MapS.fromList $
            [ ("=>",Arrow)
            , ("**",DoubleStar)
            , (":=",VarAssign)
            , ("/=",Inequality)
            , (">=",GreaterThanOrEqual)
            , ("<=",SignAssign)
            , ("<>",Box)
            , ("&",Ampersand)
            , ("'",Apostrophe)
            , ("(",LeftParen)
            , (")",RightParen)
            , ("*",Star)
            , ("+",Plus)
            , (",",Comma)
            , ("-",Hyphen)
            , (".",Period)
            , ("/",Slash)
            , (":",Colon)
            , (";",Semicolon)
            , ("<",LessThan)
            , ("=",Equal)
            , (">",GreaterThan)
            , ("|",Bar)
            ]
