module Sim.Output.Names
   ( printComponentActualName
   , printComponentSafeName
   , showEnum
   , showTypeName
   , showTypeConstructor
   , showTypeExtractor
   , showNewSubtypeValue
   , showPortInName
   , showPortOutName
   , showInternalSignalName
   , showPortInDef
   , showPortOutDef
   , showInternalSignalDef
   , showPortInInitialValue
   , showPortOutInitialValue
   , showInternalSignalInitialValue
   , showProcessName
   , showProcessDef
   , showProcessSetInitial
   , showProcessInitialName
   , showProcessRead
   , showPortInRead
   , showPortOutRead
   , showInternalSignalRead
   ) where

import qualified Data.Map.Strict as MapS
import Data.List (intersperse)
import Data.Function ((&))
import Numeric (showFFloat)

import Parser.Netlist.Types.Representation
         ( NetlistName
         , Enumerate(..)
         , Subtype(..)
         , Value(..)
         )

-- |Print component name as it should appear to the designer
printComponentActualName :: [String] -> String
printComponentActualName = concat . (intersperse ".")

-- |Print component name without period separators
-- NOTE: Period separators are used in Haskell qualified names so shouldn't be used in function names, etc.
printComponentSafeName :: [String] -> String
printComponentSafeName = concat . (intersperse "'")

showEnum :: String -> Enumerate -> String
showEnum typeName (Enum_Identifier str) = typeName ++ "'Iden'" ++ str
showEnum typeName (Enum_Char chr) = typeName ++ "'Char'" ++ (characterMap MapS.! chr)
   where characterMap :: MapS.Map Char String
         characterMap =
            [ (' ',"SPACE")
            , ('!',"EXCLAMATION")
            , ('"',"QUOTE")
            , ('#',"HASH")
            , ('$',"DOLLAR")
            , ('%',"PERCENT")
            , ('&',"AMPERSAND")
            , ('\'',"TICK")
            , ('(',"LEFTPAREN")
            , (')',"RIGHTPAREN")
            , ('*',"STAR")
            , ('+',"PLUS")
            , (',',"COMMA")
            , ('-',"HYPHEN")
            , ('.',"PERIOD")
            , ('/',"FORWARDSLASH")
            , ('0',"0")
            , ('1',"1")
            , ('2',"2")
            , ('3',"3")
            , ('4',"4")
            , ('5',"5")
            , ('6',"6")
            , ('7',"7")
            , ('8',"8")
            , ('9',"9")
            , (':',"COLON")
            , (';',"SEMICOLON")
            , ('<',"LESSTHAN")
            , ('=',"EQUAL")
            , ('>',"GREATERTHAN")
            , ('?',"QUESTION")
            , ('@',"AT")
            , ('A',"A")
            , ('B',"B")
            , ('C',"C")
            , ('D',"D")
            , ('E',"E")
            , ('F',"F")
            , ('G',"G")
            , ('H',"H")
            , ('I',"I")
            , ('J',"J")
            , ('K',"K")
            , ('L',"L")
            , ('M',"M")
            , ('N',"N")
            , ('O',"O")
            , ('P',"P")
            , ('Q',"Q")
            , ('R',"R")
            , ('S',"S")
            , ('T',"T")
            , ('U',"U")
            , ('V',"V")
            , ('W',"W")
            , ('X',"X")
            , ('Y',"Y")
            , ('Z',"Z")
            , ('[',"LEFTSQUAREBRACE")
            , ('\\',"BACKSLASH")
            , (']',"RIGHTSQUAREBRACE")
            , ('^',"CAROT")
            , ('_',"UNDERSCORE")
            , ('`',"BACKTICK")
            , ('a',"a")
            , ('b',"b")
            , ('c',"c")
            , ('d',"d")
            , ('e',"e")
            , ('f',"f")
            , ('g',"g")
            , ('h',"h")
            , ('i',"i")
            , ('j',"j")
            , ('k',"k")
            , ('l',"l")
            , ('m',"m")
            , ('n',"n")
            , ('o',"o")
            , ('p',"p")
            , ('q',"q")
            , ('r',"r")
            , ('s',"s")
            , ('t',"t")
            , ('u',"u")
            , ('v',"v")
            , ('w',"w")
            , ('x',"x")
            , ('y',"y")
            , ('z',"z")
            , ('{',"LEFTBRACE")
            , ('|',"BAR")
            , ('}',"RIGHTBRACE")
            , ('~',"TILDE")
            ]
            & MapS.fromList

showTypeName :: (NetlistName,String) -> String
showTypeName (typePackage,typeName) = show typePackage ++ ".Type'" ++ typeName

showTypeConstructor :: (NetlistName,String) -> String
showTypeConstructor (typePackage,typeName) = show typePackage ++ ".mkType'" ++ typeName

showTypeExtractor :: (NetlistName,String) -> String
showTypeExtractor (typePackage,typeName) = show typePackage ++ ".extractType'" ++ typeName

showNewSubtypeValue :: (NetlistName,String) -> Subtype -> Value -> String
showNewSubtypeValue subtypeName subtype value =
   let baseTypeConstr = showBaseTypeConstructor subtype
       subtypeConstr = showTypeConstructor subtypeName
   in subtypeConstr
      ++ " $ "
      ++ case value of
            Value_Enum _ enum -> showEnum baseTypeConstr enum
            Value_Int int -> baseTypeConstr ++ " " ++ show int
            Value_Float flt -> baseTypeConstr ++ " " ++ (showFFloat Nothing flt) ""
            Value_Physical phys -> baseTypeConstr ++ " " ++ show phys
            --Value_Array

showBaseTypeConstructor :: Subtype -> String
showBaseTypeConstructor (EnumerationSubtype _ typeName _ _) = showTypeName typeName
showBaseTypeConstructor (IntegerSubtype _ typeName _) = showTypeConstructor typeName
showBaseTypeConstructor (FloatingSubtype _ typeName _) = showTypeConstructor typeName
showBaseTypeConstructor (PhysicalSubtype _ typeName _ _ _) = showTypeConstructor typeName
--showBaseTypeConstructor (ArraySubtype _ (typePackage,typeName) _ _) = show typePackage ++ "." ++ typeName

portInPrefix :: String
portInPrefix = "ports'in'"

showPortInName :: String -> String
showPortInName portName = portInPrefix ++ portName

portOutPrefix :: String
portOutPrefix = "ports'out'"

showPortOutName :: String -> String
showPortOutName portName = portOutPrefix ++ portName

internalSignalPrefix :: String
internalSignalPrefix = "signal'"

showInternalSignalName :: String -> String
showInternalSignalName signalName = internalSignalPrefix ++ signalName

showSignalNameDef :: (String -> String) -> String -> (NetlistName,String) -> String
showSignalNameDef nameMaker signalName subtypeName = nameMaker signalName ++ " :: Control'Signal " ++ showTypeName subtypeName

showPortInDef :: String -> (NetlistName,String) -> String
showPortInDef = showSignalNameDef showPortInName

showPortOutDef :: String -> (NetlistName,String) -> String
showPortOutDef = showSignalNameDef showPortOutName

showInternalSignalDef :: String -> (NetlistName,String) -> String
showInternalSignalDef = showSignalNameDef showInternalSignalName

showSignalNameInitialValue :: (String -> String) -> String -> String -> String
showSignalNameInitialValue nameMaker signalName valueStr =
   nameMaker signalName ++ " = Control'Signal [(initialTime," ++ valueStr ++ ")] False False"

showPortInInitialValue :: String -> String -> String
showPortInInitialValue = showSignalNameInitialValue showPortInName

showPortOutInitialValue :: String -> String -> String
showPortOutInitialValue = showSignalNameInitialValue showPortOutName

showInternalSignalInitialValue :: String -> String -> String
showInternalSignalInitialValue = showSignalNameInitialValue showInternalSignalName

processNamePrefix :: String
processNamePrefix = "processes'"

showProcessName :: String -> String
showProcessName processName = processNamePrefix ++ processName

showProcessDef :: String -> String
showProcessDef processName = showProcessName processName ++ " :: (Int,STD.STANDARD.Type'ANON'TIME)"

showProcessSetInitial :: String -> String
showProcessSetInitial processName = showProcessName processName ++ " = " ++ showProcessInitialName processName

processInitialNamePrefix :: String
processInitialNamePrefix = "process'initial'"

showProcessInitialName :: String -> String
showProcessInitialName processName = processInitialNamePrefix ++ processName

showProcessRead :: String -> String
showProcessRead processName = showProcessName processName ++ " processes"

showSignalNameRead :: (String -> String) -> String -> String -> String
showSignalNameRead nameMaker signalStateName signalName =
   nameMaker signalName ++ " " ++ signalStateName

showPortInRead :: String -> String
showPortInRead = showSignalNameRead showPortInName "portsIn"

showPortOutRead :: String -> String
showPortOutRead = showSignalNameRead showPortOutName "portsOut"

showInternalSignalRead :: String -> String
showInternalSignalRead = showSignalNameRead showInternalSignalName "signals"
