module Sim.Types
   ( outputTypes
   ) where

import qualified Data.Map.Strict as MapS
import Data.List (intersperse)
import Data.Function ((&))
import Control.Monad.Except
         ( ExceptT
         , liftIO
         )

import Parser.Netlist.Types.Representation
         ( Type(..)
         , NetlistName
         , Enumerate(..)
         , IntegerRange
         , FloatRange
         , Subtype(..)
         )
import Parser.Netlist.Functions.Representation
         ( int_scalarHigh
         , int_scalarLow
         , float_scalarHigh
         , float_scalarLow
         )
import Parser.Netlist.Types.Stores (TypeStore)
import Manager.Types.Error (ConverterError)

newline = "\n"
tab = "   "

outputTypes :: FilePath -> TypeStore -> ExceptT ConverterError IO ()
outputTypes file = (outputTypes' file) . MapS.toList

outputTypes' :: FilePath -> [(String,Type)] -> ExceptT ConverterError IO ()
outputTypes' fileName ((typeName,typeData):others) = do
   case typeData of
      EnumerationType enums -> printEnumType fileName typeName enums
      IntegerType range -> printIntType fileName typeName range
      FloatingType range -> printFloatType fileName typeName range
      PhysicalType range baseUnit _ -> printPhysicalType fileName typeName range baseUnit
      ArrayType bounds subtypeName subtypePackage _ -> printArrayType fileName typeName bounds subtypeName subtypePackage
   outputTypes' fileName others
outputTypes' _ [] = return ()

printEnumType :: FilePath -> String -> [Enumerate] -> ExceptT ConverterError IO ()
printEnumType file name enums =
   let typeName = "Type'" ++ name
       convertEnum (Enum_Identifier str) = typeName ++ "'Iden'" ++ str
       convertEnum (Enum_Char chr) = typeName ++ "'Char'" ++ (characterMap MapS.! chr)
       typeBaseFormatting = concat $ intersperse (newline ++ tab ++ "| ") $ map convertEnum enums
       typeBaseStr = "data " ++ typeName ++ " = " ++ newline ++ tab ++ typeBaseFormatting
--       typeScalar =
--         "instance (VHDL_Scalar "
--         ++ name
--         ++ ") where"
--         ++ newline ++ tab
--         ++ "scalarLeft = "
--         ++ (convertEnum $ head enums)
--         ++ newline ++ tab
--         ++ "scalarRight = "
--         ++ (convertEnum $ last enums)
--         ++ newline ++ tab
--         ++ "scalarHigh = scalarRight"
--         ++ newline ++ tab
--         ++ "scalarLow = scalarLeft"
--       (discretePosStr,discreteValStr) = makeDiscretePosVal 0 enums [] []
--       (discreteLeftFormatting,discreteRightFormatting) = makeDiscreteLeftRight enums [] []
--       discreteSuccStr = concat $ map (\s -> newline ++ tab ++ "discreteSucc " ++ s) discreteRightFormatting
--       discretePredStr = concat $ map (\s -> newline ++ tab ++ "discretePred " ++ s) discreteLeftFormatting
--       discreteLeftOfStr = concat $ map (\s -> newline ++ tab ++ "discreteLeftOf " ++ s) discreteLeftFormatting
--       discreteRightOfStr = concat $ map (\s -> newline ++ tab ++ "discreteRightOf " ++ s) discreteRightFormatting
--       typeDiscrete =
--         "instance (VHDL_Discrete "
--         ++ name
--         ++ ") where"
--         ++ discretePosStr
--         ++ discreteValStr
--         ++ discreteSuccStr
--         ++ discretePredStr
--         ++ discreteLeftOfStr
--         ++ discreteRightOfStr
--       typeOps =
--         "instance (VHDL_Eq "
--         ++ name
--         ++ 
   in liftIO $ appendFile file $ newline ++ typeBaseStr ++ newline -- ++ typeScalar ++ newline ++ typeDiscrete
   where --makeDiscretePosVal _ [] posStrLst valStrLst = (concat posStrLst,concat valStrLst)
         --makeDiscretePosVal val (enum:enums) posStrLst valStrLst =
         --   let convEnum = convertEnum enum
         --       posStr = newline ++ tab ++ "discretePos " ++ convEnum ++ " = " ++ val
         --       valStr = newline ++ tab ++ "discreteVal " ++ val ++ " = " ++ convEnum
         --   in makeDiscretePosVal (val + 1) enums (posStr:posStrLst) (valStr:valStrLst)
         --makeDiscreteLeftRight (enum1:enum2:enums) leftStr rightStr =
         --   let convEnum1 = convertEnum enum1
         --       convEnum2 = convertEnum enum2
         --       left = convEnum2 ++ " = " ++ convEnum1
         --       right = convEnum1 ++ " = " ++ convEnum2
         --   in makeDiscreteLeftRight enums (left:leftStr) (right:rightStr)
         --makeDiscreteLeftRight _ leftStr rightStr = (leftStr,rightStr)
         characterMap :: MapS.Map Char String
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

printIntType :: FilePath -> String -> IntegerRange -> ExceptT ConverterError IO ()
printIntType file name intRange =
   let typeName = "Type'" ++ name
       typeBaseStr = "newtype " ++ typeName ++ " = " ++ typeName ++ " Int64"
       constructorStr =
         let funcName = "mk" ++ typeName
             highVal = show $ int_scalarHigh intRange
             lowVal = show $ int_scalarLow intRange
         in funcName ++ " :: Integer -> " ++ typeName
            ++ newline
            ++ funcName ++ " value ="
            ++ newline ++ tab
            ++ "if value > " ++ highVal
            ++ " || value < " ++ lowVal
            ++ newline ++ tab ++ tab
            ++ "then " ++ typeName ++ " value"
            ++ newline ++ tab ++ tab
            ++ "else error $ \"" ++ typeName ++ " value \" ++ value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
--       typeScalar =
--         "instance (VHDL_Scalar "
--         ++ name
--         ++ ") where"
--         ++ newline ++ tab
--         ++ "scalarLeft = "
--         ++ show left
--         ++ newline ++ tab
--         ++ "scalarRight = "
--         ++ show right
--         ++ newline ++ tab
--         ++ "scalarHigh = "
--         ++ show high
--         ++ newline ++ tab
--         ++ "scalarLow = "
--         ++ show low
--       typeOps =
--         "instance (VHDL_Add "
--       typeDiscrete =
--         "instance (VHDL_Discrete "
--         ++ typeName
--         ++ ") where"
--         ++ newline ++ tab
--         ++ "discretePos ("
--         ++ typeName
--         ++ "val) = val"
--         ++ newline ++ tab
--         ++ "discreteVal val = "
--         ++ typeName
--         ++ " val"
--         ++ newline ++ tab
--         ++ "discreteSucc val = va 
   in liftIO $ appendFile file $ newline ++ typeBaseStr ++ newline ++ constructorStr ++ newline -- ++ typeScalar ++ newline ++ typeOps ++ typeDiscrete

printFloatType :: FilePath -> String -> FloatRange -> ExceptT ConverterError IO ()
printFloatType file name floatRange =
   let typeName = "Type'" ++ name
       typeBaseStr = "newtype " ++ typeName ++ " = " ++ typeName ++ " Double"
       constructorStr =
         let funcName = "mk" ++ typeName
             highVal = show $ float_scalarHigh floatRange
             lowVal = show $ float_scalarLow floatRange
         in funcName ++ " :: Double -> " ++ typeName
            ++ newline
            ++ funcName ++ " value ="
            ++ newline ++ tab
            ++ "case value of"
            ++ newline ++ tab ++ tab
            ++ "_ | isInfinite value -> error \"" ++ typeName ++ " value is outside of 64 bit IEEE 754 range\""
            ++ newline ++ tab ++ tab
            ++ "_ | value < " ++ lowVal ++ " || value > " ++ highVal ++ " -> error \"" ++ typeName ++ " value \" ++ value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
   in liftIO $ appendFile file $ newline ++ typeBaseStr ++ newline ++ constructorStr ++ newline

printPhysicalType :: FilePath -> String -> IntegerRange -> String -> ExceptT ConverterError IO ()
printPhysicalType fileName name intRange baseUnit =
   let typeName = "Type'" ++ name
       typeBaseStr = "newtype " ++ typeName ++ " = " ++ typeName ++ " Int64"
       constructorStr =
         let funcName = "mk" ++ typeName
             highVal = show $ int_scalarHigh intRange
             lowVal = show $ int_scalarLow intRange
         in funcName ++ " :: Integer -> " ++ typeName
            ++ newline
            ++ funcName ++ " value ="
            ++ newline ++ tab
            ++ "if value > " ++ highVal
            ++ " || value < " ++ lowVal
            ++ newline ++ tab ++ tab
            ++ "then " ++ typeName ++ " value"
            ++ newline ++ tab ++ tab
            ++ "else error $ \"" ++ typeName ++ " value \" ++ value ++ \" is outside of range " ++ lowVal ++ " < value < " ++ highVal ++ "\""
   in liftIO $ appendFile fileName $ newline ++ typeBaseStr ++ newline ++ constructorStr ++ newline

printArrayType :: FilePath -> String -> [Subtype] -> String -> NetlistName -> ExceptT ConverterError IO ()
printArrayType fileName name bounds elemTypeName elemTypePackage =
   let typeName = "Type'" ++ name
       typeBaseStr =
         "newtype " ++ typeName ++ " = "
         ++ typeName ++ " Data.Map.Strict.Map ("
         ++ (concat $ intersperse "," $ map (\(Subtype _ subtypeName subtypePackage _ _) -> show subtypePackage ++ ".Type'" ++ subtypeName) bounds)
         ++ ") " ++ show elemTypePackage ++ ".Type'" ++ elemTypeName
   in liftIO $ appendFile fileName $ newline ++ typeBaseStr ++ newline
