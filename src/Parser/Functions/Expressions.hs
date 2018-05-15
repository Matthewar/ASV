module Parser.Functions.Expressions
   ( notLocallyStatic
   , isLocallyStatic
   ) where

import Parser.Types.Expressions

notLocallyStatic :: Staticity -> Bool
notLocallyStatic LocallyStatic = False
notLocallyStatic _ = True

isLocallyStatic :: Staticity -> Bool
isLocallyStatic LocallyStatic = True
isLocallyStatic _ = False

--compareTypes :: AllTypes -> Type -> Bool
--compareTypes (Type_Type type1) type2 = type1 == type2
--compareTypes Type_UniversalInt (IntegerType _) = True
--compareTypes Type_UniversalInt _ = False
--compareTypes Type_UniversalReal (FloatingType _) = True
--compareTypes Type_UniversalReal _ = False
--compareTypes Type_String (ArrayType constraint _ _ subtype) =
--   case 
--compareTypes Type_BitString (ArrayType 
