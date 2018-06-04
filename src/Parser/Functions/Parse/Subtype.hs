module Parser.Functions.Parse.Subtype
   ( parseSubtype
   , parseSubtypeIndication
   ) where

import qualified Data.Map.Strict as MapS
import Data.List (find)
import Data.Char (toUpper)
import Control.Monad.Except
         ( throwError
         , replicateM
         , when
         , unless
         )

import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper
         ( passPosition
         , raisePosition
         )
import Lexer.Types.Error (ParserError(..))
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Netlist.Types.Representation
         ( Type(..)
         , Subtype(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , UnitStore
         , SubtypeStore
         )
import Parser.Netlist.Functions.Stores
         ( matchSubtypeNameInScope
         , matchFunctionNameInScope
         , isNameInUnit
         )
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Functions.IdentifyToken
         ( isKeywordIs
         , isKeywordRange
         , isLeftParen
         , isSemicolon
         , matchIdentifier
         )
import Parser.Netlist.Types.Representation
         ( IntegerRange(..)
         , FloatRange(..)
         , RangeDirection(..)
         , NetlistName
         )
import Parser.Functions.Parse.Type
         ( parseRange
         , RangeConstraint(..)
         )
import Sim.Output.Names (showEnum)
import Manager.Types.Error (ConverterError(..))

parseSubtype :: ScopeStore -> UnitStore -> NetlistName -> ParserStack (SubtypeStore -> SubtypeStore)
parseSubtype scope unit unitName = do
   [subtypeNameTok,keywordTok] <- replicateM 2 getToken
   subtypeName <- case matchIdentifier subtypeNameTok of
                     Just (PosnWrapper _ name) -> return $ map toUpper name
                     Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSubtypeName subtypeNameTok
   when (isNameInUnit unit subtypeName) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_SubtypeNameAlreadyDefined subtypeName) subtypeNameTok
   unless (isKeywordIs keywordTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedKeywordIsInSubtypeDecl keywordTok
   (_,_,subtypeData,_) <- parseSubtypeIndication True scope unit unitName
   let modSubtypes = MapS.insert subtypeName subtypeData
   endTok <- getToken
   unless (isSemicolon endTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSemicolonInSubtypeDecl endTok
   return modSubtypes

parseSubtypeIndication :: Bool -> ScopeStore -> UnitStore -> NetlistName -> ParserStack (NetlistName,String,Subtype,SubtypeStore -> SubtypeStore)
parseSubtypeIndication resFuncAllowed scope unit unitName = do
   fstToken <- getToken
   resFunc <- if resFuncAllowed
               then throwError $ ConverterError_NotImplemented $ passPosition "Resolution function in subtype indication" fstToken
               else do
                  saveToken fstToken
                  return Nothing
   typeNameTok <- getToken -- ?? Adapt to allow expanded names
   typeName <- case matchIdentifier typeNameTok of
                  Just (PosnWrapper _ name) -> return $ map toUpper name
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedTypeMarkInSubtypeIndic typeNameTok
   let --typeCheck = case matchTypeNameInScope scope unit unitName typeName of -- Name denotes type and unconstrained subtype
       --              Just (typeData,packageName) ->
       --              Nothing -> subtypeCheck
       subtypeCheck = case matchSubtypeNameInScope scope unit unitName typeName of
                        Just (subtypeData,packageName) -> parseConstraint scope unit unitName (subtypeData,packageName,typeName)
                        Nothing -> functionCheck
       functionCheck =  let errorName = case matchFunctionNameInScope scope unit unitName typeName of
                                          Just _ | not resFuncAllowed -> NetlistError_ResFuncNotAllowedInSubtypeIndic --typeName
                                          Just _ -> NetlistError_ExpectedTypeMarkButGotFunctionName
                                          Nothing -> NetlistError_UnrecognisedNameInSubtypeIndic --typeName
                        in throwError $ ConverterError_Netlist $ passPosition errorName typeNameTok
   -- returns (subtypePackage,subtypeName,subtypeData,modSubtype)
   -- NetlistName of subtype, string name of subtype, subtype data (Subtype), function to modify subtype store
   subtypeCheck

parseConstraint :: ScopeStore -> UnitStore -> NetlistName -> (Subtype,NetlistName,String) -> ParserStack (NetlistName,String,Subtype,SubtypeStore -> SubtypeStore)
parseConstraint scope unit unitName (subtypeData,packageName,subtypeName) = do
   constraintTok <- getToken
   case constraintTok of
      token | isKeywordRange token -> do
         case subtypeData of
            ArraySubtype _ _ _ _ _ -> throwError $ ConverterError_Parse $ raisePosition ParseErr_RangeConstraintForArrayType token
            _ -> return ()
         range <- parseRange scope unit unitName $ getPos token
         let integerConstraintCompare (IntegerRange left1 right1 dir1) (IntegerRange left2 right2 dir2) =
               let checkVal val = case dir1 of
                                    To -> val >= left1 && val <= right1
                                    Downto -> val <= left1 && val >= right1
               in checkVal left2 && checkVal right2
             floatingConstraintCompare (FloatRange left1 right1 dir1) (FloatRange left2 right2 dir2) =
               let checkVal val = case dir1 of
                                    To -> val >= left1 && val <= right1
                                    Downto -> val <= left1 && val >= right1
               in checkVal left2 && checkVal right2
         case (subtypeData,range) of
            (IntegerSubtype resFunc baseTypeName constraint,IntegerConstraint newConstraint) | integerConstraintCompare constraint newConstraint ->
               let (left,right,dir) = case newConstraint of
                                       (IntegerRange left right To) -> (show left,show right,"TO")
                                       (IntegerRange left right Downto) -> (show left,show right,"DOWNTO")
                   newTypeName = subtypeName ++ "'ANON'" ++ dir ++ "'" ++ left ++ "'" ++ right
                   newTypeData = IntegerSubtype resFunc baseTypeName newConstraint
               in return (unitName,newTypeName,newTypeData,MapS.insert newTypeName newTypeData)
            (IntegerSubtype _ _ constraint,IntegerConstraint newConstraint) -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_InvalidSubtypeIntConstraint constraint newConstraint) token
            (FloatingSubtype resFunc baseTypeName constraint,FloatConstraint newConstraint) | floatingConstraintCompare constraint newConstraint ->
               let (left,right,dir) = case newConstraint of
                                       (FloatRange left right To) -> (show left,show right,"TO")
                                       (FloatRange left right Downto) -> (show left,show right,"DOWNTO")
                   newTypeName = subtypeName ++ "'ANON'" ++ dir ++ "'" ++ left ++ "'" ++ right
                   newTypeData = FloatingSubtype resFunc baseTypeName newConstraint
               in return (unitName,newTypeName,newTypeData,MapS.insert newTypeName newTypeData)
            (FloatingSubtype _ _ constraint,FloatConstraint newConstraint) -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_InvalidSubtypeFloatConstraint constraint newConstraint) token
            (EnumerationSubtype resFunc baseTypeName enums constraint,EnumerationConstraint potentialConstraints) ->
               case find (\(_,_,enumPackage,enumTypeName,_) -> enumPackage == packageName && enumTypeName == subtypeName) potentialConstraints of
                  Just (left,right,_,_,EnumerationType _) ->
                     let newTypeName = subtypeName ++ "'ANON" ++ showEnum "" left ++ showEnum "" right
                         newTypeData = EnumerationSubtype resFunc baseTypeName enums (left,right)
                     in return (unitName,newTypeName,newTypeData,MapS.insert newTypeName newTypeData)
                  Nothing -> throwError $ ConverterError_Netlist $ passPosition NetlistError_NoValidEnumRangeFound token
            (PhysicalSubtype resFunc baseTypeName baseUnit secondaryUnits constraint,PhysicalConstraint newConstraint) | integerConstraintCompare constraint newConstraint ->
               let (left,right,dir) = case newConstraint of
                                       (IntegerRange left right To) -> (show left,show right,"TO")
                                       (IntegerRange left right Downto) -> (show left,show right,"DOWNTO")
                   newTypeName = subtypeName ++ "'ANON'" ++ dir ++ "'" ++ left ++ "'" ++ right
                   newTypeData = PhysicalSubtype resFunc baseTypeName baseUnit secondaryUnits newConstraint
               in return (unitName,newTypeName,newTypeData,MapS.insert newTypeName newTypeData)
            (PhysicalSubtype resFunc baseTypeName baseUnit secondaryUnits constraint,PhysicalConstraint newConstraint) -> throwError $ ConverterError_Netlist $ passPosition (NetlistError_InvalidSubtypePhysConstraint constraint newConstraint) token
      token | isLeftParen token -> throwError $ ConverterError_NotImplemented $ passPosition "Discrete range constraint (array type subtype indication)" token
      token -> do
         saveToken token
         return (packageName,subtypeName,subtypeData,\s -> s)
