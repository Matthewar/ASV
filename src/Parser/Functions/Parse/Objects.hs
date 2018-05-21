module Parser.Functions.Parse.Objects
   ( parseConstant
   , parseSignal
   ) where

import qualified Data.Map.Strict as MapS
import Data.Char (toUpper)
import Control.Monad.Except
         ( throwError
         , unless
         , when
         )

import Lexer.Types.PositionWrapper
import Lexer.Types.Error (ParserError(..))
import Lexer.Functions.PositionWrapper
         ( passPosition
         , raisePosition
         )
import Parser.Types.Expressions
         ( AllTypes(..)
         , Staticity(..)
         )
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Netlist.Types.Representation
         ( NetlistName
         , Constant(..)
         , Signal(..)
         , Subtype(..)
         , IntegerRange(..)
         , FloatRange(..)
         , Value(..)
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , UnitStore
         , ConstantStore
         , SubtypeStore
         , SignalStore
         )
import Parser.Netlist.Functions.Stores (isNameInUnit)
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Functions.IdentifyToken
         ( isColon
         , isComma
         , isKeywordBus
         , isKeywordRegister
         , isSemicolon
         , isVarAssign
         , matchIdentifier
         )
import Parser.Functions.Parse.Subtype (parseSubtypeIndication)
import Parser.Functions.Parse.Expression
         ( parseExpression
         , staticTypeCompare
         )
import Manager.Types.Error (ConverterError(..))

-- ?? Pass constant keyword position for error messages?
parseConstant :: ScopeStore -> UnitStore -> NetlistName -> ParserStack (ConstantStore,SubtypeStore -> SubtypeStore)
parseConstant scope unit unitName = do
   idenList <- parseIdentifierList scope unit
   contTok <- getToken
   unless (isColon contTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedColonInConstDecl contTok
   (subtypePackage,subtypeName,subtypeData,modSubtype) <- parseSubtypeIndication False scope unit unitName
   valueTok <- getToken
   case valueTok of
      token | isVarAssign token -> do
         values <- parseExpression LocallyStatic scope unit unitName
         value <- staticTypeCompare subtypeData values $ getPos token
         let constant = Constant (subtypePackage,subtypeName) subtypeData $ Just $ value
         finalTok <- getToken
         unless (isSemicolon finalTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedConstEnd finalTok
         let newConsts = MapS.fromList $ zip idenList $ repeat constant
         return (newConsts,modSubtype)
      token | isSemicolon token -> throwError $ ConverterError_NotImplemented $ passPosition "Deferred constants" token
      token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedConstValueOrEnd token

parseSignal :: ScopeStore -> UnitStore -> NetlistName -> ParserStack (SignalStore,SubtypeStore -> SubtypeStore)
parseSignal scope unit unitName = do
   idenList <- parseIdentifierList scope unit
   contTok <- getToken
   unless (isColon contTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedColonInSigDecl contTok
   (subtypePackage,subtypeName,subtypeData,modSubtype) <- parseSubtypeIndication False scope unit unitName -- ?? False should be true
   valueTok <- getToken
   value <- case valueTok of
      token | isVarAssign token -> do
         values <- parseExpression LocallyStatic scope unit unitName
         staticTypeCompare subtypeData values $ getPos token
      token | isSemicolon token ->
         case subtypeData of
            EnumerationSubtype _ baseTypeName _ (left,_) -> return $ Value_Enum baseTypeName left
            IntegerSubtype _ _ (IntegerRange left _ _) -> return $ Value_Int $ toInteger left
            FloatingSubtype _ _ (FloatRange left _ _) -> return $ Value_Float left
            PhysicalSubtype _ _ _ _ (IntegerRange left _ _) -> return $ Value_Physical $ toInteger left
            ArraySubtype _ _ _ _ _ -> throwError $ ConverterError_NotImplemented $ passPosition "Array type" token
      token | isKeywordRegister token || isKeywordBus token -> throwError $ ConverterError_NotImplemented $ passPosition "Guarded signals" token -- ?? Signal kind support
      token -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSigValueOrEnd token
   let signal = Signal (subtypePackage,subtypeName) subtypeData value
   finalTok <- getToken
   unless (isSemicolon finalTok) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSigEnd finalTok
   let newSigs = MapS.fromList $ zip idenList $ repeat signal
   return (newSigs,modSubtype)

parseIdentifierList :: ScopeStore -> UnitStore -> ParserStack [String]
parseIdentifierList scope unit =
   let parseIdentifierList' :: [String] -> ParserStack [String]
       parseIdentifierList' identifierList = do
         nameTok <- getToken
         constName <- case matchIdentifier nameTok of
                        Just (PosnWrapper _ name) -> return $ map toUpper name
                        Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedConstName nameTok
         when (isNameInUnit unit constName) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_ConstNameAlreadyDefined constName) nameTok
         when (elem constName identifierList) $ throwError $ ConverterError_Netlist $ passPosition (NetlistError_DuplicateConstName constName) nameTok
         contTok <- getToken
         case contTok of
            token | isComma token -> parseIdentifierList' (constName:identifierList)
            token -> do
               saveToken token
               return $ reverse (constName:identifierList)
   in parseIdentifierList' []
