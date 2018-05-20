module Parser.Functions.Parse.Objects
   ( parseConstant
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
         )
import Parser.Netlist.Types.Stores
         ( ScopeStore
         , UnitStore
         , ConstantStore
         , SubtypeStore
         )
import Parser.Netlist.Functions.Stores (isNameInUnit)
import Parser.Netlist.Types.Error (NetlistError(..))
import Parser.Functions.IdentifyToken
         ( isColon
         , isComma
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
