module Parser.Functions.Parse.Context
   ( parseContext
   ) where

import Control.Monad.Except
         ( unless
         , lift
         , throwError
         , replicateM
         )
import Control.Monad.Trans.State
         ( gets
         , modify
         )
import Data.Char (toUpper)

import Lexer.Types.PositionWrapper
import Lexer.Types.Error (ParserError(..))
import Lexer.Functions.PositionWrapper
import Parser.Types.Monad (ScopeStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Functions.IdentifyToken
         ( isKeywordLibrary
         , isKeywordUse
         , matchIdentifier
         , isComma
         , isSemicolon
         , isPeriod
         )
import Parser.Functions.IdentifyName (matchSuffix)
import Parser.Netlist.Types.Scope
         ( Scope(..)
         , UnitScope
         , DeclarationScopeItem(..)
         , WrappedDeclarationScopeItem
         , ScopeConverterError(..)
         )
import Parser.Netlist.Types.Representation (NetlistName(..))
import Manager.Types.Error (ConverterError(..))

parseContext :: ScopeStack ()
parseContext = do
   contextToken <- lift getToken
   if isKeywordLibrary contextToken
      then parseLibraryContext
      else if isKeywordUse contextToken
         then parseUseContext
         else lift $ saveToken contextToken

parseLibraryContext :: ScopeStack ()
parseLibraryContext = do
   identifier <- lift getToken
   case matchIdentifier identifier of
      Just libName -> addLibrary libName
      Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedLibraryName identifier
   op <- lift getToken
   if isComma op
      then parseLibraryContext
      else unless (isSemicolon op) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedLibraryClauseContiue op

parseUseContext :: ScopeStack ()
parseUseContext = do
   [libTok,dotTok1,unitTok,dotTok2,decTok] <- replicateM 5 $ lift getToken
   libName <- case matchIdentifier libTok of
                  Just (PosnWrapper _ lib) -> return $ map toUpper lib
                  Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedLibraryNameInUseClause libTok
   unless (elem libName ["IEEE","WORK","STD"]) $ throwError $ ConverterError_Scope $ passPosition (ScopeConverterError_InvalidLibrary libName) libTok -- ?? CURRENTLY DON'T ACCEPT CUSTOM LIBRARIES
   unless (isPeriod dotTok1) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPeriodInUseClause dotTok1
   packageName <- case matchIdentifier unitTok of
                     Just (PosnWrapper _ name) -> return $ map toUpper name
                     Nothing -> throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPackageNameInUseClause unitTok
   unless (isPeriod dotTok2) $ throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedPeriodInUseClause dotTok2
   declareContents <- matchSuffix decTok
   let isLibInScope = (elem libName) . scopeLibraries
   libInScope <- gets isLibInScope
   unless libInScope $ throwError $ ConverterError_Scope $ passPosition (ScopeConverterError_LibNoScope libName) libTok
   let netlistName = NetlistName libName packageName
       scopeItem = passPosition declareContents decTok
   let updateDeclares scope = scope { scopeDeclarations = combineDeclares netlistName scopeItem $ scopeDeclarations scope }
   modify updateDeclares

addLibrary :: PosnWrapper String -> ScopeStack ()
addLibrary libName = do
   let upperLibName = map toUpper $ unPos libName
   unless (elem upperLibName ["IEEE","WORK","STD"]) $ throwError $ ConverterError_Scope $ passPosition (ScopeConverterError_InvalidLibrary upperLibName) libName -- ?? CURRENTLY DON'T ACCEPT CUSTOM LIBRARIES
   let isLibInScope = (elem upperLibName) . scopeLibraries
   libInScope <- gets isLibInScope
   let insertLibInScope scope = scope { scopeLibraries = (upperLibName:scopeLibraries scope) }
   unless libInScope $ modify insertLibInScope

-- |Combine declarations in declaration scope
combineDeclares :: NetlistName -> WrappedDeclarationScopeItem -> [UnitScope] -> [UnitScope]
combineDeclares name (PosnWrapper pos Declare_All) unitScopeList =
   -- ?? Warning for each declare overwritten by this?
   (name,PosnWrapper pos Declare_All) : filter (\(lstName,_) -> lstName /= name) unitScopeList
combineDeclares name (PosnWrapper pos declare) unitScopeList =
   if any (\(lstName,(PosnWrapper _ lstDeclare)) -> name == lstName && (declare == lstDeclare || lstDeclare == Declare_All)) unitScopeList
      then unitScopeList -- ?? Warning if element is already in scope
      else ((name,PosnWrapper pos declare):unitScopeList)
