{-|
   Module      : Netlister.Convert.Scope
   Description : Converter to find scope of current parsetree position

   Uses library and use statements to find current scope for declarations
-}
module Netlister.Convert.Scope
         ( convertScope
         , evalScope
         ) where

import qualified Data.Map.Strict as MapS
import Data.List (nub)
import Data.Char (toUpper)
import Control.Monad (unless)
import Control.Monad.Trans.State
         ( StateT
         , execStateT
         , modify
         , gets
         )
import Control.Monad.Except
         ( ExceptT
         , throwError
         , lift
         , liftEither
         , withExceptT
         )

import Parser.Alex.BaseTypes (AlexPosn)
import Parser.Happy.Types
         ( WrappedContextItem
         , ContextItem(..)
         , LibraryClause(..)
         , UseClause(..)
         , WrappedSimpleName
         , WrappedSelectedName
         , SelectedName(..)
         , Prefix(..)
         , Suffix(..)
         , Name(..)
         )
import Parser.PositionWrapper
import Netlister.Types.Scope
         ( Scope(..)
         , UnitScope
         , WrappedDeclarationScopeItem
         , DeclarationScopeItem(..)
         , ScopeReturn
         , ScopeConverterError(..)
         , WrappedScopeConverterError
         )
import Netlister.Types.Operators
         ( Operator
         , convertOperator
         )
import Netlister.Types.Top
         ( ConverterError(ConverterError_Scope)
         , ConversionStack
         )
import Netlister.Types.Stores
         ( NetlistStore(..)
         , NetlistName(..)
         , Package(..)
         , FunctionStore
         , ScopeStore(..)
         , ScopeStore(..)
         , emptyScopeStore
         )
import Netlister.Types.Representation
         ( Function(..)
         , FunctionBody
         , Designator(..)
         )
import Netlister.Builtin.Netlist as InitialNetlist (scope)

-- |Convert context items to scope
-- Takes context items (library/use statements) to scope
convertScope :: [WrappedContextItem] -> Either WrappedScopeConverterError Scope
convertScope contextItems = do
   let contextOrder = reverse contextItems
       conversion = convertScope' contextOrder
       reorderScopedDeclares scope = scope { scopeDeclarations = reverse $ scopeDeclarations scope }
   scope <- execStateT conversion InitialNetlist.scope
   return $ reorderScopedDeclares scope

-- |Internal: Convert context items to scope
-- Takes context items, initial scope and build scope
convertScope' :: [WrappedContextItem] -> ScopeReturn ()
convertScope' (PosnWrapper _ contextItem:otherContext) = do
   case contextItem of
      Context_LibraryClause (LibraryClause libNames) -> addLibraries $ reverse libNames
      Context_UseClause (UseClause selectedNames) -> addDeclarations $ reverse selectedNames
   convertScope' otherContext
convertScope' [] = return ()

-- |Add library to the scope
-- Take library names, initial scope and return updated scope
addLibraries :: [WrappedSimpleName] -> ScopeReturn ()
addLibraries (PosnWrapper pos libName:libs) = do
   let upperLibName = map toUpper libName
   unless (elem upperLibName ["IEEE","WORK","STD"]) $ throwError $ PosnWrapper pos $ ScopeConverterError_InvalidLibrary upperLibName -- ?? CURRENTLY DON'T ACCEPT CUSTOM LIBRARIES
   let isLibInScope = (elem upperLibName) . scopeLibraries
   libInScope <- gets isLibInScope
   let insertLibInScope scope = scope { scopeLibraries = (upperLibName:scopeLibraries scope) }
   unless libInScope $ modify insertLibInScope
   addLibraries libs
addLibraries [] = return ()

-- |Add declarations to the scope
-- Takes selected names, initial scope and return updated scope
addDeclarations :: [WrappedSelectedName] -> ScopeReturn ()
addDeclarations
   (  (PosnWrapper _ (SelectedName
         (PosnWrapper _ (Prefix_Name (Name_Selected (SelectedName
            (PosnWrapper libPos (Prefix_Name (Name_Simple libName)))
            (PosnWrapper packagePos (Suffix_Name packageName))
         ))))
         (PosnWrapper suffixPos suffix)
      ))
      :otherNames
   ) = do
      let upperLibName = map toUpper libName
      unless (elem upperLibName ["IEEE","WORK","STD"]) $ throwError $ PosnWrapper libPos $ ScopeConverterError_InvalidLibrary upperLibName -- ?? CURRENTLY DON'T ACCEPT CUSTOM LIBRARIES
      let upperPackageName = map toUpper packageName
      declareContents <- case suffix of
         Suffix_Name str -> return $ Declare_Identifier str
         Suffix_Operator opStr -> case convertOperator opStr of
            Just op -> return $ Declare_Operator op
            Nothing -> throwError $ PosnWrapper suffixPos $ ScopeConverterError_InvalidOperator opStr
         Suffix_All -> return Declare_All
         Suffix_Char chr -> throwError $ PosnWrapper suffixPos $ ScopeConverterError_SuffixChar chr
      let wrappedDeclareContents = PosnWrapper suffixPos declareContents
      let isLibInScope = (elem upperLibName) . scopeLibraries
      libInScope <- gets isLibInScope
      unless libInScope $ throwError $ PosnWrapper libPos $ ScopeConverterError_LibNoScope upperLibName
      let netlistName = NetlistName upperLibName upperPackageName
          scopeItem = PosnWrapper suffixPos declareContents
      let updateDeclares scope = scope { scopeDeclarations = combineDeclares netlistName scopeItem $ scopeDeclarations scope }
      modify updateDeclares
      addDeclarations otherNames
addDeclarations [] = return ()
addDeclarations (PosnWrapper pos selectedName:_) =
   throwError $ PosnWrapper pos $ ScopeConverterError_UseClause selectedName

-- |Combine declarations in declaration scope
combineDeclares :: NetlistName -> WrappedDeclarationScopeItem -> [UnitScope] -> [UnitScope]
combineDeclares name (PosnWrapper pos Declare_All) unitScopeList =
   -- ?? Warning for each declare overwritten by this?
   (name,PosnWrapper pos Declare_All) : filter (\(lstName,_) -> lstName /= name) unitScopeList
combineDeclares name (PosnWrapper pos declare) unitScopeList =
   if any (\(lstName,(PosnWrapper _ lstDeclare)) -> name == lstName && (declare == lstDeclare || lstDeclare == Declare_All)) unitScopeList
      then unitScopeList -- ?? Warning if element is already in scope
      else ((name,PosnWrapper pos declare):unitScopeList)

-- |Secondary state stores scoped conversions
-- Scoped conversions are converted modules required by the scope
type BuildScope = StateT ScopeStore (StateT NetlistStore (ExceptT ConverterError IO)) ()

-- |Evaluate scope
-- Run through scope, evaluating (IE converting) any modules not yet parsed/converted
evalScope :: (String -> String -> ConversionStack()) -> Scope -> ConversionStack ScopeStore
evalScope create scope = do
   let evalScope' :: [UnitScope] -> BuildScope
       evalScope' = evalScopeList create
   -- initialScope <- gets $ \(NetlistStore _) -> NetlistStore MapS.empty -- ?? Keep entities, just empty packages
   execStateT (evalScope' $ scopeDeclarations scope) emptyScopeStore
   -- ?? Combine entities from original scope -- is this even needed

-- |Evaluate scope list
-- Run through scope list, evaluating any units not yet parsed/converted
evalScopeList :: (String -> String -> ConversionStack ()) -> [UnitScope] -> BuildScope
evalScopeList create ((NetlistName libName unitName,scopeItem):otherUnits) = do
   let netlistName = NetlistName libName unitName
       isInScope :: NetlistStore -> Bool
       isInScope = (MapS.member netlistName) . packages
   -- ?? Add other checks
   inScope <- lift $ gets isInScope
   unless inScope $ lift $ create libName unitName
   newWrappedScope <- lift $ gets $ getPackageParts netlistName scopeItem
   newScope <- lift $ lift $ withExceptT (ConverterError_Scope) $ liftEither newWrappedScope
   mergeScope newScope
   evalScopeList create otherUnits
evalScopeList _ [] = return ()

-- |Get scoped declares from stored netlist
-- Take relevant parts of included packages and put into scope
getPackageParts :: NetlistName -> WrappedDeclarationScopeItem -> NetlistStore -> Either WrappedScopeConverterError ScopeStore
getPackageParts name scopeItem netlistStore =
   let package = (packages netlistStore) MapS.! name
   in case scopeItem of
         PosnWrapper _ Declare_All ->
            return $
               ScopeStore
                  { scopeFunctions = packageFunctions package
                  , scopeTypes = packageTypes package
                  }
         PosnWrapper pos (Declare_Operator op) ->
            let newFunctions = findPackageFunctionsByOperator op $ packageFunctions package
            in if MapS.null newFunctions
                  then return $ ScopeStore newFunctions MapS.empty
                  else throwError $ PosnWrapper pos $ ScopeConverterError_InvalidOpDeclare op
         PosnWrapper pos (Declare_Identifier name) -> checkTypes package name pos

-- |Get scoped declares from stored types
-- Check types of package and if relevant, put into scope
-- If not, check functions
checkTypes :: Package -> String -> AlexPosn -> Either WrappedScopeConverterError ScopeStore
checkTypes package name pos =
   case MapS.lookup name $ packageTypes package of
      Just foundType -> return $ ScopeStore MapS.empty $ MapS.fromList [(name,foundType)]
      Nothing -> checkFunctions package name pos

-- |Get scoped declares from stored functions
-- Check functions of package and if relevant, put into scope
-- If not, throw error
-- ?? If not, check <next declares type>
checkFunctions :: Package -> String -> AlexPosn -> Either WrappedScopeConverterError ScopeStore
checkFunctions package name pos =
   let newFunctions = findPackageFunctionsByIdentifier name $ packageFunctions package
   in if not $ MapS.null newFunctions
         then return $ ScopeStore newFunctions MapS.empty
         else throwError $ PosnWrapper pos $ ScopeConverterError_InvalidDeclare name -- ?? Continue from here once expand package/scope definition

-- |Find an operator function
findPackageFunctionsByOperator :: Operator -> FunctionStore -> FunctionStore
findPackageFunctionsByOperator expectedOp =
   let functionFind :: Function -> (Maybe FunctionBody) -> Bool
       functionFind (Function (Designator_Operator op) _ _) _ = op == expectedOp
       functionFind _ _ = False
   in MapS.filterWithKey functionFind

-- |Find a (non-operator) function
findPackageFunctionsByIdentifier :: String -> FunctionStore -> FunctionStore
findPackageFunctionsByIdentifier expectedName =
   let functionFind :: Function -> (Maybe FunctionBody) -> Bool
       functionFind (Function (Designator_Identifier name) _ _) _ = name == expectedName
       functionFind _ _ = False
   in MapS.filterWithKey functionFind

-- ?? Loss of order may cause package to overwrite parts
-- | Merge scope from current unit with total scope
mergeScope :: ScopeStore -> BuildScope
mergeScope (ScopeStore newFuncs newTypes) =
   let modifyScope (ScopeStore oldFuncs oldTypes) =
         ScopeStore
            (MapS.union newFuncs oldFuncs)
            (MapS.union newTypes oldTypes)
   in modify modifyScope
