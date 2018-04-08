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
import Control.Monad
         ( unless
         , when
         )
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
         , NewScopeDeclares(..)
         , WrappedNewScopeDeclares
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
import Netlister.Functions.Stores (convertPackageToScope)
import Netlister.Types.Representation
         ( Function(..)
         , FunctionBody
         , Designator(..)
         , Type
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
evalScope :: (String -> String -> ConversionStack()) -> Scope -> [NetlistName] -> ConversionStack ScopeStore
evalScope create scope dependencies = do
   let evalScope' :: [UnitScope] -> BuildScope
       evalScope' = evalScopeList create dependencies
   -- initialScope <- gets $ \(NetlistStore _) -> NetlistStore MapS.empty -- ?? Keep entities, just empty packages
   execStateT (evalScope' $ scopeDeclarations scope) emptyScopeStore
   -- ?? Combine entities from original scope -- is this even needed

-- |Evaluate scope list
-- Run through scope list, evaluating any units not yet parsed/converted
evalScopeList :: (String -> String -> ConversionStack ()) -> [NetlistName] -> [UnitScope] -> BuildScope
evalScopeList create dependencies ((NetlistName libName unitName,scopeItem):otherUnits) = do
   let netlistName = NetlistName libName unitName
       isInScope :: NetlistStore -> Bool
       isInScope = (MapS.member netlistName) . packages
       isDependency = elem netlistName dependencies
   when isDependency $ throwError $ ConverterError_Scope $ PosnWrapper (getPos scopeItem) $ ScopeConverterError_CyclicDependency netlistName dependencies
   inScope <- lift $ gets isInScope
   unless inScope $ lift $ create libName unitName
   newWrappedScopeDeclare <- lift $ gets $ getPackageParts netlistName scopeItem
   newScopeDeclare <- lift $ lift $ withExceptT (ConverterError_Scope) $ liftEither newWrappedScopeDeclare
   mergeScope newScopeDeclare
   evalScopeList create dependencies otherUnits
evalScopeList _ _ [] = return ()

-- |Get scoped declares from stored netlist
-- Take relevant parts of included packages and put into scope
getPackageParts :: NetlistName -> WrappedDeclarationScopeItem -> NetlistStore -> Either WrappedScopeConverterError WrappedNewScopeDeclares
getPackageParts packageName scopeItem netlistStore =
   let package = (packages netlistStore) MapS.! packageName
   in case scopeItem of
         PosnWrapper pos Declare_All ->
            return $ PosnWrapper pos $ NewScopeDeclare_Package $ convertPackageToScope package
         PosnWrapper pos (Declare_Operator op) ->
            let newFunctions = findPackageFunctionsByOperator op $ packageFunctions package
            in if MapS.null newFunctions
                  then return $ PosnWrapper pos $ NewScopeDeclare_Functions newFunctions
                  else throwError $ PosnWrapper pos $ ScopeConverterError_InvalidOpDeclare op packageName
         PosnWrapper pos (Declare_Identifier name) -> checkTypes packageName package name pos

-- |Get scoped declares from stored types
-- Check types of package and if relevant, put into scope
-- If not, check functions
checkTypes :: NetlistName -> Package -> String -> AlexPosn -> Either WrappedScopeConverterError WrappedNewScopeDeclares
checkTypes packageName package name pos =
   case MapS.lookup name $ packageTypes package of
      Just foundType -> return $ PosnWrapper pos $ NewScopeDeclare_Type name foundType
      Nothing -> checkFunctions packageName package name pos

-- |Get scoped declares from stored functions
-- Check functions of package and if relevant, put into scope
-- If not, throw error
-- ?? If not, check <next declares type>
checkFunctions :: NetlistName -> Package -> String -> AlexPosn -> Either WrappedScopeConverterError WrappedNewScopeDeclares
checkFunctions packageName package name pos =
   let newFunctions = findPackageFunctionsByIdentifier name $ packageFunctions package
   in if not $ MapS.null newFunctions
         then return $ PosnWrapper pos $ NewScopeDeclare_Functions newFunctions
         else throwError $ PosnWrapper pos $ ScopeConverterError_InvalidDeclare name packageName -- ?? Continue from here once expand package/scope definition

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

-- | Merge scope from current unit with total scope
mergeScope :: WrappedNewScopeDeclares -> BuildScope
mergeScope (PosnWrapper pos (NewScopeDeclare_Package newScope)) =
   -- ?? Should be checking for overlaps and warning
   let modifyScope oldScope =
         ScopeStore
            (MapS.union (scopeFunctions newScope) (scopeFunctions oldScope))
            (MapS.union (scopeTypes newScope) (scopeTypes oldScope))
            (MapS.union (scopeSubtypes newScope) (scopeSubtypes oldScope))
            (MapS.union (scopeConstants newScope) (scopeConstants oldScope))
            (MapS.union (scopeSignals newScope) (scopeSignals oldScope))
   in modify modifyScope
mergeScope (PosnWrapper pos (NewScopeDeclare_Functions newFuncs)) = mergeScopeFuncs $ PosnWrapper pos newFuncs
mergeScope (PosnWrapper pos (NewScopeDeclare_Type typeName typeDeclare)) = mergeScopeType $ PosnWrapper pos (typeName,typeDeclare)

-- |Merge a single type into the scope
mergeScopeFuncs :: PosnWrapper FunctionStore -> BuildScope
mergeScopeFuncs (PosnWrapper pos newFuncs) =
   -- ?? Should be checking for overlaps and warning
   let modifyScope oldScope = oldScope { scopeFunctions = MapS.union newFuncs $ scopeFunctions oldScope }
   in modify modifyScope

-- |Merge a single type into the scope
mergeScopeType :: PosnWrapper (String,Type) -> BuildScope
mergeScopeType (PosnWrapper pos (name,declare)) = do
   let readScopeTypes = scopeTypes
   isInScope <- gets ((MapS.member name) . readScopeTypes)
   -- ?? when isInScope $ ?? Warning?
   let modifyScope oldScope = oldScope { scopeTypes = MapS.insert name declare $ scopeTypes oldScope }
   modify modifyScope
