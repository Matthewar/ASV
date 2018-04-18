{-|
   Module      : Parser.Functions.Convert.Scope
   Description : Converter to find scope of current parser position

   Uses library and use statements to find current scope for declarations
-}
module Parser.Functions.Convert.Scope
         ( evalScope
         ) where

import qualified Data.Map.Strict as MapS
import Control.Monad
         ( unless
         , when
         )
import Control.Monad.Trans.State
         ( execStateT
         , modify
         , gets
         )
import Control.Monad.Except
         ( throwError
         , lift
         , liftEither
         , withExceptT
         )

import Lexer.Types.PositionWrapper
import Lexer.Functions.PositionWrapper (passPosition)
import Lexer.Alex.Types (AlexPosn)
import Parser.Types.Monad (BuildScopeStack)
import Parser.Netlist.Types.Monad (NetlistStack)
import Parser.Netlist.Types.Scope
         ( Scope(..)
         , UnitScope
         , WrappedDeclarationScopeItem
         , DeclarationScopeItem(..)
         , NewScopeDeclares(..)
         , WrappedNewScopeDeclares
         , ScopeConverterError(..)
         , WrappedScopeConverterError
         )
import Parser.Netlist.Types.Operators (Operator)
import Parser.Netlist.Types.Stores
         ( NetlistStore(..)
         , NetlistName(..)
         , Package(..)
         , FunctionStore
         , ScopeStore(..)
         , emptyScopeStore
         )
import Parser.Netlist.Functions.Stores (convertPackageToScope)
import Parser.Netlist.Types.Representation
         ( Function(..)
         , FunctionBody
         , Designator(..)
         , Type
         )
import Manager.Types.Error (ConverterError(ConverterError_Scope))

-- |Evaluate scope
-- Run through scope, evaluating (IE converting) any modules not yet parsed/converted
evalScope :: (String -> String -> NetlistStack ()) -> Scope -> [NetlistName] -> NetlistStack ScopeStore
evalScope create scope dependencies = do
   let evalScope' :: [UnitScope] -> BuildScopeStack
       evalScope' = evalScopeList create dependencies
   -- initialScope <- gets $ \(NetlistStore _) -> NetlistStore MapS.empty -- ?? Keep entities, just empty packages
   execStateT (evalScope' $ scopeDeclarations scope) emptyScopeStore
   -- ?? Combine entities from original scope -- is this even needed

-- |Evaluate scope list
-- Run through scope list, evaluating any units not yet parsed/converted
evalScopeList :: (String -> String -> NetlistStack ()) -> [NetlistName] -> [UnitScope] -> BuildScopeStack
evalScopeList create dependencies ((NetlistName libName unitName,scopeItem):otherUnits) = do
   let netlistName = NetlistName libName unitName
       isInScope :: NetlistStore -> Bool
       isInScope = (MapS.member netlistName) . packages
       isDependency = elem netlistName dependencies
   when isDependency $ throwError $ ConverterError_Scope $ passPosition (ScopeConverterError_CyclicDependency netlistName dependencies) scopeItem
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
mergeScope :: WrappedNewScopeDeclares -> BuildScopeStack
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
mergeScopeFuncs :: PosnWrapper FunctionStore -> BuildScopeStack
mergeScopeFuncs (PosnWrapper pos newFuncs) =
   -- ?? Should be checking for overlaps and warning
   let modifyScope oldScope = oldScope { scopeFunctions = MapS.union newFuncs $ scopeFunctions oldScope }
   in modify modifyScope

-- |Merge a single type into the scope
mergeScopeType :: PosnWrapper (String,Type) -> BuildScopeStack
mergeScopeType (PosnWrapper pos (name,declare)) = do
   let readScopeTypes = scopeTypes
   isInScope <- gets ((MapS.member name) . readScopeTypes)
   -- ?? when isInScope $ ?? Warning?
   let modifyScope oldScope = oldScope { scopeTypes = MapS.insert name declare $ scopeTypes oldScope }
   modify modifyScope
