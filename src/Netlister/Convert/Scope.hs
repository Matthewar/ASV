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
         ( Scope
         , UnitScope
         , ScopeReturn
         , DeclarationScope(..)
         , DeclarationNames(..)
         , NewDeclarationScope(..)
         , WrappedNewDeclarationScope
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
convertScope contextItems = let contextOrder = reverse contextItems
                                conversion = convertScope' contextOrder
                            in execStateT conversion InitialNetlist.scope

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
   libInScope <- gets $ MapS.member upperLibName
   unless libInScope $ modify $ MapS.insert upperLibName MapS.empty
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
         Suffix_Name str -> return $ NewDeclare_Identifier str
         Suffix_Operator opStr -> case convertOperator opStr of
            Just op -> return $ NewDeclare_Operator op
            Nothing -> throwError $ PosnWrapper suffixPos $ ScopeConverterError_InvalidOperator opStr
         Suffix_All -> return NewDeclare_All
         Suffix_Char chr -> throwError $ PosnWrapper suffixPos $ ScopeConverterError_SuffixChar chr
      let wrappedDeclareContents = PosnWrapper suffixPos declareContents
      newScope <- gets $ MapS.lookup upperLibName
      unitScope <- case newScope of
         Just unitScope -> return unitScope
         Nothing -> throwError $ PosnWrapper libPos $ ScopeConverterError_LibNoScope upperLibName
      unitInScope <- gets $ MapS.member upperPackageName
      let innerUpdate = if unitInScope
                           then MapS.adjust (combineDeclares wrappedDeclareContents) upperPackageName
                           else MapS.insert upperPackageName $
                                 case declareContents of
                                    NewDeclare_Identifier str ->
                                       IncludedDeclares [(Declare_Identifier str,suffixPos)]
                                    NewDeclare_Operator op ->
                                       IncludedDeclares [(Declare_Operator op,suffixPos)]
                                    NewDeclare_All -> AllDeclares
      modify $ MapS.adjust innerUpdate upperLibName
      addDeclarations otherNames
addDeclarations [] = return ()
addDeclarations (PosnWrapper pos selectedName:_) =
   throwError $ PosnWrapper pos $ ScopeConverterError_UseClause selectedName

-- |Combine declarations in declaration scope
combineDeclares :: WrappedNewDeclarationScope -> DeclarationScope -> DeclarationScope
combineDeclares (PosnWrapper _ NewDeclare_All) _ = AllDeclares
combineDeclares _ AllDeclares = AllDeclares
combineDeclares (PosnWrapper pos newDeclare) (IncludedDeclares lst) =
   let newVal = case newDeclare of
                  NewDeclare_Identifier str -> Declare_Identifier str
                  NewDeclare_Operator op -> Declare_Operator op
   in IncludedDeclares $
         if any (\(val,_) -> val == newVal) lst
            then lst
            else ((newVal,pos):lst)

-- |Secondary state stores scoped conversions
-- Scoped conversions are converted modules required by the scope
type BuildScope = StateT ScopeStore (StateT NetlistStore (ExceptT ConverterError IO)) ()

-- |Evaluate scope
-- Run through scope, evaluating (IE converting) any modules not yet parsed/converted
evalScope :: (String -> String -> ConversionStack()) -> Scope -> ConversionStack ScopeStore
evalScope create scope = do
   let evalScope' :: [(String,UnitScope)] -> BuildScope
       evalScope' = evalScopeList create
   -- initialScope <- gets $ \(NetlistStore _) -> NetlistStore MapS.empty -- ?? Keep entities, just empty packages
   execStateT (evalScope' $ MapS.toList scope) emptyScopeStore -- ?? Need to order scope correctly
   -- ?? Combine entities from original scope -- is this even needed

-- |Evaluate scope list
-- Run through scope list, evaluating any units not yet parsed/converted
evalScopeList :: (String -> String -> ConversionStack ()) -> [(String,UnitScope)] -> BuildScope
evalScopeList create ((libName,unitScope):libScope) = do
  let evalUnitScope' :: [(String,DeclarationScope)] -> BuildScope
      evalUnitScope' = evalUnitScope create libName
  evalUnitScope' $ MapS.toList unitScope
  evalScopeList create libScope
evalScopeList _ [] = return ()

-- |Evaluate unit scope
-- Run through unit scope, evaluating any units not yet parsed/converted
evalUnitScope :: (String -> String -> ConversionStack ()) -> String -> [(String,DeclarationScope)] -> BuildScope
evalUnitScope create libName ((unitName,scope):otherUnits) = do
   let netlistName = NetlistName libName unitName
       isInScope :: NetlistStore -> Bool
       isInScope = (MapS.member netlistName) . packages
   -- ?? Add other checks
   inScope <- lift $ gets isInScope
   unless inScope $ lift $ create libName unitName
   newWrappedScope <- lift $ gets $ getPackageParts netlistName scope
   newScope <- lift $ lift $ withExceptT (ConverterError_Scope) $ liftEither newWrappedScope
   mergeScope newScope
   evalUnitScope create libName otherUnits
evalUnitScope _ _ [] = return ()

-- |Get scoped declares from stored netlist
-- Take relevant parts of included packages and put into scope
getPackageParts :: NetlistName -> DeclarationScope -> NetlistStore -> Either WrappedScopeConverterError ScopeStore
getPackageParts name declareScope netlistStore =
   let package = (packages netlistStore) MapS.! name
   in case declareScope of
         AllDeclares ->
            return $
               ScopeStore
                  { scopeFunctions = packageFunctions package
                  , scopeTypes = packageTypes package
                  }
         IncludedDeclares declares -> execStateT (getPackageDeclares package declares) emptyScopeStore

-- |Get scoped declares from package
-- Take relevant parts of package and put into scope
getPackageDeclares :: Package -> [(DeclarationNames,AlexPosn)] -> StateT ScopeStore (Either WrappedScopeConverterError) ()
getPackageDeclares exportPackage ((Declare_Operator op,pos):declares) = do
   let functions = packageFunctions exportPackage
       newFunctions = findPackageFunctionsByOperator op functions
       modifyScope newScope = newScope { scopeFunctions = MapS.union newFunctions $ scopeFunctions newScope }
   if MapS.null newFunctions
      then modify modifyScope
      else throwError $ PosnWrapper pos $ ScopeConverterError_InvalidOpDeclare op
   getPackageDeclares exportPackage declares
getPackageDeclares exportPackage ((Declare_Identifier name,pos):declares) = do
   checkTypes exportPackage name pos
   getPackageDeclares exportPackage declares
getPackageDeclares _ [] = return ()

-- |Get scoped declares from stored types
-- Check types of package and if relevant, put into scope
-- If not, check functions
checkTypes :: Package -> String -> AlexPosn -> StateT ScopeStore (Either WrappedScopeConverterError) ()
checkTypes exportPackage name pos =
   let modifyScope foundType newScope = newScope { scopeTypes = MapS.insert name foundType $ scopeTypes newScope }
   in case MapS.lookup name $ packageTypes exportPackage of
         Just foundType -> modify $ modifyScope foundType
         Nothing -> checkFunctions exportPackage name pos

-- |Get scoped declares from stored functions
-- Check functions of package and if relevant, put into scope
-- If not, throw error
-- ?? If not, check <next declares type>
checkFunctions :: Package -> String -> AlexPosn -> StateT ScopeStore (Either WrappedScopeConverterError) ()
checkFunctions exportPackage name pos =
   let newFunctions = findPackageFunctionsByIdentifier name $ packageFunctions exportPackage
       modifyScope newScope = newScope { scopeFunctions = MapS.union newFunctions $ scopeFunctions newScope }
   in if not $ MapS.null newFunctions
         then modify modifyScope
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
