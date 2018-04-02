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
import Control.Monad.Except (throwError)

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
         , ScopeReturn
         , DeclarationScope(..)
         , DeclarationNames(..)
         , NewDeclarationScope(..)
         , ScopeConverterError(..)
         , WrappedScopeConverterError
         )
import Netlister.Types.Operators (convertOperator)
import Netlister.Types.Top (ConversionStack)
import Netlister.Types.Stores
         ( NetlistStore(..)
         , NetlistName(..)
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
      newScope <- gets $ MapS.lookup upperLibName
      unitScope <- case newScope of
         Just unitScope -> return unitScope
         Nothing -> throwError $ PosnWrapper libPos $ ScopeConverterError_LibNoScope upperLibName
      unitInScope <- gets $ MapS.member upperPackageName
      let innerUpdate = if unitInScope
                           then MapS.adjust (combineDeclares declareContents) upperPackageName
                           else MapS.insert upperPackageName $
                                 case declareContents of
                                    NewDeclare_Identifier str ->
                                       IncludedDeclares [Declare_Identifier str]
                                    NewDeclare_Operator op -> 
                                       IncludedDeclares [Declare_Operator op]
                                    NewDeclare_All -> AllDeclares
      modify $ MapS.adjust innerUpdate upperLibName
      addDeclarations otherNames
addDeclarations [] = return ()
addDeclarations (PosnWrapper pos selectedName:_) =
   throwError $ PosnWrapper pos $ ScopeConverterError_UseClause selectedName

-- |Combine declarations in declaration scope
combineDeclares :: NewDeclarationScope -> DeclarationScope -> DeclarationScope
combineDeclares NewDeclare_All _ = AllDeclares
combineDeclares _ AllDeclares = AllDeclares
combineDeclares newDeclare (IncludedDeclares lst) =
   let newVal = case newDeclare of
                  NewDeclare_Identifier str -> Declare_Identifier str
                  NewDeclare_Operator op -> Declare_Operator op
   in IncludedDeclares $ nub (newVal:lst)

-- |Evaluate scope
-- Run through scope, evaluating (IE converting) any modules not yet parsed/converted
evalScope :: (String -> String -> ConversionStack()) -> Scope -> ConversionStack ()
evalScope create =
   let evalScope' ((libName,unitScope):libScope) =
         let createUnit :: String -> ConversionStack ()
             createUnit = create libName
             createNetlistName :: String -> NetlistName
             createNetlistName = NetlistName libName
             evalUnitScope :: [String] -> ConversionStack ()
             evalUnitScope (unitName:otherUnitNames) = do
               let netlistName = createNetlistName unitName
                   isInScope :: NetlistStore -> Bool
                   isInScope = (MapS.member netlistName) . packages
                   -- ?? Add other checks
               inScope <- gets isInScope
               unless inScope $ createUnit unitName
               evalUnitScope otherUnitNames
             evalUnitScope [] = return ()
         in do
               evalUnitScope $ MapS.keys unitScope
               evalScope' libScope
       evalScope' [] = return ()
   in evalScope' . MapS.toList

{-
-- |Convert scope
-- Take the scope, which is a list of names, and convert it to a readable store
buildRealScope :: Scope -> ConversionStack NetlistStore
buildRealScope scope =
   let scopeList = MapS.toList scope
       emptyNetlist (NetlistStore _) = NetlistStore MapS.empty -- ?? Keep entities, just empty packages
   in do
      initialNetlist <- gets emptyNetlist
      evalScope' scopeList initialNetlist
   where evalScope' :: [(String,UnitScope)] -> NetlistStore -> ConversionStack NetlistStore
         evalScope' ((libName,unitScope):scopes) netlistStore =
            let newNetlistStore = evalUnitScope libName (MapS.toList unitScope) netlistStore
            in evalScope' scopes newNetlistStore
         evalScope' [] netlistStore = return netlistStore
         evalUnitScope :: String -> [(String,DeclarationScope)] -> NetlistStore -> ConversionStack NetlistStore
         evalUnitScope libName ((unitName,AllDeclares):declares) netlistStore = do
            let name = NetlistName libName unitName
                getAll (NetlistStore packages) = packages MapS.! name
            newPackage <- gets getAll
            evalUnitScope libName declares $ MapS.insert name newPackage netlistStore
         evalUnitScope 
-}
