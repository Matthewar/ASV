{-|
   Module      : Netlister.Convert.Scope
   Description : Converter to find scope of current parsetree position

   Uses library and use statements to find current scope for declarations
-}
module Netlister.Convert.Scope
         ( convertScope
         ) where

import qualified Data.Map.Strict as MapS
import Data.List (nub)
import Data.Char (toUpper)

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
         )
import Netlister.Types.Operators (convertOperator)
import Netlister.Builtin.Netlist as InitialNetlist (scope)

-- |Convert context items to scope
-- Takes context items (library/use statements) to scope
convertScope :: [WrappedContextItem] -> ScopeReturn
convertScope contextItems = convertScope' contextItems InitialNetlist.scope

-- |Internal: Convert context items to scope
-- Takes context items, initial scope and build scope
convertScope' :: [WrappedContextItem] -> Scope -> ScopeReturn
convertScope' (contextItem:otherItems) currentScope = do
   let itemInfo = unPos contextItem
   newScope <- case itemInfo of
                  Context_LibraryClause (LibraryClause libNames) ->
                     addLibraries libNames currentScope
                  Context_UseClause (UseClause selectedNames) ->
                     addDeclarations selectedNames currentScope
   convertScope' otherItems newScope
convertScope' [] currentScope = Right currentScope

-- |Add library to the scope
-- Take library names, initial scope and return updated scope
addLibraries :: [WrappedSimpleName] -> Scope -> ScopeReturn
addLibraries (PosnWrapper pos libName:libs) currentScope =
   let upperLibName = map toUpper libName
   in if MapS.member upperLibName currentScope then addLibraries libs currentScope
      else
         if upperLibName == "IEEE" then
            addLibraries libs $ MapS.insert "IEEE" MapS.empty currentScope
         else -- ?? CURRENTLY DON'T ACCEPT CUSTOM LIBRARIES
            Left $ PosnWrapper { getPos = pos, unPos = ScopeConverterError_InvalidLibrary upperLibName }
addLibraries [] currentScope = Right currentScope

-- |Add declarations to the scope
-- Takes selected names, initial scope and return updated scope
addDeclarations :: [WrappedSelectedName] -> Scope -> ScopeReturn
addDeclarations
   (  (PosnWrapper _ (SelectedName
         (PosnWrapper _ (Prefix_Name (Name_Selected (SelectedName
            (PosnWrapper libPos (Prefix_Name (Name_Simple libName)))
            (PosnWrapper packagePos (Suffix_Name packageName))
         ))))
         suffix
      ))
      :otherNames
   )
   currentScope = do
      let upperLibName = map toUpper libName
      validLibName <- if upperLibName == "IEEE" then return "IEEE"
                      else Left $ PosnWrapper libPos $ ScopeConverterError_InvalidLibrary upperLibName
      let upperPackageName = map toUpper packageName
      declareContents <- case suffix of
                           PosnWrapper _ (Suffix_Name str) -> return $ NewDeclare_Identifier str
                           PosnWrapper pos (Suffix_Operator opStr) ->
                              case convertOperator opStr of
                                 Just op -> return $ NewDeclare_Operator op
                                 Nothing -> Left $ PosnWrapper pos $ ScopeConverterError_InvalidOperator opStr
                           PosnWrapper _ Suffix_All -> return NewDeclare_All
                           PosnWrapper pos (Suffix_Char chr) ->
                              Left $ PosnWrapper pos $ ScopeConverterError_SuffixChar chr
      newScope <- case MapS.lookup validLibName currentScope of
                     Just unitScope ->
                        if MapS.member upperPackageName unitScope then
                           let updateVal = MapS.adjust (combineDeclares declareContents) upperPackageName
                           in return $ MapS.adjust updateVal validLibName currentScope
                        else
                           let newVal = case declareContents of
                                          NewDeclare_Identifier str ->
                                             IncludedDeclares [Declare_Identifier str]
                                          NewDeclare_Operator op -> 
                                             IncludedDeclares [Declare_Operator op]
                                          NewDeclare_All -> AllDeclares
                               addVal = MapS.insert upperPackageName newVal
                           in return $ MapS.adjust addVal validLibName currentScope
                     Nothing -> Left $ PosnWrapper libPos $ ScopeConverterError_LibNoScope validLibName
      addDeclarations otherNames newScope
   where combineDeclares :: NewDeclarationScope -> DeclarationScope -> DeclarationScope
         combineDeclares NewDeclare_All _ = AllDeclares
         combineDeclares _ AllDeclares = AllDeclares
         combineDeclares newDeclare (IncludedDeclares lst) =
            let newVal = case newDeclare of
                           NewDeclare_Identifier str -> Declare_Identifier str
                           NewDeclare_Operator op -> Declare_Operator op
            in IncludedDeclares $ nub (newVal:lst)
addDeclarations [] scope = Right scope
addDeclarations (PosnWrapper pos selectedName:_) _ =
   Left $ PosnWrapper { getPos = pos, unPos = ScopeConverterError_UseClause selectedName }
