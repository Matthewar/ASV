module Parser.Functions.IdentifyName
   ( matchSuffix
   ) where

import Control.Monad.Trans.State (StateT)
import Control.Monad.Except
         ( ExceptT
         , throwError
         )

import Manager.Types.Error (ConverterError(..))
import qualified Lexer.Types.Token as Tokens
import Lexer.Types.PositionWrapper
import Lexer.Types.Error (ParserError(..))
import Lexer.Alex.Types (AlexState)
import Lexer.Functions.PositionWrapper
import Parser.Netlist.Types.Scope
         ( Scope
         , DeclarationScopeItem(..)
         )
import Parser.Netlist.Types.Stores (NetlistStore)
import Parser.Netlist.Types.Operators (convertOperator)

matchSuffix :: PosnWrapper Tokens.Token -> StateT Scope (StateT AlexState (StateT NetlistStore (ExceptT ConverterError IO))) DeclarationScopeItem
matchSuffix (PosnWrapper _ (Tokens.Identifier iden)) = return $ Declare_Identifier iden
matchSuffix (PosnWrapper pos (Tokens.Literal (Tokens.Str str))) =
   case convertOperator str of
      Just op -> return $ Declare_Operator op
      Nothing -> throwError $ ConverterError_Parse $ PosnWrapper pos $ ParseErr_ExpectedOperatorInUseClause str
matchSuffix (PosnWrapper _ (Tokens.Keyword Tokens.All)) = return Declare_All
matchSuffix token = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSuffixInUseClause token
