module Parser.Functions.IdentifyName
   ( matchSuffix
   ) where

import Control.Monad.Except (throwError)

import Manager.Types.Error (ConverterError(ConverterError_Parse))
import qualified Lexer.Types.Token as Tokens
import Lexer.Types.PositionWrapper
import Lexer.Types.Error (ParserError(..))
import Lexer.Functions.PositionWrapper (raisePosition)
import Parser.Types.Monad (ScopeStack)
import Parser.Netlist.Types.Scope (DeclarationScopeItem(..))
import Parser.Netlist.Types.Operators (convertOperator)

matchSuffix :: Tokens.WrappedToken -> ScopeStack DeclarationScopeItem
matchSuffix (PosnWrapper _ (Tokens.Identifier iden)) = return $ Declare_Identifier iden
matchSuffix (PosnWrapper pos (Tokens.Literal (Tokens.Str str))) =
   case convertOperator str of
      Just op -> return $ Declare_Operator op
      Nothing -> throwError $ ConverterError_Parse $ PosnWrapper pos $ ParseErr_ExpectedOperatorInUseClause str
matchSuffix (PosnWrapper _ (Tokens.Keyword Tokens.All)) = return Declare_All
matchSuffix token = throwError $ ConverterError_Parse $ raisePosition ParseErr_ExpectedSuffixInUseClause token
