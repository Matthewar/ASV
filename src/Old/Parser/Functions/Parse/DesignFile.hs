module Parser.Functions.Parse.DesignFile
   ( parseDesignFile
   ) where

import Control.Monad (unless)
import Control.Monad.Trans.State
         ( execStateT
         , evalStateT
         )

import Lexer.Types.Monad (Alex)
import Parser.Types.Monad (ParserStack)
import Parser.Functions.Monad
         ( getToken
         , saveToken
         )
import Parser.Functions.Monad (accessNetlist)
import Parser.Functions.IdentifyToken (isEOF)
import Parser.Functions.Parse.Context (parseContext)
import Parser.Functions.Parse.Library (parseLibrary)
import Parser.Functions.Convert.Scope (evalScope)
import Parser.Netlist.Types.Representation (NetlistName)
import Parser.Netlist.Types.Monad (NetlistStack)
import Parser.Netlist.Builtin.Netlist as InitialNetlist (scope)

parseDesignFile :: (String -> String -> NetlistStack ()) -> String -> [NetlistName] -> Alex ()
--parseDesignFile create libraryName dependencies =
parseDesignFile create libraryName dependencies = evalStateT (parseDesignUnit create libraryName dependencies) []

parseDesignUnit :: (String -> String -> NetlistStack ()) -> String -> [NetlistName] -> ParserStack ()
parseDesignUnit create libraryName dependencies = do
   scope <- execStateT parseContext InitialNetlist.scope
   realScope <- accessNetlist $ evalScope create scope dependencies
   parseLibrary realScope libraryName
   endToken <- getToken
   unless (isEOF endToken) $ do
      saveToken endToken
      parseDesignUnit create libraryName dependencies
