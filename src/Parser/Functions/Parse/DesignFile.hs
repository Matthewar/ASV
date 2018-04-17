module Parser.Functions.Parse.DesignFile
   ( parseDesignFile
   ) where

import Control.Monad.Trans.State
         ( StateT
         , execStateT
         )
import Control.Monad.Except
         ( ExceptT
         , lift
         )

import Lexer.Types.Monad (Alex)
import Parser.Functions.Parse.Context
import Parser.Functions.Convert.Scope (evalScope)
import Parser.Netlist.Types.Stores
         ( NetlistName
         , NetlistStore
         )
import Parser.Netlist.Builtin.Netlist as InitialNetlist (scope)
import Manager.Types.Error (ConverterError)

parseDesignFile :: (String -> String -> StateT NetlistStore (ExceptT ConverterError IO) ()) -> String -> [NetlistName] -> Alex ()
--parseDesignFile create libraryName dependencies =
parseDesignFile = parseDesignUnit

parseDesignUnit :: (String -> String -> StateT NetlistStore (ExceptT ConverterError IO) ()) -> String -> [NetlistName] -> Alex ()
parseDesignUnit create libraryName dependencies = do
   scope <- execStateT parseContext InitialNetlist.scope
   realScope <- lift $ evalScope create scope dependencies
   return ()
