module Main where

import Parser.ErrorTypes (printParserError)
import Parser.Alex.Functions (runAlex)
import qualified Parser.Parser as Parser

main :: IO ()
main = do
   s <- getContents
   print $ case runAlex s Parser.v1987 of
      Left err -> printParserError err
      Right result -> show result
--
--import Options.Applicative
--import Data.Semigroup ((<>))
--
--import Manager.Args
--import qualified Manager.Filing as Filing
--
--main :: IO ()
--main = control =<< execParser opts
--   where opts = info (optionsGroup <**> helper)
--             ( fullDesc
--            <> progDesc "VHDL Simulator"
--            <> header "VHDL Simulator - To create simulation binaries for VHDL"
--             )
--
--control :: Options -> IO ()
--control options = do
--   Filing.checkArguments options
--   Design.createTop options
