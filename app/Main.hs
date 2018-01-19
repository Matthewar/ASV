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
