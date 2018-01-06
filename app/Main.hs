module Main where

import qualified Parser.Lexer as Lexer
import Parser.Alex.Functions (runAlex)
import qualified Parser.Parser as Parser

--main :: IO ()
main :: IO ()
main = do
   s <- getContents
   --print $ Lexer.lexerList s
   print $ show $
      --Lexer.lexer s >>= Parser.v1987
      runAlex s Parser.v1987
