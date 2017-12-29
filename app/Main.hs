module Main where

import qualified Parser.Lexer as Lexer

--main :: IO ()
main :: IO ()
main = do
   s <- getContents
   print $ Lexer.lexerList s
