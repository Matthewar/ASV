module Main where

--import qualified Parser.Lexer as Lexer
import qualified Parser.Parser as Parser

--main :: IO ()
main :: IO ()
main = do
   s <- getContents
   --print $ Lexer.lexerList s
   print $ Parser.parse s
