module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Manager.Args
import Manager.Control (control)

main :: IO ()
main = control =<< execParser opts
   where opts = info (optionsGroup <**> helper)
             ( fullDesc
            <> progDesc "VHDL Simulator"
            <> header "VHDL Simulator - To create simulation binaries for VHDL"
             )
