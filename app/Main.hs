module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Manager.Args
import Manager.NewDesignUnit (createTop)

main :: IO ()
main = createTop =<< execParser opts
   where opts = info (optionsGroup <**> helper)
             ( fullDesc
            <> progDesc "VHDL Simulator"
            <> header "VHDL Simulator - To create simulation binaries for VHDL"
             )
