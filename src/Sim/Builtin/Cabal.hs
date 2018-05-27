module Sim.Builtin.Cabal
   ( cabalPrefix
   , cabalSuffix
   ) where

import Data.Text

cabalPrefix :: Text
cabalPrefix = pack
   "name:                VHDL-Sim\n\
   \version:             0.1.0.0\n\
   \-- synopsis:\n\
   \-- description:\n\
   \category:            Simulation\n\
   \build-type:          Simple\n\
   \--extra-source-files:  README.md\n\
   \cabal-version:       >=1.10\n\
   \\n\
   \library\n\
   \  hs-source-dirs:      src\n\
   \  exposed-modules:     Top\n\
   \  other-modules:       Control\n\
   \                     , STD.STANDARD"

cabalSuffix :: Text
cabalSuffix = pack
   "\n  build-depends:       base >= 4.7 && < 5\n\
   \                     , mtl\n\
   \                     , containers\n\
   \                     , transformers\n\
   \  default-language:    Haskell2010\n\
   \\n\
   \executable VHDL-Sim-exe\n\
   \  hs-source-dirs:      app\n\
   \  main-is:             Main.hs\n\
   \  other-modules:       \n\
   \  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\n\
   \  build-depends:       base\n\
   \                     , VHDL-Sim\n\
   \--                     , optparse-applicative\n\
   \  default-language:    Haskell2010"
