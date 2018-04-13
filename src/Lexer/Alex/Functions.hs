{-|
   Module      : Parser.Alex.Functions
   Description : Alex specific lexer functions

   Adapted from Alex default wrapper file.
   See [Alex](https://www.haskell.org/alex/)
|-}
module Lexer.Alex.Functions where

import Data.Char (ord)
import qualified Data.Bits
import Control.Monad.Trans.State
         ( evalStateT
         , gets
         , modify
         )
import Control.Monad.Except
         ( ExceptT
         , throwError
         )

import Lexer.Types.Error
import Lexer.Types.Monad (Alex(..))
import Lexer.Alex.Types

-- | Encode a Haskell String to a list of Byte values, in UTF8 format.
utf8Encode :: Char -> [Byte]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

-- | Remove pending bytes on current character
ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_,s) = (p,c,[],s)

-- | Get previous character from alex input
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

-- | Get next byte from alex input
alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

-- | Gives the position of the start of the file
alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

-- | Size of a tab for counter
alex_tab_size :: Int
alex_tab_size = 4

-- | Calculates the new position after traversing a given character, assuming the usual eight character tab stops.
alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

-- `eof_pos' a standard encoding for the end of file.

-- ?? Compile with -funbox-strict-fields for best results!

-- | Take in input a string and run the lexer on it
runAlex :: String -> Alex a -> ExceptT WrappedParserError IO a
runAlex input__ f =
   evalStateT f $
      (AlexState {alex_pos = alexStartPos,
                  alex_inp = input__,
                  alex_chr = '\n',
                  alex_bytes = [],
                  alex_scd = 0})

-- | Get lexer input state
alexGetInput :: Alex AlexInput
alexGetInput = gets $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} -> (pos,c,bs,inp__)

-- | Get the state of the lexer
alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp__) =
   modify $ \s -> s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__}

-- | Return lexer error
alexError :: WrappedParserError -> Alex a
alexError err = throwError err

-- | Get lexer current startcode
alexGetStartCode :: Alex Int
alexGetStartCode = gets $ \s@AlexState{alex_scd=sc} -> sc

-- | Set lexer startcode
alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = modify $ \s -> s{alex_scd=sc}
