module Main where

import StringBuffer
import Editor
import JoinListBuffer
import Buffer

main = runEditor editor buffer
    where buffer :: JLBuffer
          buffer = fromString $ unlines [ "This buffer is for notes you don't want to save, and for"
            , "evaluation of steam valve coefficients."
            , "To load a different file, type the character L followed"
            , "by the name of the file."
            ]
