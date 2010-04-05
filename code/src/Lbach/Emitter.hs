module Lbach.Emitter
where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core
import Lbach.Emitter.Expressions
import Lbach.Emitter.Control

emit :: Program -> String
emit (Program b) = emitBlock b

-- Turns an expression into the equvilent assembly
emit2 :: Assign -> String
emit2 expr= "section .data\n" ++ emitDataA expr 
            ++ "section .bss\n" ++ emitBssA expr 
            ++ "section .text\n" ++emitTextA expr