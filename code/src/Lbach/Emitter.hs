module Lbach.Emitter
where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core
import Lbach.Emitter.Expressions

-- Turns an expression into the equvilent assembly
emit :: Assign -> String
emit expr= "section .data\n" ++ emitDataA expr 
            ++ "section .bss\n" ++ emitBssA expr 
            ++ "section .text\n" ++emitTextA expr