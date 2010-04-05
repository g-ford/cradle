module Lbach.Emitter
    (module Lbach.Emitter.Control, emit)
where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core
import Lbach.Emitter.Expressions
import Lbach.Emitter.Control

-- Turns an expression into the equvilent assembly
emit :: Assign -> String
emit expr= "section .data\n" ++ emitDataA expr 
            ++ "section .bss\n" ++ emitBssA expr 
            ++ "section .text\n" ++emitTextA expr