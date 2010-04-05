module Lbach.Emitter
    (module Lbach.Emitter.Control, emit, emitP)
where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core
import Lbach.Emitter.Expressions
import Lbach.Emitter.Control

emitP :: Program -> String
emitP (Program b) = emitBlock b

-- Turns an expression into the equvilent assembly
emit :: Assign -> String
emit expr= "section .data\n" ++ emitDataA expr 
            ++ "section .bss\n" ++ emitBssA expr 
            ++ "section .text\n" ++emitTextA expr