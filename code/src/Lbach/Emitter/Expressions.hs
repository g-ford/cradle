module Lbach.Emitter.Expressions where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core

-- Generates the contents of section .text
emitText :: Expression -> String
emitText expr = case expr of 
     Num a      -> emitLn ("MOV eax, " ++ (show a))
     Add a b    -> emitText a ++  pushEax ++ emitText b ++ add
     Sub a b    -> emitText a ++  pushEax ++ emitText b ++ sub
     Mul a b    -> emitText a ++  pushEax ++ emitText b ++ mul
     Div a b    -> emitText a ++  pushEax ++ emitText b ++ divide
     Var a      -> emitLn ("MOV eax, [" ++ a ++ "]")
     
emitTextA :: Assign -> String
emitTextA (Assign a b) = emitText b ++ emitLn ("MOV [" ++ a ++ "], eax")
     
-- Generates the contents of section .data
emitData :: Expression -> String
emitData expr = case expr of
    Var a       -> emitLn (a ++ "\tdd\t0")
    Add a b     -> emitData a ++ emitData b
    Sub a b     -> emitData a ++ emitData b
    Mul a b     -> emitData a ++ emitData b
    Div a b     -> emitData a ++ emitData b
    otherwise   -> ""
    
emitDataA :: Assign -> String
emitDataA (Assign a b) = emitData b 
 
-- Generates the contents of section .bss
emitBss :: Expression -> String    
emitBss expr = case expr of
    Add a b     -> emitBss a ++ emitBss b
    Sub a b     -> emitBss a ++ emitBss b
    Mul a b     -> emitBss a ++ emitBss b
    Div a b     -> emitBss a ++ emitBss b
    otherwise   -> ""
    
emitBssA :: Assign -> String
emitBssA (Assign a b) = emitLn (a ++ "\tresd") ++ emitBss b