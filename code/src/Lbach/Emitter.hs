module Lbach.Emitter
where

import Lbach.Grammar.Basics

-- Prefix a string with a tab
emitSt s = "\t" ++ s
 
-- Prefix a string with a tab and postfix it with a new line
emitLn s = (emitSt s) ++ "\n"
 
-- Turns an expression into the equvilent assembly
emit :: Assign -> String
emit expr= "section .data\n" ++ emitDataA expr 
            ++ "section .bss\n" ++ emitBssA expr 
            ++ "section .text\n" ++emitTextA expr
 
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
 
-- Basic math functions
popEbx = emitLn "POP ebx"
popEax = emitLn "POP eax"
pushEax = emitLn "PUSH eax"  
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
mul = popEbx ++ emitLn "MUL ebx"
divide = emitLn "MOV ebx, eax" ++ popEax ++ emitLn "MOV edx, 0" ++ emitLn "DIV ebx"