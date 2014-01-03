module Cradle.Generator.Nasm

where

import Cradle.Grammar

emit :: Assign -> String
emit expr = externs 
		++ dataSection expr
        ++ bssSection expr 
        ++ textSection
        ++ mainSection expr	

emitDataA :: Assign -> String
emitDataA (Assign a b) = emitData b

emitData :: Expression -> String
emitData expr = case expr of
        Add a b     -> emitData a ++ emitData b
        Sub a b     -> emitData a ++ emitData b
        Mul a b     -> emitData a ++ emitData b
        Div a b     -> emitData a ++ emitData b
        Var a 		-> emitLn (a ++ "\tdd\t0")
        otherwise   -> ""

 
emitBssA :: Assign -> String
emitBssA (Assign a b) = emitLn (a ++ "\tresd\t1") ++ emitBss b
    
emitBss :: Expression -> String
emitBss expr = case expr of
    Add a b     -> emitBss a ++ emitBss b
    Sub a b     -> emitBss a ++ emitBss b
    Mul a b     -> emitBss a ++ emitBss b
    Div a b     -> emitBss a ++ emitBss b
    otherwise   -> ""

emitTextA (Assign a b) = emitText b ++ emitLn ("MOV [" ++ a ++ "], eax")
emitText expr = case expr of 
         Num a      -> emitLn ("MOV eax, " ++ (show a))
         Add a b    -> emitText a ++  pushEax ++ emitText b ++ add
         Sub a b    -> emitText a ++  pushEax ++ emitText b ++ sub
         Mul a b    -> emitText a ++  pushEax ++ emitText b ++ mul
         Div a b    -> emitText a ++  pushEax ++ emitText b ++ divide
         Var a      -> emitLn ("MOV eax, [" ++ a ++ "]")



emitLn x = "\t" ++ x ++ "\n" 

-- Basic math functions
popEbx = emitLn "POP ebx"
popEax = emitLn "POP eax"
pushEax = emitLn "PUSH eax"  
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
mul = popEbx ++ emitLn "MUL ebx"
divide = emitLn "MOV ebx, eax" ++ popEax ++ emitLn "MOV edx, 0" ++ emitLn "DIV ebx"


externs = "extern _printf\n"

dataSection expr = "section .data\n"
			++ "msg1\tdb\t\"Result: %i\",0xa\n"
			++ emitDataA expr 

bssSection expr = "section .bss\n"  
        	++ emitBssA expr

textSection = "section .text\n" 
            ++ "\tglobal\t_main\n"

mainSection expr = "_main:  ; this is where code starts getting exec'ed\n"
			++"\tpush ebp\t\t; set up stack frame\n"
			++"\tmov ebp,esp\n"
			++"\n"
			++"\t; add the calculation below here\n"
	++ emitTextA expr
			++"\n"
			++"\t; print the result\n"
			++"\tPUSH eax\n"
			++"\tPUSH dword msg1\n"
			++"\tcall _printf\n"
			++"\tadd esp, 8 ; clear the params off the stack\n"
			++"\n"
			++"\tmov\teax,0\t\t;  normal, no error, return value\n"
			++"\tleave\n"
			++"\tret\t\t\t; return\n"