module Cradle.Generator.Nasm

where

import Cradle.Grammar
import Cradle.Grammar.Control
import Cradle.Grammar.Expressions
import Cradle.Grammar.Boolean


class Emittable e where 
    emitData :: e -> String
    emitBss :: e -> String
    emitText :: Integer -> e -> (Integer, String)

emit :: Program -> String
emit p = externs
      ++ dataSection p
      ++ bssSection p 
      ++ textSection p

instance Emittable Program where
    emitData (Program p) = emitData p
    emitBss (Program p) = emitBss p
    emitText l (Program p) = emitText l p

instance Emittable Block where
    emitData (Block b) = foldl (++) ""  (map  emitData b)
    emitBss (Block b) = foldl (++) ""  (map  emitBss b)
    emitText l (Block []) = (l, "")
    emitText l (Block (b:bs)) = (l', first ++ rest)
        where (l1, first) = emitText (l + 1) b
              (l', rest)   = emitText l1 (Block bs)

instance Emittable Statement where
    emitData s = case s of
        Statement (Assign a b)  -> emitData b
        Branch condition b      -> emitData condition ++ emitData b
        otherwise               -> emitLn "<placeholder>"
    emitBss s = case s of
        Statement (Assign a b)  -> emitLn (a ++ "\tresd\t1") ++ emitBss b
        Branch condition b      -> emitBss condition ++ emitBss b
        otherwise               -> emitLn "<placeholder>"
    emitText l s = case s of
        Statement (Assign a b)  -> (l', block1 ++ emitLn ("MOV [" ++ a ++ "], eax"))
                                    where (l', block1) = emitText l b
        Branch condition b      -> (l', cond ++ block1 ++ emitLabel)
                                    where (l1, cond) = emitText l condition
                                          (l', block1) = emitText l1 b
                                          emitLabel = "Label" ++ (show l) ++ ":\n"
        
        Branch2 condition b1 b2 -> (l', cond ++ block1 ++ branch ++ emitLabel1 ++ block2 ++ emitLabel2)
                                    where (l1, cond)       = emitText l condition 
                                          (l2, block1)     = emitText l1 b1
                                          (l', block2)     = emitText l2 b2
                                          emitLabel1 = "Label" ++ (show l1) ++ ":\n"
                                          emitLabel2 = "Label" ++ (show l2) ++ ":\n"
                                          branch = emitLn ("jmp " ++ "Label" ++ (show l2))
        otherwise -> (l, emitLn "<placeholder>")

instance Emittable BoolExpression where
    emitData e =  ""
    emitBss e =  ""
    emitText l e = (l, emitLn "<condition>" ++ emitLn ("jne " ++ "Label" ++ (show l)))

instance Emittable Expression where
    emitData e = case e of
        Add a b     -> emitData a ++ emitData b
        Sub a b     -> emitData a ++ emitData b
        Mul a b     -> emitData a ++ emitData b
        Div a b     -> emitData a ++ emitData b
        Var a         -> "" --emitLn (a ++ "\tdd\t0")
        otherwise   -> ""
    emitBss expr = case expr of
        Add a b     -> emitBss a ++ emitBss b
        Sub a b     -> emitBss a ++ emitBss b
        Mul a b     -> emitBss a ++ emitBss b
        Div a b     -> emitBss a ++ emitBss b
        otherwise   -> ""
    emitText l expr = case expr of 
         Num a      -> (l, emitLn ("MOV eax, " ++ (show a)))
         Add a b    -> (l, partA ++  pushEax ++ partB ++ add)
            where (_, partA) = emitText l a
                  (_, partB) = emitText l b
         Sub a b    -> (l, partA ++  pushEax ++ partB ++ sub)
            where (_, partA) = emitText l a
                  (_, partB) = emitText l b
         Mul a b    -> (l, partA ++  pushEax ++ partB ++ mul)
            where (_, partA) = emitText l a
                  (_, partB) = emitText l b
         Div a b    -> (l, partA ++  pushEax ++ partB ++ divide)
            where (_, partA) = emitText l a
                  (_, partB) = emitText l b
         Var a      -> (l, emitLn ("MOV eax, [" ++ a ++ "]"))




---- Helpers

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

dataSection program = "section .data\n"
			++ "msg1\tdb\t\"Result: %i\",0xa\n"
			++ emitData program 

bssSection program = "section .bss\n"  
        	++ emitBss program

textSection program = "section .text\n" 
            ++ "\tglobal\t_main\n"
            ++ "_main:  ; this is where code starts getting exec'ed\n"
			++"\tpush ebp\t\t; set up stack frame\n"
			++"\tmov ebp,esp\n"
			++"\n"
			++"\t; add the calculation below here\n"
	++ prog
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
        where (_, prog) = emitText 1 program