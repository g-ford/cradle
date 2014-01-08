---
layout: layout
title: "Code Generation 102"
group: book
permalink: code_generation_102.html
---

# Code Generation

Now that we a have a well organised codebase with a compiler that can compile simple addition and multiplication we need to revist code generation.

In Code Generation 101 we only generated partial assembly without any boilerplate to allow it to actually be assembled.  In this chapter we will go through the full process so that we can successfully assemble the output using the NASM assembler.  The complete program will be hardcoded to calculate the same sum every time.

As mentioned in the first code generation, I will be using NASM and it's flavour of assembly syntax.  After we have generated all our assembly we can use the following cmds to assemble the output into a runnable executable.

<aside class="col-xs-12 col-sm-4 pull-right well">I've used `gcc` to do the linking so that we can access `libc` to make printing easier.  For those that want to be a bit more bare bones you could use the print system directive directly to print the results.  This would allow you to use `ld` to do the linking and generate smaller executables.</aside>

For OSX

	~> ./cradle "test = 12 * 45 + 1" > calc_test.asm
	~> nasm -f macho calc_test.asm 
	~> gcc -arch i386 -o calc_test calc_test
	~> ./calc_test 

## The structure of an assembly program

Assembly programs are usually divided into three distinct areas called `sections`. The first allocates constants and is called the **data** section. The second is used to allocate or reserve some memory for variables before they can be used and is called **bss**.  The final section is where the actual code lives and is called the **text** section.  

For example, a simple program for `c=a+b` where `a=1` and `b=2` would require us to allocate `a` and `b` and reserve space for `c`.  This would look like:

    ; Note: this is not a complete program 
    ; dd and resd are used for dword (32 bit) allocations
    section .data
        a	dd	1   ; a=1 
        b   dd  2   ; b=2

    section .bss
        c   resd   1 ; reserve dword (32bits) for c

    section .text
        mov eax, [a]    ; put the value of a into eax
        add eax, [b]    ; add eax and the value of b
        mov [c], eax    ; c = eax

To emit the assembly in the text section is pretty straight forward and doesn't change much from out first attempt.  You'll notice that the output for variables (`Var a`) and integers (`Num a`) is pactically the same, but instead of loading a literal integer into the register we load the value located at the label.

Assignment simply loads whatever is in the `eax` register, after all the operations have occured, into the label.

~~~ Haskell
emitTextA :: Assign -> String
emitTextA (Assign a b) = emitText b ++ emitLn ("MOV [" ++ a ++ "], eax")

emitText :: Expression -> String
emitText expr = case expr of 
         Num a      -> emitLn ("MOV eax, " ++ (show a))
         Add a b    -> emitText a ++  pushEax ++ emitText b ++ add
         Sub a b    -> emitText a ++  pushEax ++ emitText b ++ sub
         Mul a b    -> emitText a ++  pushEax ++ emitText b ++ mul
         Div a b    -> emitText a ++  pushEax ++ emitText b ++ divide
         Var a      -> emitLn ("MOV eax, [" ++ a ++ "]")
~~~

We havn't covered multiply and divide yet.  In the case of multiplication the first term is assumed to be in `eax` and the multiplier is what is given to `mul` op.  The results are stored in two registers.  The highest 32 bits go to `edx` and the lower 32 bits go to `eax`.  For todays purposes we are only interested in the lower 32 to keep things simpler - so don't try multiply greater than 2^32.

Divide is similar in nature.  The dividend is assumed to be in `edx:eax` which we means we will have to move the most significant bits into `edx`. With our artificial limit on values to 32bit this means we simply set `edx` to `0`.  Again the results are stored in `edx:eax` with the remainder in `edx` and the quotient in `eax`.  To keep things simple so we only have to worry about the `eax` results we'll use integer division in our calculator.

~~~ Haskell
mul = popEbx ++ emitLn "MUL ebx"
divide = emitLn "MOV ebx, eax" ++ popEax ++ emitLn "MOV edx, 0" ++ emitLn "DIV ebx"
~~~

## Outputing the sections

To get all the sections to output correctly we need to define a function for each section that takes our assignment expression and produces the relevant lines for that section.

For the data section, we are only intereted in `Var` expressions.  Unfortunately we have not yet allowed for our variables to have an initial value yet, so we will default everything to zero - this is a sensible default considering we are only dealing with single digit integers.
    
~~~ Haskell
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
~~~

As you can see the only one that actually creates output is `Var` and the rest just pass through.

The bss section is pretty similar except this time we are only really interested in the first part of `Assign`.  For now we don't need to worry too much about typing so we will hard code everything to a 32 bit DWORD.

~~~ Haskell
emitBssA :: Assign -> String
emitBssA (Assign a b) = emitLn (a ++ "\tresd\t1") ++ emitBss b
    
emitBss :: Expression -> String
emitBss expr = case expr of
    Add a b     -> emitBss a ++ emitBss b
    Sub a b     -> emitBss a ++ emitBss b
    Mul a b     -> emitBss a ++ emitBss b
    Div a b     -> emitBss a ++ emitBss b
    otherwise   -> ""
~~~

And then we can tie it all together with a new `emit` function.  This will create the section labels and then hand off to the relevant emit function for that section.

~~~ Haskell
-- Turns an expression into the equvilent assembly
emit :: Expression -> String
emit expr= "section .data\n" ++ emitData expr 
            ++ "section .bss\n" ++ emitBss expr 
            ++ "section .text\n" ++ emitText expr
~~~

Finally update `Cradle.hs` to use the new generator and you can then `cabal build` and play around with the asseble & linking process described earier.

~~~ Haskell
module Main
where

import System.Environment
import Cradle.Grammar
import Cradle.Generator.Nasm

main :: IO ()
main = getArgs >>= e . head

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)

-- | Parse and print. Utility and test function for use in @ghci@.
p = putStrLn . show . parse

-- | Parse and emit.
e = putStrLn . emit . parse
~~~

## Conclusion

We can generate a fairly fixed and simple program and assemble and link the results.  It works but it's not pretty as we haven't made this a seamless process yet.  

This will be the last of assembly generation for a while until we have a more complete parser and abstract tree to do code generation with.
