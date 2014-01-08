---
layout: layout
title: "Code Generation 101"
group: book
permalink: code_generation_101.html
---

# Code generation 

Our parsing is not very useful bound up in some Haskell types.  We can write a very simple code generator to emit what we can currently handle.  Our code generator will need to take an `Expression` and recursively traverse the tree and build up a string.  We start by converting it back into the same infix string again.

~~~ Haskell
emit expr = case expr of
	Num x -> [x]
	Add x y -> emit x ++ " + " ++ emit y
	Sub x y -> emit x ++ " - " ++ emit y
~~~

Which will produce output similar to:

~~~
Prelude> :l Main.hs
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main> let e = expression "1+1+3"
*Main> e
Add (Num '1') (Add (Num '1') (Num '3'))
*Main> emit e
"1 + 1 + 3"
*Main> emit (expression "1+1-2")
"1 + 1 - 2"
~~~

Congratulations - you've made a 'pretty-printer' for a very small subset of math.

## Generating Assembly

<aside class="col-xs-12 col-sm-4 pull-right well">
<strong>A note on assembly</strong>

<p>There are a couple of variants of x86 assembly syntax versions and even more Assemblers.  As I will be using NASM, the syntax I am using throughout most of this is based on thr Intel variant as used by NASM.</p>
</aside>

The most basic assembly we can do is use registers to store each `term` in and then use these as the input to `ADD` or `SUB` as needed.  The order of operations are:

 - Move the first term into `eax`
 - Move the value of `eax` to `ebx`
 - Move the second term into `eax`
 - Add or subtract `ebx` from `eax` leaving the result in `eax`

We'll start with a couple helper methods to do write and format the assembly instructions. 

~~~ Haskell
emitLn s = "\t" ++ s ++ "\n"

add = emitLn "ADD eax, ebx"
sub = emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
pushEax = emitLn "MOV ebx, eax"
~~~

A minor implementation note for subtraction is the fact that subtraction is not communicative.  `1-2` is not the same as `2-1`.  Because we want to always leave the result in `eax` and `SUB` (and `ADD`) will leave the result in the first register we will actually end up executing the subtraction in the reverse. However there is a easy trick to make subtraction almost communicative - simply negate the result. 

We'll write a new `emitAsm` function to generate our code using the above functions.  We'll also throw in a function to parse and emit in one go.

~~~ Haskell
emitAsm expr = case expr of 
	Num a   -> emitLn ("MOV eax, " ++ [a])
	Add a b -> emitAsm a ++  pushEax ++ emitAsm b ++ add
	Sub a b -> emitAsm a ++  pushEax ++ emitAsm b ++ sub

parseAndEmit = emitAsm . expression
~~~

If you test this in GHCi with only two term expressions such as `4+6` or `8-3` you can manually verify the generated asembly is correct.  But as soon as you try more than two terms you will notice that it produces incorrect results.  For example `1-2+2` will produce `-2` instead of the expected `1`.  

~~~
*Main> putStr $ parseAndEmit "1-2+2"
	MOV eax, 1 			; eax = 1
	MOV ebx, eax 		; ebx = 1
	MOV eax, 2 			; eax = 2  
	MOV ebx, eax 		; ebx = 2 -- Oops we just clobbered the 1!
	MOV eax, 2    		; eax = 2
	ADD eax, ebx        ; eax = 2 + 2 = 4
	SUB eax, ebx        ; eax = 4 - 2 = 2
	NEG eax 			; eax = -2
~~~

## Stacking it up

The  intermediate instructions are built up in a tree format, that when flattened, come out in a format very similar to reverse polish notation or RPN.  RPN is most commonly executed using a stack - terms are `push`ed onto the stack and when an operand is encountered the terms are `pop`ed off to use with the operator.  And that's exectly how we will write our asm.

We can store an arbitraty number of items on the stack in a last-in first-out manner.  It is a relatively minor change to the existing code to use the stack. Instead of moving `eax` into `ebx` we `PUSH` it onto the stack.  Then `add` and `sub` simply `POP` the top term off before doing their calculations.

~~~ Haskell
popEbx = emitLn "POP ebx"
pushEax = emitLn "PUSH eax"
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
~~~

`emitAsm` remains unchanged. And now the generated assembly will calculate the correct result if we lived in a right associative world i.e. `1-2+2` is interpretted as `1-(2+2)`

~~~
*Main> putStr $ parseAndEmit "1-2+2"
	MOV eax, 1 		; eax = 1
	PUSH eax  		; stack = 1
	MOV eax, 2 		; eax = 2
	PUSH eax 		; stack = 2,1
	MOV eax, 2 		; eax = 2
	POP ebx 		; ebx = 2, stack = 1
	ADD eax, ebx 	; eax = 2 + 2 = 4
	POP ebx 		; ebx = 1, stack=<empty>
	SUB eax, ebx    ; eax = 4 - 1 = 3
	NEG eax
~~~

## Living in a left associative world

Unfortunately the math that the majority of us know is left associative.  This means that `1-2+2` is commonly interpretted as `(1-2)+2` resulting in `1`.  There is actually a simple approach to acheiving this - we look one more character ahead and build the left side first.  
~~~ Haskell
expression (x:[]) = term x
expression (a:b:c:d:ds) = (addOperation d) (expression [a,b,c]) (expression ds)
expression (x:y:zs) = (addOperation y) (expression [x]) (expression zs)
~~~

**N.B** You must add the new `expression` before the previous one that handled 3 terms.

This will, finally, give us the correct results for 3 terms in an expression, but what about 4:

~~~
*Main> expression "1-2+2"
Add (Sub (Num '1') (Num '2')) (Num '2') -- Add (-1, 2) = 2
*Main> expression "1-2-5-2"
Sub (Sub (Num '1') (Num '2')) (Sub (Num '5') (Num '2')) -- Sub (-1, 3) = -4 !? Should be -8
~~~

We could go and add another `expression` to handle 4 terms but what about 5, 6, 99...  Clearly this approach is not sustainable and we will need to find another technique.



