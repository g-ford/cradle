---
layout: layout
title: "Single Digit Math"
group: book
permalink: sigle_digit_math.html
---

# Single Digit Math

Just about every programming language needs to support simple math primitives so it's a good place to start.  This will let us get some results without having to worry about what or how the new language will look like just yet.

We'll want to eventually parse a full mathmatical expression such as `x = (y * 3^2) * (5 + b)` but there is a lot of ground to cover first.  So, in the words of Maria, let's "..start at the very beginning".

## A Single Digit

The very simplist first step is to be able to recognise a single digit i.e. one of `0`..`9`.  This may sound boringly simple, but it will serve to set up a few basic processes first.

To be able to parse anything we will need to read the input and determine whether it is a numerical digit.  To keep things simple we will simply return the digit if it is a digit or else print an error saying 'Digit expected'.  The `Data.Char` module holds many of the basic functions that we will need including one that convieniently determines if a string is a digit named `isDigit`.

~~~ haskell
-- Main.hs

module Data.Char

expected x = error $ x ++ " expected"

expression x
  | isDigit x = x
  | otherwise = expected "Digit"
~~~

So we don't have to deal with reading and outputing files just yet we will make use of the interactive prompt that [GHCi]() provides.  We can test our first 'compiler' like so:

~~~
Prelude> :l Main.hs
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main> expression '1'
'1'
*Main> expression 'a'
*** Exception: Digit expected
~~~

As you can see it meets our initial requirements - an exception is generated if we call `expression` with anything other than a digit.  

## Binary expressions

Let's try something a little more difficult.  The next step is to be able to parse a simple binary expression which we will define as a single digit followed by add or minus and then another single digit.  Some examples:

* 1 + 1
* 5-4 

More genericaly this can eb expressed as `<term> <addOperation> <term>`.  We will need to write a new `expression` to match our new definition that simply returns a string representation of what was input.  Our existing `expression` can be turned into `term` with some minor modification to explictly take a `Char` and return a string or `[Char]`.  

~~~ Haskell
term x
  | isDigit x = [x]
  | otherwise = expected "Digit"

addOperation x
  | x == '+' = "+"
  | x == '-' = "-"
  | otherwise = expected "AddOp"

expression (x:y:z:[]) = (term x) ++ (addOperation y) ++ (term z)
~~~

And testing this in GHCi gives the expected results:

~~~
*Main> :l Main.hs
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main> expression "1+1"
"1+1"
*Main> expression "1*1"
"1*** Exception: AddOp expected
*Main> expression "1+a"
"1+*** Exception: Digit expected
~~~

You will notice that we defined `expression` to take exactly three character inputs only. Expressions with whitespace will fail. We'll deal with whitespace a little latter but out of curiosity let's see what happens.

~~~
*Main> expression "1 + 1"
"*** Exception: Main.hs:(14,1)-(17,35): Non-exhaustive patterns in function expression
~~~

## Beyond binary operations

We can generalise a binary expression into the virtually any number of `term`s using the following form:

`<expression> ::= <term>[<addOperation><term>]*`

This defines an expression as a term followed by any number of +/- and digit pairs.  A recursive definition naturally leads to a recursive implemntation.

~~~ Haskell
expression (x:[]) = term x
expression (x:y:zs) = (term x) ++ (addOperation y) ++ (expression zs)
~~~

The implimentation is remarkable close to the definition.  Once again we will test our running implimentation in GHCi.

~~~
Prelude> :l Main.hs
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main> expression "1+1"
"1+1"
*Main> expression "1+1+1"
"1+1+1"
*Main> expression "1+1-2"
"1+1-2"
*Main> expression "1+1-a"
"1+1-*** Exception: Digit expected
*Main> expression "1+1*1"
"1+1*** Exception: AddOp expected
~~~

## Adding Types

Handling and managing the parsed content into strings is not going to very feasible for much longer.  We'll introduce some types to store our values that can then later be processed to produce the output needed, whether it be raw assembly or another target such as LLVM.

Let's look back at our definition of an `expression`

`<expression> ::= <term>[<addOperation><term>]*`

Again a recursive definition leads to a recursive data type definition.  Our definiton says that we can have a single number, or we can add or subtract any two sub-expression.  This is quite literally expressed in the following data type.

~~~ Haskell
data Expression = 
  Num Int 
  | Add Expression Expression
  | Sub Expression Expression
  deriving (Show) 
~~~ 

Now we can rework our functions to us the new type.  `term` is just wrapping the result in the new type.  Similarly `addOperation` is returning the appropriate value constructor.  `expression` will need to shuffle around to accomodate the argument ordering.

~~~ Haskell
term x
  | isDigit x = Num x
  | otherwise = expected "Digit"

addOperation x
  | x == '+' = Add
  | x == '-' = Sub
  | otherwise = expected "AddOp"

expression (x:[]) = term x
expression (x:y:zs) = (addOperation y) (expression [x]) (expression zs)
~~~

Let's play with it in GHCi and see what results we have now.

~~~
Prelude> :l Main.hs
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main> expression "1+1-2"
Add (Num '1') (Sub (Num '1') (Num '2'))
*Main> expression "1+1*2"
Add (Num '1') *** Exception: AddOp expected
*Main> expression "1+a"
Add (Num '1') *** Exception: Digit expected
~~~

Those with a lisp background, or any of it's variants, might be having a light bulb moment seeing that output.  

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
  MOV eax, 1      ; eax = 1
  MOV ebx, eax    ; ebx = 1
  MOV eax, 2      ; eax = 2  
  MOV ebx, eax    ; ebx = 2 -- Oops we just clobbered the 1!
  MOV eax, 2        ; eax = 2
  ADD eax, ebx        ; eax = 2 + 2 = 4
  SUB eax, ebx        ; eax = 4 - 2 = 2
  NEG eax       ; eax = -2
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
  MOV eax, 1    ; eax = 1
  PUSH eax      ; stack = 1
  MOV eax, 2    ; eax = 2
  PUSH eax    ; stack = 2,1
  MOV eax, 2    ; eax = 2
  POP ebx     ; ebx = 2, stack = 1
  ADD eax, ebx  ; eax = 2 + 2 = 4
  POP ebx     ; ebx = 1, stack=<empty>
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







