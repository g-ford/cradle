---
layout: layout
title: "Single Digit Math"
group: book
permalink: sigle_digit_math.html
---

# Single Digit Math

Just about every programming language needs to support simple math primitives so it's a good place to start.  This will let us get some results without having to worry about what or how the new language will look like just yet.

We'll want to eventually parse a full mathmatical expression such as `x = (y * 3^2) * (5 + b)` but there is a lot of ground to cover first.  So in the words of Mary Poppins let's "..start at the very beginning".

## A Single Digit

The very simplist first step is to be able to recognise a single digit i.e. one of `0`..`9`.  This may sound boringly simple, but it will serve to set up a few basic processes first.

To be able to parse anything we will need to read the input and determine whether it is a numerical digit.  To keep things simple we will simply return the digit if it is a digit or else print an error saying 'Digit expected'.  The `Data.Char` module holds many of the basic functions that we will need including one that convieniently determins if a string is a digit name `isDigit`.

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

You will notice that we defined `expression` to take exactly three character inputs only which means that expressions with whitespace will fail. We'll deal with whitespace a little latter but out of curiosity let's see what happens.

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





