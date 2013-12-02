---
layout: layout
title: "Adding Types"
group: book
permalink: adding_types.html
---

# Adding Types

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


## Stacking

We are currently building up a string as soon as we see each term.  Handling operator precedence in this current incarnation wont cut it.  Whoever decided that multiplication is more important than addition, and that this should be true even when you don't use parentheses, was a madman. Anyway, even without operator precedence, the current technique would not be able to handle expressions such as `5-(1+2)`. 

To handle this, we use one the first data structures taught in computer science - a stack.  We can store an arbitraty number of items on the stack in a last-in first-out manner.  It is a relatively minor change to the existing code to use the stack. Instead of moving `eax` into `ebx` we `PUSH` it onto the stack.  Then `add` and `sub` simply `POP` the top term off before doing their calculations.