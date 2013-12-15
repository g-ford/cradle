---
layout: layout
title: "Combinatorial Expressions"
group: book
permalink: combinatorial_expressions.html
---

# Combinatorial Expressions

Let's recap our current definition of an expression.

`<expression> ::= <term>[<addOperation><term>]*`

`term` is defined as a single digit number and `addOperation` is defined as either `+` or `-`.  Using our existing combinators and parsers we can define a new `expression`.

Starting simple we will write an `expression` parser that can only parse our single digits returning an `Expression` using the `Num` constructor.

~~~ Haskell
expression :: Parser Expression
expression = digit >>> Num
~~~

We have a new combinator `>>>` that means "transformed by" or "applied to".  This combinator will apply the given function or transformation if the parser returns a non-empty result.

~~~ Haskell
-- Transform a parsers result
infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b
(parser >>> transformer) input = 
	case parser input of
		Nothing -> Nothing
		Just (resultA, remainder) -> Just ((transformer resultA), remainder)
~~~

We've chosen use a left associative infix operator and carefully chosen the weighting so that the result of combining sequences and alternatives will collectively be passed to the transformer.

Testing this in GHCi will show that instead of getting a basic `Just('1', "")` that `digit` gives us we end up with `Just (Num '1',"")`.

This can be pushed down to a `term` function so that we can create a new `expression` to handle binary addition or subtraction.  The obvious solution is implied by the definition.

~~~ Haskell
expression = term <+> addOp <+> term 

addOp :: Parser Char
addOp = literal '+' <|> literal '-'
~~~

You might have notice that I left the type hint off `expression`. This was intentional as the current definition won't create a valid `Expression` although it will create a valid result that looks like:

~~~
*Main> expression "1+1"
Just (((Num '1','+'),Num '1'),"")
~~~ 

But we need to get that into something that looks like `Just ((Add (Num '1'), (Num '1')),"")`.  We can use our transform cobinator `>>>` to pass the results to another function that will rip the previous result apart and build the format that we need.

~~~ Haskell
expression :: Parser Expression
expression = term <+> addOp <+> term >>> buildOp

buildOp ((termA, op), termB)
	| op == '+' = Add termA termB
	| op == '-' = Sub termA termB
	| otherwise = expected "add operation"
~~~

