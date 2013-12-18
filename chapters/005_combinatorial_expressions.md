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

In this definition `buildOp` uses some guards to figure out which operation to build, and also has a call to `expected`. Through the wonders of partial and first class functions, we can actually rewrite `addOp` to tell us which data constructor to use, which means `buildOp` gets simplified and won't have a chance to fail.

~~~ Haskell

addOp :: Parser (Expression -> Expression -> Expression)
addOp = literal '+' >>> (\_ -> Add)
    <|> literal '-' >>> (\_ -> Sub)

buildOp :: ((Expression, Expression -> Expression -> Expression), Expression) -> Expression
buildOp ((expressionA, op), expressionB) = op expressionA expressionB
~~~

Here you can see the careful selection of infix weighting on the combinators coming to fruition.  In the definiton of `addOp` we use the alterantive and the transform combinators in a logical and clear to read manner.  

## Beyond binary 

So that's binary. We have the basic process so now let's extend it to the n-ary version. We'll take a different approach to the first attempt. What we want to do is find the first `term` and then pass that onto a subfunction that continuosly builds an `Expression` based on the parsing `[<addOp><term>]`. 

~~~ Haskell
expression :: Parser Expression
expression = term +> expression'
expression' e = addOp <+> term >>> buildOp e +> expression'
            <|> result e
~~~

This subfunction takes an `Expression`, parses out a `addOp` and another factor, passes the original `Expression`, the `addOp` and the new `term` into `buildOp` and then sends this new `Expression` through itself again in a recursive manner until it can no longer parse a `addOp` and `term` at which point it returns the `Expression` built so far.	

We've introduced a new combinator `+>` which can be though of as a reduction.  It will take everything on the left and apply the function on the right returning the raw result.  This is similar to `>>>` which applies the function and returns the wrapped result, whereas `+>` will unwrap the result.

~~~ Haskell
-- Extract a parsers result	
infix 4 +>
(+>) :: Parser a -> (a -> Parser b) -> Parser b
(parser +> function) input = 
	case parser input of
		Nothing -> Nothing
		Just (a, cs) -> function a cs
~~~

Similarly `result` is a parser that simply returns what it is given wrapped up as a `Parser` type.  This lets us do things such as define default alternatives, or in this case, return the accumulated result. 

~~~ Haskell
result :: a -> Parser a
result a cs = Just(a,cs)
~~~

We also need to revisit `buildOp`.  This function now takes two parameters getting the new operation and factor as a pair. It's a small shuffle so let's quickly redefine it.

~~~ Haskell
buildOp :: Expression -> ((Expression -> Expression -> Expression), Expression) -> Expression
buildOp expressionA (op, expressionB) = op expressionA expressionB
~~~

There are a couple of added benefits to defining `expression` using the two parts.  We solved the left associative issue because the operations are built up on the way throught the parsing instead of all at the end.  Additionally we can now have single term expressions.

## Assignable Expressions

In the previous chapter we defined `assign` using `digit` and had to use a data constructor directly in `parse` to get a valid `Assign`.  We can make some small changes to use any `Expression` on the right of `Assign`.

~~~ Haskell
parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)

assign :: Parser (Char, Expression)
assign = letter <+-> literal '=' <+> expression
~~~

And now we can get some pretty good (and correct) results out of our parser.

~~~
*Main> parse "a=1"
Assign 'a' (Num '1')
*Main> parse "a=1+7-3"
Assign 'a' (Sub (Add (Num '1') (Num '7')) (Num '3'))
~~~
