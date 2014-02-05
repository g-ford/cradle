---
layout: layout
title: "Combinators"
group: book
permalink: combinators.html
---

# Parser Combinators 

First of all, let's define what a **parser** is:

> [parse](http://dictionary.reference.com/browse/parser) - 3. Computers. to analyze (a string of characters) in order to associate groups of characters with the syntactic units of the underlying grammar. ([Dictionary.com](http://dictionary.com))

And naturally a parser is something that performs a parse operation.  

> A **combinator** is the "...use of a [Higher Order Function] as an infix operator in a function-definition..." 	([Parser Combinator - Wikipedia](http://en.wikipedia.org/wiki/Parser_combinator))

Practically speaking, the technique means that we write small _parser_ functions that do one little thing, such as detect only digits, and return the result and the remainder of the input.

We can then write _combinator_ functions that take two parsers. The input is applied to the first parser, and the output of the first is (potentially) used in the second.

Given the two parsers `digit` and `letter` and the two combinators `<+>` (for sequence) and `<|>` (for alternative), this means that we can write a parser for `identity` which is defined as a letter followed by a letter or a digit as:

    identity = letter <+> (letter <|> digit)

Given the input `a2 the rest of the input` the general flow is (the specific implementation is slightly different):

1. `letter` returns the pair `('a', "2 the rest of the input")`
2. `<+>` extracts the second item and puts it through the next parser
3. `letter` is given `"2 the rest of the input"` which fails to detect a letter 
4. `<|>` sees that `letter` has failed so passes the input onto `digit`
5. `digit` returns the pair `(2, "the rest of the input")`
6. Now that `<+>` has a result for both sides, it bundles the two success up into a pair and pairs that with the remaining input giving `(('a',2), "the rest of the input")`

If we were to try and write `identity` using our previous technique, it would be far more cumbersome and much harder to read the intent of the code.  As you can see, using combinators is a powerful and clean technique that makes the intent far more readable.

## Starting again

Okay, combinators are cool. So now that we know about this technique, let's put it to use in our own compiler.  We will be borrowing very heavily from Andersons paper [Parsing with Haskell](http://www.cs.lth.se/EDA120/assignment4/parser.pdf) in the inital stages, starting from a blank slate and building up the ability to parse `expression`s again. We will keep our `Expression` data type.

The first thing we need is a `Parser` type.  As we saw above, it should return a pair, with the first item being the result, and the second being the unused portion of the input.  However occasionally a parser may fail, and we need to know about that.  So we will wrap our pair in a `Maybe`. We won't stipulate what type the result should be, as it may be a `Integer`, a `String` etc. but the type of the parser will be the type of the success result. 

~~~ Haskell
type Parser a = String -> Maybe (a, String)
~~~
    
Starting from the very basics again, we need a `Parser` that can parse a single character. We will need to start using type declarations on our parsers so that we can combine them all together.

~~~ Haskell
char :: Parser Char
char [] = Nothing
char (x:xs) = Just(x, xs)
~~~

## Our first combinator

Rather than define a digit parser that explicitly tests for digits, we will define it as:

~~~ Haskell
digit :: Parser Char
digit = char <=> isDigit
~~~
    
We haven't defined `<=>` yet, but this will be the first combinator we write. `<=>` is essentially a boolean test operator.  This combinator will only return the result of the first Parser if, and only if, the result passes the boolean expression.  You can think of it as the same as `==`.

~~~ Haskell
-- Given a parser and a predicate return the parser only if it satisfies the predicate.
infix 7 <=> 
(<=>) :: Parser a -> (a -> Bool) -> Parser a 
(parser <=> predicate) input =
    case parser input of 
        Nothing       -> Nothing 
        Just(a,rest)  -> if (predicate a) then Just(a,rest) else Nothing
~~~

Because our parsers are a `Maybe` we need to handle both the `Nothing` and `Just` cases.  The `Nothing` case simply propogates the `Nothing`.  The important part of this definition is in the `Just` case.  It will return a `Just` containing the `parser` only if the value satifies the `predicate`.

Using this we can define a couple more parsers that will be useful a bit further down the track. These two should be quite clear in what they do:

~~~ Haskell
space :: Parser Char
space = char <=> isSpace    

letter :: Parser Char
letter = char <=> isAlpha
~~~ 

And one that will be quite useful for checking for the `+` or `-` in our `Expression`s:

~~~ Haskell
literal :: Char -> Parser Char
literal c = char <=> (==c)
~~~
    
## Alternatives Combinator

This combinator will allow us to define two or more paths through a parse operation.  An example utility parser should explain it clearly.

~~~ Haskell
alphanum :: Parser Char
alphanum = digit <|> letter
~~~

I told you `letter` would be useful.  `<|>` operates like an `or`, that is, the result of `<|>`ing two or more parsers together is the first one that returns a successful result. 

~~~ Haskell
-- Combine two parsers using a 'or' type operation        
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(parserA <|> parserB) input = 
	case parserA input of 
    	Nothing -> parserB input 
    	result -> result
~~~
    
You might notice this time we used a left associative infix. This means that we can write `a <|> b <|> c` and it will logicaly return the first one that is a success. 

We also used a lower order of precedence - the higher the number the further down the order it sinks.  This means that we can write expressions such as `a <=> b <|> c` which will equate to `(a <=> b) <|> c`.

## Basic assignment 

We now have enough parsers and combinators to be able to parse assignments of the form:
    
`assignment ::= <letter>=<digit>`

This will let us parse things such as `a=1`.  Pretty basic. Let's create an assignment type for this and a parser to recognise the form.

~~~ Haskell
data Assign = Assign Char Expression 
	deriving Show

assign :: Parser ((Char, Char), Char)
assign = letter <+> literal '=' <+> digit 
~~~

Now that reads almost exactly like the definition of assignment. The combinator `<+>` is a left associative sequence operator.  This will only apply the second parser if the first is succesful and will return the two successes combined in a pair.  Multiple applications of `<+>` will create nested pairs.

~~~ Haskell
-- Combine two parser together pairing their results up in a tuple
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(parserA <+> parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
        	Just (resultB, cs) -> Just((resultA, resultB), cs)
~~~

We'll also need to write a function that uses our new `assign` parser to recognise a valid assignment and build an `Assign` data type from the results.

~~~ Haskell
parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just (((a, _), b), _) -> (a, (Num b)) -- the underscores represent the '=' which we don't need to keep
~~~              

Great, but that extra '=' in the result of `assign` which just gets thrown away all the time is annoying me.  It makes more sense for `assign` to have the type `Parser (Char, Char)` so that it returns results in the form `(a,1)`.

~~~ Haskell
parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, Num b)
                   
assign = letter <+-> literal '=' <+> digit  
~~~
    
We've refactored the new result format into `parse`, and the new combinator `<+->` has been introduced into `assign`.  This combinator has the exact same semantics as `<+>` only it discards the second result.

~~~ Haskell
-- Sequence operator that discards the second result 
infixl 6 <+-> 
(<+-> ) :: Parser a -> Parser b -> Parser a
(parserA <+->  parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
	        Just (_, cs) -> Just(resultA, cs)
~~~

And just for completeness, one that discards the first result and keeps only the second.

~~~ 
-- Sequence operator that discards the first result       
infixl 6 <-+> 
(<-+> ) :: Parser a -> Parser b -> Parser b
(parserA <-+>  parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
	        Just (resultB, cs) -> Just(resultB, cs) 
~~~

We now have a the beginnings of a compiler that, while a bit longer than the previous attempt, is much simpler to read and definately far simpler to extend.  


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
