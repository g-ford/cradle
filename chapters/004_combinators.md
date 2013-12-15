---
layout: layout
title: "Combinators"
group: book
permalink: combinators.html
---

# Parser Combinators 

First of all, we need to define what a **parser** is:

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