#Let's build a compiler (in Haskell): Part 7 - Parser Combinators

Our parser has served us well so far.  We can successfully `parseAndEmit` simple mathmatical expressions as well as assignments.

However the technique used so far, one big `parse` function, has some serious drawbacks.  Not least is the fact that it is starting to get too long and complicated.  We also have a major issue with implementing things such as parentheses, multi-character factors and variables and alternatives such as what looks like an identifier may actually end up being a function call.

In this article we will rewrite out compiler using a technique that has been used in parsers, especially those written in functional languages, for several decades now. 

## Parser Combinators 

First of all, we need to define what a *parser* is:

> [parse](http://dictionary.reference.com/browse/parser) - 3. Computers. to analyze (a string of characters) in order to associate groups of characters with the syntactic units of the underlyinggrammar. - [Dictionary.com](http://dictionary.com)

And naturally a parser is something that performs a parse operation.  

A *combinator* is the "...use of a [Higher Order Function] as an infix operator in a function-definition..."([Parser Combinator - Wikipedia](http://en.wikipedia.org/wiki/Parser_combinator))

Practically speaking, the technique means that we write small _parser_ functions that do one little thing, such as detect only digits, and return the result and the remainder of the input.

We can then write _combinator_ functions that take two parsers. The input is applied to the first parser, and the output of the first is (potentially) used in the second.

Given the two parsers `digit` and `letter` and the two combinators `<+>` (for sequence) and `<|>` (for alternativc), this means that we can write a parser for `identity` which is defined as a letter followed by a letter or a digit as:

    identity = letter <+> (letter <|> digit)

Given the input `a2 the rest of the input` the general flow is (the specific implementation is slightly different):

1. `letter` returns the pair `('a', "2 the rest of the input")`
2. `<+>` extracts the second item and puts it through the next parser
3. `letter` is given `"2 the rest of the input"` which fails to detect a letter 
4. `<|>` sees that `letter` has failed so passes the input onto `digit`
5. `digit` returns the pair `(2, "the rest of the input")`
6. Now that `<+>` has a result for both sides, it bundles the two success up into a pair and pairs that with the remaining input giving `(('a',2), "the rest of the input")`

If we were to try and write `identity` using our previous technique, it would be far more cumbersome and much harder read the intent of the code.  As you can see, using combinators is a powerful and clean technique that makes the intent far more readable.

## Starting again (again)

So now that we know about this technique, let's put it to use in our own compiler.  In this article I will be borrowing very heavily from Andersons paper [Parsing with Haskell](http://www.cs.lth.se/EDA120/assignment4/parser.pdf) which we saw earlier. 

We will be starting from a blank slate and build up the ability to parse assignment expressions again. We will keep our `Expression` type.

The first thing we need is a `Parser` type.  As we saw above, it should return a pair, with the first item being the result, and the second being the unused portion of the input.  However occasionally a parser may fail, and we need to know about that.  So we will wrap our pair in a `Maybe`. We won't stipulate what type the result should be, as it may be a `Integer`, a `String` etc. but the type of the parser will be the type of the success result. This gives us:

    type Parser a = String -> Maybe (a, String)
    
Starting from the very basics again, we need a recogniser that can parse a single character. 

    char :: Parser Char
    char [] = Nothing
    char (x:xs) = Just(x, xs)

## Our first combinator

Rather than define a digit recogniser that explicitly tests for digits, we will define it as:

    digit :: Parser Char
    digit cs = (char <=> isDigit) cs

or even more consisely:

    digit :: Parser Char
    digit = char <=> isDigit
    
We haven't defined `<=>` yet, but this will be the first combinator we write. `<=>` is essentially a boolean test operator.  This combinator will only return the result of the first Parser if, and only if, the result passes the boolean expression.

    -- Given a parser and a predicate, return the result of the parser only if
    -- it also satisfies the predicate.
    infix 7 <=> 
    (<=>) :: Parser a -> (a -> Bool) -> Parser a 
    (m <=> p) cs =
        case m cs of 
            Nothing     -> Nothing 
            Just(a,cs)  -> if p a then Just(a,cs) else Nothing
            
Using this we can define a couple more recognisers that will be useful a bit further down the track. These two should be quite clear in what they do:

    space :: Parser Char
    space = char <=> isSpace    

    letter :: Parser Char
    letter = char <=> isAlpha
    
And one that will be quite useful for checking for the '=' in our assignments:

    literal :: Char -> Parser Char
    literal c = char <=> (==c)
    
## Combinator #2: Alternatives

This combinator will allow us to define two or more paths through a parse operation.  An example:

    alphanum :: Parser Char
    alphanum = digit <|> letter

I told you `letter` would be useful.  `<|>` operates like an `or`, that is, the result of `<|>`ing two or more parsers together is the first one that returns a successful result. 

    -- Combine two parsers using a 'or' type operation        
    infixl 3 <|>
    (<|>) :: Parser a -> Parser a -> Parser a 
    (m <|> n) cs = case m cs of 
        Nothing -> n cs 
        mcs -> mcs
    
You might notice this time we used a left associative infix. This means that we can write `a <|> b <|> c` and it will logicaly return the first one that is a success. 

We also used a lower order of precedence (the higher the number the further down the order it sinks).  This means that we can write expressions such as `a <=> b <|> c` which will equate to `(a <=> b) <|> c`.

## Basic assignment 

We now have enough parsers and combinators to be able to parse assignments of the form:
    
    assignment ::= <letter>=<digit>

This will let us parse things such as `a=1`.  Pretty basic.

Let's seperate assignment from expression and create a new type for this:

    data Assign = Assign Char Expression
              deriving (Show)

So our new `parse` function will be:

    parse :: String -> Assign
    parse s = Assign id expr
        where (id, r1) = case letter s of
                       Nothing -> error "expected letter"
                       Just a -> a
              (_, r2)  = case literal '=' r1 of
                       Nothing -> error "expected '='"
                       Just a -> a
              (expr, _)= case digit r2 of 
                       Nothing -> error "expected digit"
                       Just (a, b) -> (Num (read [a]), b)
                       
We'll need a few more parsers and combinators before we can realistically parse an expression so we have to put the `(Num (read [a])` in for now.  

## Sequencing combinators

It's a bit hard to see what is happening in the `parse` above so we'll write an `assign` parser.  This parser will return a result like `(('a','='),'1')` so we can rewrite out `parse` function as:

    parse :: String -> Assign
    parse s = Assign id expr
        where (id, expr) = case assign s of 
                Nothing -> error "Invalid assignment"
                Just (((a, _), b), _) -> (a, (Num (read [b])))
                
This is clearer as we can see that we are getting `id` and `expr` from an `assign` function that is much easier to read that the multiple where clauses:

    assign :: Parser ((Char, Char), Char)
    assign = letter <+> literal '=' <+> digit 

Now that reads almost exactly like the definition of assignment. The combinator `<+>` is a left associative sequence operator.  This will only apply the second parser if the first is succesfull, and will return the two successes combined in a pair.  Multiple applications of `<+>` will create nested pairs.

    -- Sequence operator that pairs up two parsers
    infixl 6 <+>
    (<+>) :: Parser a -> Parser b -> Parser (a, b)
    (m <+> n) cs = case m cs of
        Nothing -> Nothing
        Just (a, cs') -> case n cs' of
            Nothing -> Nothing
            Just (b, cs2) -> Just((a, b), cs2)

