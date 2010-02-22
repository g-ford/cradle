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

We will be starting from a blank slate, but we will keep our `Expression` type.

The first thing we need is a `Parser` type.  As we saw above, it should return a pair, with the first item being the result, and the second being the unused portion of the input.  However occasionally a parser may fail, and we need to know about that.  So we will wrap our pair in a `Maybe`. We won't stipulate what type the result should be, as it may be a `Integer`, a `String` etc. but the type of the parser will be the type of the success result. This gives us:

    type Parser a = String -> Maybe (a, String)