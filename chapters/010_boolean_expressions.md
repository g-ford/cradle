---
layout: layout
title: "Boolean Expressions"
group: book
permalink: boolean_expressions.html
---

# Boolean Expressions

In the last chapter we made do with a placeholder for the barnching conditions so that we could focus on the control contstructs themselves.  We'll fill those placeholders in with some boolean expressions. 

A boolean expression is one that reduces to one of two possible values: a truth value and a non-truth value.  Commonly this is `true` and `false`. For many languages these values are actually aliased to `1` and `0` mostly for implementation reasons.

## Starting small

The simplest boolean expression is either of the two constants `true` or `false`.  We'll create a new module in the `Cradle.Grammar` namespace called `Boolean` to hold all our boolean relted parsing and creat a data type that contains these two constants. 

~~~ Haskell
module Cradle.Grammar.Boolean

where

import Cradle.Parser

data BoolExpression = 
    BTrue
    | BFalse
~~~

I've chosen to prefix the constants with `B` to avoid conflicts with the built in true and false constants. Parsing these is pretty simple.

~~~ Haskell
bLiteral :: Parser BoolExpression
bLiteral = accept "true"  >>> (\_ -> BTrue)
       <|> accept "false" >>> (\_ -> BFalse)
~~~

Next we'll add the simple logic combinators AND and OR.  I've chosen to use the symbols most familiar to anyone coming from most C-style langagues - `&&` and `||`.  We'll call these boolean operators.

~~~ Haskell
data BoolExpression = 
    BTrue
    | BFalse
    | BOr BoolExpression BoolExpression
    | BAnd BoolExpression BoolExpression

boolOp :: Parser (BoolExpression -> BoolExpression -> BoolExpression)
boolOp = token(accept "&&") >>> (\_ -> BAnd)
    <|> token(accept "||") >>> (\_ -> BOr)
~~~

This looks correct at first glance but if you recall our definition of `accept` it only accepts `letters`.  So we need to write a parser that can handle arbitrary characters.  I chose to implment one that will consume the input until a `space` is found.  This has the side effect that the language forces the use of a space after these constructs i.e. `a&&b` will result in an invalid input.  Personally I don't mind this as I find it far easier to read code with whitespace in it.

~~~ Haskell
notSpace :: Parser Char
notSpace = char <=> (not . isSpace) 

-- |A parser that will accept a given alpha string
acceptWord :: String -> Parser String
acceptWord w = token (letters <=> (==w))

-- |A parser that will accept a given string
accept :: String -> Parser String
accept w = token ((iter notSpace) <=> (==w))  
~~~  

To make it more explicit that the old `accept` is the special case I have renamed it `accetWord` which means all existing uses will need to be updated.  The new `accept` will iteration over all characters until a space character is found.  `isSpace` comes from the `Data.Char` module.

So now that `boolOp` works correctly we can write our first iteration of `boolExpression`.  A boolean expression is very similar in construct to an `Expression` and the basic form reflects that.

~~~ Haskell
boolExpression :: Parser BoolExpression
boolExpression = token(bFactor) +> boolExpression'
boolExpression' e = boolOp <+> bFactor >>> buildRelOp e +> boolExpression'
            <|> result e

bFactor :: Parser BoolExpression
bFactor = bLiteral

buildRelOp :: BoolExpression -> ((BoolExpression -> BoolExpression -> BoolExpression), BoolExpression) -> BoolExpression
buildRelOp expressionA (op, expressionB) = op expressionA expressionB
~~~

Most of the above reflects the relevant parts of `Expression` module.  

To round out the simple parts we can add in boolean variables.

~~~ Haskell
data BoolExpression = 
    BTrue
    | BFalse
    | BVar String
    | BOr BoolExpression BoolExpression
    | BAnd BoolExpression BoolExpression

bVar :: Parser BoolExpression
bVar = letters >>> BVar

bFactor :: Parser BoolExpression
bFactor = bLiteral
      <|> bVar
~~~

## This is easy...NOT!

Again I chose the symbol that is familiar to me to implement as my boolean negative `!`.

~~~ Haskell
bNot :: Parser(BoolExpression -> BoolExpression)
bNot = token(literal '!') >>> (\_ -> BNot)
~~~  

We can integrate this into `bFactor` so that we can negate entire factors. 

~~~ Haskell
data BoolExpression = 
	BVar String
	| BTrue
	| BFalse
	| BAnd BoolExpression BoolExpression
	| BOr BoolExpression BoolExpression
	| BNot BoolExpression
	| BExp Expression

bFactor :: Parser BoolExpression
bFactor = bNot <+> bLiteral >>> (\(n, lit) -> n lit)
      <|> bLiteral
      <|> bNot <+> bVar >>> (\(n, lit) -> n lit)
      <|> bVar
~~~

This will always look for the negative first and if found will use the `BNot` data constructor resulting in wrappings like `BNot (BVar "a")`

## Getting relational

Now comes the fun part.  A lot of conditions are written in the form of `a > b` including using `Expression`s e.g. `a > 2` or `a * 3 >= limit`.  A boolean factor can be a relation which is defined as an expression optionally followed by any number or relational operator and expression pairs.

    <b-factor>	   ::= <b-literal> | <b-variable> | <relation>
    <relation>	   ::= <expression> [<relop> <expression>]

Relational operators, or comparison operators, come in several flavours.  Once again I've gone with the symbolic representations rather than words or mnemonics.  Alternatives such as `<>` for `RNotEqual` can easily be substitued or even added alongside.

~~~ Haskell
data BoolExpression = 
	...
	| REqual BoolExpression BoolExpression
	| RNotEqual BoolExpression BoolExpression
	| RGreaterThan BoolExpression BoolExpression
	| RLessThan BoolExpression BoolExpression
	| RGreaterThanOrEqualTo BoolExpression BoolExpression
	| RLessThanOrEqualTo BoolExpression BoolExpression
	deriving (Show)

relOp :: Parser (BoolExpression -> BoolExpression -> BoolExpression)
relOp = token(accept ">=") >>> (\_ -> RGreaterThanOrEqualTo)
    <|> token(accept "<=") >>> (\_ -> RLessThanOrEqualTo)
    <|> token(literal '>') >>> (\_ -> RGreaterThan)
    <|> token(literal '<') >>> (\_ -> RLessThan)
    <|> token(accept "==") >>> (\_ -> REqual)
    <|> token(accept "!=") >>> (\_ -> RNotEqual)
 ~~~ 

The output from `relOp` can be used in `buildRelOp` similar to the way `boolOp` is without needing to change anything.  

When attempting to write a `relExpression` function I first tried to use `Cradle.Grammar.Expressions.expression` directly which resulted in type errors.  `expression` has a type of `Parser Expression` where as all out boolean expressions need to use `Parser BoolExpression`.  I persisted for a while until I realised the easiest way was to simply wrap the `Expression` in a `BoolExpression` using a new data constructor `BExp`.

~~~ Haskell
module Cradle.Grammar.Boolean

where

import Cradle.Parser
import Cradle.Grammar.Expressions

data BoolExpression = 
	...
	| BExp Expression
	deriving (Show)

bFactor :: Parser BoolExpression
bFactor = relExpression
	  <|> bNot <+> bLiteral >>> (\(n, lit) -> n lit)
      <|> bLiteral
      <|> bNot <+> bVar >>> (\(n, lit) -> n lit)
      <|> bVar

relExpression :: Parser BoolExpression
relExpression = bExpression +> relExpression'
relExpression' e = relOp <+> bExpression >>> buildRelOp e +> relExpression'
               <|> result e

bExpression :: Parser BoolExpression
bExpression = token expression >>> BExp
~~~


