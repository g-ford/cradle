---
layout: layout
title: "Basic Control Structures"
group: book
permalink: cbasic_control_structures.html
---

# Basic Control Structures

In this chapter we will start implementing programming structures beyond the basic universal mathmatical expressions.  Some decisions need to made first on what the new language looks like.  We will look back to Crenshaw and emulate his simple procedural style language based on Pascal. The specifics of the language being implemented do not really effect the way the compiler is constructed at this early stage.

## Multiple Statments

The single statement assignments we have been creating until now are not particularly useful when we start getting into branching and looping.  We need a construct that can hold multiple statments which we can call `Program`. We'll define a `Program` as a collection, or `Block` of `Statment`s ending with the keyword `end`.

First we start with some data types.  For now we will make all our `Statments` be of the `Expression` type.  This will allow us to create a simple 'program' like `1+1 end`

~~~ Haskell
data Program = Program Block deriving (Show)
type Block = [Statement]
data Statement = Statement Expression deriving (Show)
~~~

We'll create a parser for each of the components from the bottom up to see how they feed into each other. 

~~~ Haskell
statement :: Parser Statement
statement = expression >>> Statement

block :: Parser Block
block = iterS statement

program :: Parser Program
program = block <+-> accept "end" >>> Program
~~~

Currently `statement` is a wrapper for `expression` but we will add branching and looping later.  `block` uses the non-space-checking version of our iteration combinator `iterS` which will create a list of `Statements`.  Finally `program` calls `block` and then uses a new combinator `accept` to detect the end keyword.  

`accept` is defined using existing combinators to `token`ize a list of `letters` that match the input.

~~~ Haskell
accept :: String -> Parser String
accept w = token (letters <=> (==w))
~~~

At this point we run into the classic problem of distinguishing keywords from variables.  We have a few options here:

# Redefine our algerbraic data types to make variables different from expressions so that we can limit where they are detected.
# Backtrack whenever we find an `Expression` containing a single `Var`
# Exclude keywords from being valid variables


