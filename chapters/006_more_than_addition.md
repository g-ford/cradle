---
layout: layout
title: "More than addition"
group: book
permalink: more_than_addition.html
---

# More than addition

Introducing multiplication and division into out compiler means that we have to think about operator precedence if we want our language implementation to adhere to the general conventions of math. We all know from basic math that `1+2*3` should be interpreted as `1+(2*3)`.  

As it turns out, there is a rather simple trick to this - define `term` in terms of multiplication.

    <term> := <factor>[<mulop><factor>]*

Now we call a single digit a `factor`, and a `term` is any result of a multiplication operation involving these factors.  What this effectively means is that all multiplication has happened before we get to addition, and thus operator precedence is preserved.

Before we start implementing this we will need some new data constructors to handle the `Mul` and `Div`. We can extend the existing `Expression` quite simply.  We'll also need a `mulOp` similar to `addOp`. 

~~~ Haskell
data Expression = 
  Num Char 
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  deriving (Show) 

mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = literal '*' >>> (\_ -> Mul)
    <|> literal '/' >>> (\_ -> Div)
~~~

Factor is our new jargon for single digits so we can simply rename `term` to `factor`.  We then need to define a new `term` that looks very similar to `expression`.

~~~ Haskell
term :: Parser Expression
term = factor +> term'
term' e = mulOp <+> term >>> buildOp e +> term'
      <|> result e

factor :: Parser Expression
factor = digit >>> Num
~~~

And that's really all we need to do.  We now have the four basic math operators obeying the laws of operator precedence.  A little play in `GHCi` shows that the stack is built correctly - multiplication before addition moving left to right.

~~~ 
*Main> expression "1+7*3"
Just (Add (Num '1') (Mul (Num '7') (Num '3')),"")
*Main> expression "1+7*3-4"
Just (Sub (Add (Num '1') (Mul (Num '7') (Num '3'))) (Num '4'),"")
~~~

## More operator precedence

The precedence between `mulOp` and `addOp` fell out due to the way we defined our expressions. Left associativety, i.e. strings such as "8/4/2" should be parsed as "(8/4)/2" and not "8/(4/2)", was due to the way we defined `buildOp`.  We could confuse everyone and create a right associative expression parser simply by changing the order of the expressions in `buildOp`. We won't because we are nice, but it's nice to know we can.

But what about when you really do mean "8/(4/2)"? In regular math we need to use parentheses, and we shall follow conventions here too. With a bit of knowledge and thinking, you might realise that a parenthesised expression can be considered to be a factor, giving us a new definition for factor:

	<factor> ::= <number>|(<expression>)

This is surprisingly easy now.

~~~ Haskell
factor :: Parser Expression
factor = digit >>> Num 
     <|> literal '(' <-+> expression <+-> literal ')'
~~~

And checking it we can see that parenthesised expressions take precedence.

~~~ Haskell
*Main> expression "1+7*3-4"
Just (Sub (Add (Num '1') (Mul (Num '7') (Num '3'))) (Num '4'),"")
*Main> expression "1+7*(3-4)"
Just (Add (Num '1') (Mul (Num '7') (Sub (Num '3') (Num '4'))),"")
~~~

## More than single digits

We've been working on the assumption that everything is a single character because it kept things easy.  However, with our new techniques, it is not very hard at all to extend this to multi-characters.

Looking at variables first, if we extend them to be any number of letters we would write it as:

~~~ Haskell
letters :: Parser String
letters :: iter letter
~~~

`iter` is a function that iterates over the input string until 'letter' fails, joining the results as we go.

~~~ Haskell
iter :: Parser a -> Parser [a]
iter m = m <+> iter m >>> (\(x, y) -> x:y) 
	 <|> result []
~~~	

Pretty simple - no new parsers or combinators in there.  If we change the `Assign` data constructor to use `String`, and alter `assign` to use `letters` instead of `letter`, we can now use arbitrary length variables in our assignment statements.

~~~ Haskell
data Assign = Assign String Expression 
	deriving Show

assign :: Parser (String, Expression)
assign = letters <+-> literal '=' <+> expression
~~~

~~~ Haskell
*Main> parse "asdf=1"
Assign "asdf" (Num '1')
~~~

We can something very simialr for multi-character numbers.  A few type changes later we can simply keep collecting numbers until `digit` fails.  We need to change the order of the alternatives in `factor` to avoid the greedy nature of `iter`.

~~~ Haskell
data Expression = 
  Num String 
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  deriving (Show) 

factor :: Parser Expression
factor = literal '(' <-+> expression <+-> literal ')'
	 <|> digits >>> Num

digits :: Parser String
digits = iter digit
~~~

And now we are starting to get somewhere.

~~~ Haskell
*Main> parse "asdf=(123+53)*5"
Assign "asdf" (Mul (Add (Num "123") (Num "53")) (Num "5"))
~~~

## Adding whitespace

Now that we have `iter` we can write a function that chews up all the whitespace between the stuff we are interested in.  These things are generally called tokens so we will call our function `token`.

~~~ Haskell
token :: Parser a -> Parser a
token = (<+-> iter space)
~~~

This uses `iter` in conjunction with `<+->` which, as you will recall, drops the right hand side from the result.  We can use token like so:

~~~ Haskell
assign :: Parser (String, Expression)
assign = token(letters) <+-> token(literal '=') <+> expression

expression :: Parser Expression
expression = token(term) +> expression'
expression' e = addOp <+> term >>> buildOp e +> expression'
            <|> result e

factor :: Parser Expression
factor = token number 
	 <|> token (literal '(') <-+> token expr <+-> token (literal ')')
	 <|> token var
~~~

If you also add `token` to `mulOp` and `addOp` you can now do even more amazing things like:

~~~ Haskell
*Main> parse "asdf = (123 + 53) * 5"
Assign "asdf" (Mul (Add (Num "123") (Num "53")) (Num "5"))
*Main> parse "asdf = ( 123 + 53 ) * 5"
Assign "asdf" (Mul (Add (Num "123") (Num "53")) (Num "5"))
~~~

## More than numbers in expressions

We have all the infrastucture to add variables into `Expression` so that we can write things like `a = x * y`.  First we will need to extend `Expression` to include a `Var` data constructor which takes a string similar to `Num`.  We will then need to figure out where `Var`s can be created - and seeing as they are stand ins for `Num` we can add a pattern to to `factor` to accomodate them.

If you simply add the new pattern to `factor` you will run into some issues due to the way `iter` works. For the alternatives to be processed we need the parsers to return `Nothing` on failure but `iter` will never fail - rather it will return an empty list.  This was alluded to earlier when we needed to put the parenthesis checking before the number checking.  

To make `iter` conform to our `Parser` type we will rewrite it to check if the result is an empty list then it should be return `Nothing`.  To do this we can pass the result of `iter` through our comparison combinator like so.

~~~ Haskell
iter :: Parser Char -> Parser String
iter m = (iterS m) <=> (/="")
iterS m = m <+> iterS m >>> (\(x, y) -> x:y)
	 <|> result []
~~~

The one exception to using the new `iter` everywhere is `token` as is is optional space so it should not return a `Nothing`. `token` will need to use the sub `iterS`.

~~~ Haskell
token :: Parser a -> Parser a
token = (<+-> iterS space)
~~~

Now we can write add the new pattern to `factor` safely.

~~~ Haskell
data Expression = 
  Num Integer 
  | Var String
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  deriving (Show) 

factor :: Parser Expression
factor = token (literal '(') <-+> token(expression) <+-> token(literal ')')
	 <|> digits >>> Num
	 <|> letters >>> Var
~~~

And let's check that it looks good.

~~~ Haskell
*Main Data.Char> parse "a = 1 * y"
Assign "a" (Mul (Num "1") (Var "y"))
*Main Data.Char> parse "alpha = 13 * ( yaxis + 234 )"
Assign "alpha" (Mul (Num "13") (Add (Var "yaxis") (Num "234")))
~~~

## More than strings

Sometimes we may want a parser to return something other than a string value.  For example we would prefer that `Num` use a real number rather than a string representation.  We have the transform combinator at our disposal so this is not really difficult to do.

Because we now have a safe iterator being used in `digits` we can use `read` to convert the string to an integer.  I tried doing this before changing `iter` and the empty string was cases `read` to throw and unrecoverable exception. We could use `reads` which can be recovered from if it fails to read but it over complicates things a fair bit.  Now that `digits` is gaurenteed to return a string with at least one digit or `Nothing` we can safely use `read` and keep things simple.

~~~ Haskell
data Expression = 
  Num Integer 
  | Var String
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  deriving (Show) 

factor :: Parser Expression
factor = token (literal '(') <-+> token(expression) <+-> token(literal ')')
	 <|> number >>> Num
	 <|> letters >>> Var

number :: Parser Integer
number = literal '-' <-+> digits >>> (\n -> -1 * (read n :: Integer) )
     <|> digits >>> (\n -> read n :: Integer)
~~~

We've changed the `Num` data constructor to use and integer and created a new `number` parser that returns a `Parser` wrapped `Integer` using `read`.  This is then used in `factor` instead of the basic `digits`. I also slipped the ability to prefix a number with a sign allowing expressions such as `-1 * 4`.



## More than...?

We've come a long way in this chapter. From single digit addition with no spacing to being able to use multicharachter terms and factors, adding in multiplication, parenthesis and variables and using whitespace to make the equations look nicer.  And it was all rather simple due to the combination of small simple functions.

