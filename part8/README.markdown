#Let's build a compiler (in Haskell): Part 8 - Expressions with Combinators

In case you forgot, here is what our definition for expression is:

	<expression> ::= <term>[<addop><term>]*
	<term> ::= <factor>[<mulOp><factor>]*
	<factor> ::= <number>|<variable>
	<addOp> ::= +|-
	<mulOp> ::= *|-
	<variable> ::= a-z
	<number> ::= 0-9

## Rebuilding factors

Starting from the bottom up, we'll define a `number` parser. Our `Num` constuctor requires an `Int`, but so far we only have a `digit` parser that returns a `Parser Char`.  Let's define a new parser that turns this into a real integer.

	digitVal :: Parser Int
	digitVal = digit >>> digitToInt
	
We have a new combinator, `>>>`, that transforms a result of a parser. In this case it changes the result from `Parser Char` to `Parser Int`.  This combinator simply applies the second function to the first parsers result.

	infixl 5 >>>
	(>>>) :: Parser a -> (a -> b) -> Parser b
	(m >>> n) cs = case m cs of
		Nothing -> Nothing
		Just (a, cs') -> Just (n a, cs')
		
Now that we have the `>>>` combinator and a parser for recognising single integer value, we cane define `number` as:
	
	number :: Parser Expression
	number = digitVal >>> Num

Similarly with the the `>>>` combinator and a parser for recognising single characters, `var` becomes pretty straigt forward as well.

	var :: Parser Expression
	var = letter >>> Var

Finally we get to `factor`, which we already have the component pieces for, looks almost exactly like the definition.

	factor :: Parser Expression
	factor = number <|> var
	
## Rebuilding term

Here is where things get a bit tricky. It took me a while to actually figure out how Anderson managed to do this, but I will try to explain it as best I can.

A term is defined as a a series of factors seperated by multiplication or division operators.  Like always, we'll start with the simple binary solution.

	term = factor <+> mulOp <+> factor
	mulOp = literal '*' <|> literal '/'

The result of `term` would be something like `1*2` would be `Just (((Num 1,'*'),Num 2),"")` which is clearly not a valid expression.  We need to get it in the form of `Just (Mul (Num 1) (Num 2), "")`. So lets pass the result onto a transformation function called `buildOp`.

	term = factor <+> mulOp <+> factor >>> buildOp
	mulOp = literal '*' <|> literal '/'
	buildOp ((e1, op) e2) 
		| op == '*' = Mul e1 e2
		| op == '/' = Div e1 e2
		| otherwise = error "Unknown operation"
	
In this definition `buildOp` uses some guards to figure out which operation to build, and also has a call to `error`. Through the wonders of partial functions, we can actually rewrite `mulOp` to tell us which data constuctor to use, which means `buildOp` gets simplified and won't have a chance to fail.

	term = factor <+> mulOp <+> factor >>> buildOp
	mulOp = literal '*' >>> (\_ -> Mul)
	    <|> literal '/' >>> (\_ -> Div)
	buildOp :: ((Expression, Expression -> Expression -> Expression), Expression) -> Expression
	buildOp ((e1, op), e2) = op e1 e2

That's a lot of expressions in that type def there. An additional benefit of this method is that when we get to `addOp` we don't need to change `buildOp` at all - it'll just work.

So that's binary. We have the basic process so now let's extend it to the n-ary version. What we want to do is find the first `factor` and then pass that onto a function `term'` that continusouly builds an `Expression` based on the parsing `[<mulOp><factor>]`. So let's look at `term'` first.

	term' e = mulOp <+> factor >>> buildOp e +> term'
      <|> result e

This function takes an `Expression`, parses out a mulOp and another factor, passes the original `Expression`, the mulOp and the new factor into buildOp and then sends this new `Expression` through itself again in a recursive manner until it can no longer parse a mulOp and factor, at which point it returns the `Expression` built so far.

Before we get onto `result` and  `+>` we need to revisit `buildOp`.  This function now takes an accumulator, and only gets the new operation and factor as a pair. So let's quickly redefine it.

	buildOp e (op, e') = op e e'

Right. `result` is a parser that simply returns what it is given wrapped up as a `Parser` type.  This lets us do things such as define default alternatives, or in this case, return the accumulated result. 

	result :: a -> Parser a
	result a cs = Just(a,cs)
	
Onto '+>'. We know that `term'` expects only an `Expression`, but the use of '>>>' will give us a `Parser`. `+>` is used to extract only the raw result for use in another function.

	infix 4 +>
	(+>) :: Parser a -> (a -> Parser b) -> Parser b
	(m +> k) cs = case m cs of
		Nothing -> Nothing
		Just (a, cs') -> k a cs'

We can finish up our new `term` based on combinator with a simple function that grabs the first factor and then uses `term'`.  For completeness, this is all the term parsers.

	mulOp = literal '*' >>> (\_ -> Mul)
		<|> literal '/' >>> (\_ -> Div)

	term = factor +> term'	

	term' e = mulOp <+> factor >>> buildOp e +> term'
		  <|> result e
		  
	buildOp e (op, e') = op e e'
	
## Rebuilding expression

All that hard thinking for terms, can be simply reused for expressions.

	addOp = literal '+' >>> (\_ -> Add)
		<|> literal '-' >>> (\_ -> Sub)
	
	expr :: Parser Expression
	expr = term +> expr'

	expr' :: Expression -> Parser Expression
	expr' e = addOp <+> term >>> buildOp e +> expr'
		  <|> result e
		  
## Putting all together

Now that we can parse these lovely expressions, we should probably plug it into our `assign` function.  All we need to do is change the call to `digit` to expression.  We can then change `parse` to remove the call to Num and read and simply return the expression created.

	parse :: String -> Assign
	parse s = Assign id expr
		where (id, expr) = case assign s of 
				Nothing -> error "Invalid assignment"
				Just ((a, b), _) -> (a, b)
					   
	assign = letter <+-> literal '=' <+> expr
		
## Operator precedence and adding parentheses

You may (or may not) have noticed that although we didn't try to, we got operator precdence working correctly.  The precedence between `mulOp` and `addOp` fell out due to the way we defined our expressions.  

However, the left associativety, i.e. strings such as "8/4/2" should be parsed as "(8/4)/2" and not "8/(4/2)", was due to the way we defined `buildOp`.  We could confuse everyone and create a right associative expression parser simply by changing the order of the expressions in `buildOp`. We wont because we are nice, but it's nice to know we can.

But what about when you really do mean "8/(4/2)". In regular math we need to use parentheses, and so shall our parser. Where do we define this?  With a bit of knowledge and thinking, you might realise that a parenthesised expression can be considered to be a factor, giving us a new definition for factor:

	<factor> ::= <number>|<variable>|(<expression>)

If you tried to implement this is the old parser style, you'd have a hard time. I tried. I failed. Conincidentally this is the reason I went look for a better way and found out about combinators.  The story is a little different now.

	factor :: Parser Expression
	factor = number 
		 <|> literal '(' <-+> expr <+-> literal ')'
		 <|> var

I've had to put the parenthesised factor before `var` because latter on when we expand `var` to handle multi-character variables it will tend to get in the way of looking for an openeing parentheses. 
		 
## Adding real integers and variables

We've been working on the assumption that everything is a single character because it kept things easy.  However, with our new techniques, it is not very hard at all to extend this to multi-characters.

Looking at variables first, if we extend them to be any number of letters we would write it as:

	letters :: Parser String
	letters :: iter letter

`iter` is a function that iterates over the input string until 'letter' fails, joining the results as we go.

	iter :: Parser a -> Parser [a]
	iter m = m <+> iter m >>> cons 
		 <|> result []
			
	cons :: (a, [a]) -> [a]
	cons (hd, tl) = hd:tl

Pretty simple - no new parsers or combinators in there.  If we change the `Var` data constructor to use `String`, and alter `var` to use `letters` instead of `letter`, we can now use arbitraty length variables.

Real integers is a little more difficult.  We can't use `iter` with `digitToInt` because `iter` will return a string where `digitToInt` expects only a char. We also can't use `read` to do the value conversion because it will through expceptions, which means we won't be able to try alternatives.

What we end up doing is using a similar technique to the expression builders we used in `term'` and `expr'`.

	integer :: Parser Int
	integer = digitVal +> integer'

	integer' :: Int -> Parser Int
	integer' n = digitVal >>> buildNumber n +> integer'
			 <|> result n

	buildNumber :: Int -> Int -> Int
	buildNumber n d = 10 * n + d

We find the first digit value, then use a recursive function to continually find another digit value, and keep the accumulator running.

Change `number` to use `integer` and now we have arbitraty length integers. 

If you change `Assign` to a String and `assign` to use `letters`, we can now do amazing things like:

	*Main> parse "longName=value+1324*(15+green)"
	Assign "longName" (Add (Var "value") (Mul (Num 1324) (Add (Num 15) (Var "green"))))

## Adding whitespace

Now that we have `iter` we can write a function that chews up all the whitespace between the stuff we are interested in.  These things are generally called tokens so we will call out function `token`.

	token :: Parser a -> Parser a
	token = (<+-> iter space)

This uses `iter` in conjuntion with `<+->` which, as you will recall, drops the right hand side from the result.  We can use token like so:

	factor :: Parser Expression
	factor = token number 
		 <|> token (literal '(') <-+> token expr <+-> token (literal ')')
		 <|> token var

If you also add `token` to `mulOp` and `addOp` you can now do even more amazing things like:

	*Main> parse "longName=value + 1324 * ( 15 + green )"
	Assign "longName" (Add (Var "value") (Mul (Num 1324) (Add (Num 15) (Var "green"))))
	*Main> parse "longName=value + 1324 * (15+green)"
	Assign "longName" (Add (Var "value") (Mul (Num 1324) (Add (Num 15) (Var "green"))))
	
[Download lbach.hs](http://github.com/alephnullplex/cradle/blob/master/part8/lbach.hs)
	
## Next
I know I mentioned in the last article that I would restucture our compiler into modules, but this installment is already pretty long with a lot of new things in it.  Program organisation is not a small dicussion so I will leave it for next time.