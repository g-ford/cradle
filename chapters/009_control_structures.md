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

First we start with some data types.  For now we will make all our `Statments` be of the `Assign` type.  This will allow us to create a simple 'program' like `a=1 end`.

~~~ Haskell
data Program = Program Block deriving (Show)
type Block = [Statement]
data Statement = Statement Assign deriving (Show)
~~~

We'll create a parser for each of the components from the bottom up to see how they feed into each other. We also need to update the existing `assign` parser to return a `Parser Assign`.  This will break the main parsing function but we'll update that to parse `Program` later.

~~~ Haskell
statement :: Parser Statement
statement = assign >>> Statement

block :: Parser Block
block = iterS statement

program :: Parser Program
program = block <+-> accept "end" >>> Program

assign :: Parser Assign
assign = token(letters) <+-> token(literal '=') <+> expression >>> (\(x, y) -> Assign x y)
~~~

Currently `statement` is a wrapper for `assign` but we will add branching and looping later.  `block` uses the non-space-checking version of our iteration combinator `iterS` which will create a list of `Statements`.  Finally `program` calls `block` and then uses a new combinator `accept` to detect the end keyword.  

`accept` is defined using existing combinators to `token`ize a list of `letters` that match the input.

~~~ Haskell
accept :: String -> Parser String
accept w = token (letters <=> (==w))
~~~

Now we can check the output of `program`.  We can now detect valid and invalid programs and multiline programs.  An added bonus is that we can use keywords in our variables.

~~~ Haskell
*Main> program "a=1 end"     
Just (Program [Statement (Assign "a" (Num 1))],"")
*Main> program "end=1 end"    
Just (Program [Statement (Assign "end" (Num 1))],"")
*Main> program "1 end"        
Nothing
*Main>program "a=1 x=a+1 end"
Program [Statement (Assign "a" (Num 1)),Statement (Assign "x" (Add (Var "a") (Num 1)))]
~~~

After updating the main module to use the new `program` we now start to have a more useful compiler.

~~~ Haskell
main :: IO ()
main = getArgs >>= p . head

parse :: String -> Program
parse s = case program s of 
            Nothing -> error "Invalid program"
            Just (a, b) -> a

-- | Parse and print. Utility and test function for use in @ghci@.
p = putStrLn . show . parse

-- | Parse and emit.
-- The emitter does not know how to handle a Program yet
-- e = putStrLn . emit . parse 
~~~

## If-Then Statements

The first control structure we will look at is the basic if statement.  This takes a condition, and if the condition is true, it will execute the body of the if statment.

The hardest part of this section is deciding what our syntax will be.  To keep things simple, we will stick to Crenshaws recommended syntax of 

    if <condition> <block> end
    
By having an explicit block terminator `end` we avoid the ambiguity of deciding 'is this still part of the if body?'  

We create a new `Statement` type constructor which I called `Branch`.  A `Branch` will hold the `Condition` and the body of the if statement, which is really just another `Block`.  For now a condition will just be a string. 

~~~ Haskell
data Statement = Statement Assign 
  | Branch Condition Block
  deriving (Show)

type Condition = String
~~~

The parser is quite simple to write using existing parsers and combinators.  I've created a `tempPlaceholder` that accepts anything other than keywords and used that as a fill-in for `condition` until we get to relational algebra.  We also update `statment` to use the new `ifthen` as well.

~~~ Haskell         
statement :: Parser Statement
statement = assign >>> Statement
  <|> ifthen 

ifthen :: Parser Statement		
ifthen = accept "if" <-+> condition <+> block <+-> accept "end" >>> buildBranch
    where buildBranch (c, b) = Branch c b

condition = tempPlaceholder

-- |This is a temporary parser that accepts anything except keywords
tempPlaceholder :: Parser String
tempPlaceholder = token letters <=> (\x -> not $ any (==x) keywords) 
  where keywords = ["if", "else", "end", "while", "until"]
~~~
        
Because we use `block` as part of the function, we can parse multi-statement bodies, and even nested if statments such as:

~~~ Haskell
*Main> ifthen "if cond a=1 if cond b=2 end end "
Just (Branch "cond" [Statement (Assign "a" (Num 1)),Branch "cond" [Statement (Assign "b" (Num 2))]],"")
*Main> let pro = "x=0 \nif a \n\tx=1 \n\tif b \n\t\tb=3 \n\tend \nend \nb=4 \nend"
*Main> putStrLn pro
x=0
if a
	x=1
	if b
		b=3
	end
end
b=4
end
*Main> program pro
Just (Program [Statement (Assign "x" (Num 0)),
               Branch "a" [Statement (Assign "x" (Num 1)),
                           Branch "b" [Statement (Assign "b" (Num 3))]
                          ],
               Statement (Assign "b" (Num 4))
              ],"")
~~~

Got to love the power of recursion.

## If-Else

Now that we have the basic concepts of creating branches, extending it to the if-else statement not so hard. Extending the definition of if to include the else statement looks like:

    if <condition> <block> [ else <block>] end

You could treat `else` as an optional part of `ifthen` but I chose to seperate them into seperate type constructors and functions. 

~~~ Haskell
data Statement = 
	Statement Assign 
  | Branch Condition Block
  | Branch2 Condition Block Block
  deriving (Show)
              
statement :: Parser Statement
statement = assign >>> Statement
  <|> ifelse
  <|> ifthen 
              
ifelse :: Parser Statement		
ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> buildBranch
    where buildBranch ((c, b1), b2) = Branch2 c b1 b2
~~~
    
As you can see, it is much the same as the basic if statement. The only difference is that we add a second `Block` to the type constructor to allow for the else body.  Again, recusrion allows for nested if statements in nested if-else statements all the way down to turtles.

## Basic loop with while

Parseing a while loop is actually extremely similar to the `ifThen` we have already done.  The real difference comes down to how the code generator treats them. Our while looks like:

	while <condition> <block>

