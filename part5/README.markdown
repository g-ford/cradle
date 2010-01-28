#Let's build a compiler (in Haskell): Part 5 - Doing it with Types

Up until now, I have been doing a near direct translation of Crenshaws tutorial to Haskell.   After reading Andersons paper _[Parsing with Haskell](http://www.cs.lth.se/EDA120/assignment4/parser.pdf)_  I realised that Haskell has a very powerful Type system for a reason.

## Defining the Types

So let's take a step back and rework what has been done so far using Types and we'll see how much easier, extensible and safer it is to use.

First of all a recap of our language elements we have defined so far.

    <expression> ::= <term>[<addop><term>]*
    <term> ::= <factor>[<mulop><factor>]*
    <factor> ::= <number>

And translating that into Haskell is actually very similar:

    data Expression = Add Term Term
                    | Sub Term Term
    
    data Term = Mul Factor Factor
              | Div Factor Factor
    
    data Factor = Num Int

However using this means that to define an expression we need to use something like:

    *Main> let y = Add (Mul (Num 1) (Num 2)) (Mul (Num 1) (Num 3))
    *Main> :t y
    y :: Expression

That is a wee bit cumbersome. To add 2 and 3 I have to multiply each by 1. Another problem arises when we want to then use that expression in another expression.

    *Main> let z = Add y y

    <interactive>:1:12:
        Couldn't match expected type `Term'
               against inferred type `Expression'
        In the first argument of `Add', namely `y'
        In the expression: Add y y
        In the definition of `z': z = Add y y
*Main>

With a little bit of simplification we can refactor the types to a single type.
    
    data Expression = Num Int
                    | Add Expression Expression
                    | Sub Expression Expression
                    | Mul Expression Expression
                    | Div Expression Expression
                    deriving (Show) 

A bit of playing around in the interpretor shows that because everything is now an `Expression` we can much more easily combine and recombine these easily and intuituvely.

    *Main> :l lbach.hs
    [1 of 1] Compiling Main             ( lbach.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> let y = Num 1
    *Main> let z = Add y y
    *Main> :t z
    z :: Expression
    *Main> let a = Sub (Num 3) (Mul (Num 2) (Num 3))
    *Main> :t a
    a :: Expression
    *Main> let b = Add z a
    *Main> :t b
    b :: Expression
    *Main> show b
    "Add (Add (Num 1) (Num 1)) (Sub (Num 3) (Mul (Num 2) (Num 3)))"

## Back to the Future : Single Integers

Now that we have this amazing Type `Expression` how do we parse input strings with it?

    parse :: String -> Expression
    parse (x:[])
        | isDigit x = Num (digitToInt x) 

Wow, we are back to 3 articles ago. Well actually not quite, as this does not emit any assembly. We can fix that by creating an new `emit` funtion and then we can create a shortcut to parse and emit.

    -- Prefix a string with a tab
    emitSt s = "\t" ++ s
    
    -- Prefix a string with a tab and postfix it with a new line
    emitLn s = (emitSt s) ++ "\n"
    
    emit :: Expression -> String
    emit expr = case expr of 
                Num a   -> emitLn ("MOV eax, " ++ (show a))
    
    parseAndEmit :: String -> String
    parseAndEmit = emit . parse

## Back to the Future Part 2 : Addition & Multiplication

Defining the basic addition is now a little simpler as well.

parse :: String -> Expression
parse (x:y:zs) 
    | y == '+'  = Add (parse [x]) (parse zs)
    | y == '-'  = Sub (parse [x]) (parse zs)
parse (x:[])
    | isDigit x = Num (digitToInt x)

This form also handles multiple additions, and extends nicely to multiplication.

parse :: String -> Expression
parse (x:y:zs) 
    | y == '*'  = Mul (parse [x]) (parse zs)
    | y == '/'  = Div (parse [x]) (parse zs)
    | y == '+'  = Add (parse [x]) (parse zs)
    | y == '-'  = Sub (parse [x]) (parse zs)
parse (x:[])
    | isDigit x = Num (digitToInt x)
    
##Back to the Future Part 3 : Opertator Precedence

There's a small problem with the redefined `parse` above - it does not honour operator precedence.  To get this we have to look a little futher ahead in the string. Because we are only dealing with single digits, this is rather easy.

parse :: String -> Expression
parse (a:b:c:d:ds) 
    | d == '+'  = Add (parse [a,b,c]) (parse ds)
    | d == '-'  = Sub (parse [a,b,c]) (parse ds)
parse (x:y:zs) 
    | y == '*'  = Mul (parse [x]) (parse zs)
    | y == '/'  = Div (parse [x]) (parse zs)
    | y == '+'  = Add (parse [x]) (parse zs)
    | y == '-'  = Sub (parse [x]) (parse zs)
parse (x:[])
    | isDigit x = Num (digitToInt x)

## Getting emit to work.

The last thing to get us back up to spec is to actually emit all the assembly again.  We'll just extend `emit` with more cases that should look pretty familir.

-- Turns an expression into the equvilent assembly
emit :: Expression -> String
emit expr = case expr of 
     Num a   -> emitLn ("MOV eax, " ++ (show a))
     Add a b -> emit a ++  pushEax ++ emit b ++ add
     Sub a b -> emit a ++  pushEax ++ emit b ++ sub
     Mul a b -> emit a ++  pushEax ++ emit b ++ mul
     Div a b -> emit a ++  pushEax ++ emit b ++ divide
     
## Why? What's it all about?

We now have two phases to the compiler.  A parse phase and an emit phase.  By breaking the two up we allow a little more flexibility and options down the road.  For example, now that emit is not embedded in the parse phase, we can much more easily drop in a [Lexer](http://en.wikipedia.org/wiki/Lexical_analysis).

The intermediate representation - `Expression` - is known as a [Parse Tree](http://en.wikipedia.org/wiki/Parse_tree) which we can then pass through optimisations or write an intepretor for as well as our compiler, or have multiple compilers if needed e.g. an ARM asm compiler.  