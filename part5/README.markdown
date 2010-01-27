#Let's build a compiler (in Haskell): Part 5 - Doing it with Types

Up until now, I have been doing a near direct translation of Crenshaws tutorial to Haskell.   After reading Andersons paper _Parsing with Haskell_ [1] I realised that Haskell has a very powerful Type system for a reason.

## Defining the Types

So let's take a step back and rework what has been done so far using Types and we'll see how much easier, extensible and safer it is to use.

First of all a recap of our language elements we have defined so far.

    <expression> ::= <term>[<addop><term>]*
    <term> ::= <factor>[<mulop><factor>]*
    <factor> ::= [0-9]|<expression>

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

    parse :: String -> [Expression]
    parse [] = []
    parse (x:xs)
        | isDigit x = Num (digitToInt x) : parse xs

Wow, we are back to 3 articles ago. Well actually not quite, as this does not emit any assembly. We can fix that by creating an new `emit` funtion and then we can create a shortcut to parse and emit.

    -- Prefix a string with a tab
    emitSt s = "\t" ++ s
    
    -- Prefix a string with a tab and postfix it with a new line
    emitLn s = (emitSt s) ++ "\n"
    
    emit :: [Expression] -> String
    emit [] = ""
    emit (expr:rest) = case expr of 
         Num a -> emitLn ("MOV eax, " ++ (show a)) ++ emit rest
    
    parseAndEmit :: String -> String
    parseAndEmit = emit . parse

## Back to the Future Part 2 : Addition
