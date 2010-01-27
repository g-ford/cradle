#Let's build a compiler (in Haskell): Part 5 - Doing it with Types

Up until now, I have been doing a near direct translation of Crenshaws' tutorial to Haskell.   After reading Andersons' paper _Parsing with Haskell_ [1] I realised that Haskell has a very powerful Type system for a reason.

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

That is a bit cumbersome.  Another problem arises when we want to then use that expression in another expression.

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

