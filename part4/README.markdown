#Let's build a compiler (in Haskell): Part 4 - Operator Precedence 

## Operator Precedence

Introducing multiplication and division into out compiler means that we have to think about operator precedence if we want our language implementation to adhere to the general conventions of math.

As it turns out, there is a rather simple trick to this - define `term` in terms of multiplication.

    <term> := <factor>[<mulop><factor>]*

Now we call a single digit a `factor`, and a `term` is any result of a multiplication operation involving these factors.  What this effectively means is that all multiplication has happened before we get to addition, and thus operator precedence is preserved.

We will need some additional functions to detect and emit multiplication and division, which should look very familiar. I've also taken the opportunity to remove a bit of redundancy. 

    -- Basic math functions
    popEbx = emitLn "POP ebx"
    pushEax = emitLn "PUSH eax"
    add = popEbx ++ emitLn "ADD eax, ebx"
    sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
    mul = popEbx ++ emitLn "MUL ebx"
    divide = popEbx ++ emitLn "DIV ebx" 

Now that our single digits are factors and not terms, we need to rename `term` to `factor` and rewrite our `expression` based on `term`.  Again there has been some refactoring.  I have merged `getOp` into `subexpression` rather than have single use functions.

    -- An expression is an add operation involving one or more terms
    expression :: String -> String
    expression x = term x
    
    subexpression :: String -> String
    subexpression [] = ""
    subexpression (x:ys)
        | x == '+'  = pushEax ++ term ys ++ add
        | x == '-'  = pushEax ++ term ys ++ sub
        | otherwise = error (expected "AddOp")  

In `subexpression` we are using `term` to define the second parameter to the operation.  And a `term` is defined as any multiplication of factors.

    -- A term is a multiplication operation involving 1 or more factors
    term :: String -> String
    term (x:xs) = (factor x) ++ (subterm xs)
    
    subterm :: String -> String  
    subterm [] = ""
    subterm (x:y:zs) 
        | x == '*'  = pushEax ++ factor y ++ mul ++ subterm zs
        | x == '/'  = pushEax ++ factor y ++ divide ++ subterm zs
        | x == '+' || x == '-' = subexpression (x:y:zs)
        | otherwise = error (expected "MulOp")

And that's it.  If you test this out in `ghci` you can see that each digit is pushed onto the stack, multiplications are performed immediately and additions are left until after, and thus the laws of math are obeyed.

[Download cradle.precedence.hs]()

## Doing it with Types

Up until now, I have been doing a near direct translation of Crenshaws' tutorial to Haskell.   After reading Andersons' paper _Parsing with Haskell_ [1] I realised that Haskell has a very powerful Type system for a reason.

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



