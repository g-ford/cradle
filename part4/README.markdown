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

