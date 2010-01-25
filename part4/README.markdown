#Let's build a compiler (in Haskell): Part 4 - Operator Precedence & Parentheses

## Operator Precedence

Introducing multiplication and division into out compiler means that we have to think about operator precedence if we want our language implementation to adhere to the general conventions of math.

As it turns out, there is a rather simple trick to this - define `term` in terms of multiplication.

    <term> := <factor>[<mulop><factor>]*

Now we call a single digit a `factor`, and a `term` is any result of a multiplication operation involving these factors.  What this effectively means is that all multiplication has happened before we get to addition, and thus operator precedence is preserved.

We will need some additional functions to detect and emit multiplication and division, which should look very familiar. I've also taken the opportunity to remove a bit of refactoring. 

    -- Processes a * or / in a general math expression
    -- Throws an error if any char other than [*/] 
    getMulOp :: Char -> String
    getMulOp x
      | x == '*'  = mul
      | x == '-'  = divide
      | otherwise = error (expected "MulOp")

    -- Basic math functions
    popEbx = emitLn "POP ebx"
    add = popEbx ++ emitLn "ADD eax, ebx"
    sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
    mul = popEbx ++ emitLn "MUL ebx"
    divide = popEbx ++ emitLn "DIV ebx" 

Now that our single digits are factors and not terms, we need to rename `term` to `factor` and create a new `term` based on multiplication.  

