#Let's build a compiler (in Haskell): Part 3 - Basic Expressions

## Binary addition and subtraction

Now that we can successfully detect single digits, we will extend that to simple addition and subtraction with only two terms. i.e. 
  
  <expression> ::= <term>+/-<term>

A term in our compiler, at this stage, is just a single digit, so we can rename the existing `expression` to `term`.

Next we define the add and minus operations. As you can see, they are very simple instructions.

    add = emitLn "ADD eax, ebx"
    sub = emitLn "SUB eax, ebx" ++ emitLn "NEG eax"

As Crenshaws notes, an extra operation to negate the result of subtraction is needed because the aperands are in the wrong order. 

Then we define a function for determining which operation to perform. Again this is quite simple, and can be quite obviously extended to multiplication and division.

    getOp :: Char -> String
    getOp x
      | x == '+'  = add
      | x == '-'  = sub
      | otherwise = error (expected "Addop")

Now that we have the necessary functions, we need to figure out how to add or subtract two numbers together.  The basic assembler instructions are:

 - Put the first term into eax
 - Move the value of eax to ebx
 - Put the second term into eax
 - Add or subtract ebx from eax
 
This turns three chacters into four instructions.  We will start by defining `expression` to match 3 and only 3 characters.

    expression (x:y:z:[]) = a ++ mov ++ b ++ op
        where   a = term x 
                mov = emitLn "MOV ebx, eax"
                b = term z
                op = getOp y

That is enough now, to 'compile' any single digit, binary addition or subtraction expression. You can see that it does not handle algebra or more than two terms in the expression.

    Prelude> :l cradle.hs
    [1 of 1] Compiling Main             ( cradle.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> putStr $ expression "3-1"
            MOV eax, 3
            MOV ebx, eax
            MOV eax, 1
            SUB eax, ebx
            NEG eax
    *Main> putStr $ expression "3+1"
            MOV eax, 3
            MOV ebx, eax
            MOV eax, 1
            ADD eax, ebx
    *Main> putStr $ expression "3+a"
            MOV eax, 3
            MOV ebx, eax
            MOV eax, *** Exception: Integer expected
    *Main> putStr $ expression "1+2+3"
    *** Exception: cradle.hs:(42,0)-(46,23): Non-exhaustive patterns in function expression

## Extending binary addition to n-ary

The general expression for n-ary addition and subtraction is:

    <expression> ::= <term>[<addop><term>]*

Crenshaw implements this as a loop, but we will use recursion instead.  From the above definition you can see that the last section is repeated as needed so this becomes:

    expression :: String -> String
    expression (x:xs) = a ++ (subexpression xs)
      where   a = term x 

    subexpression :: String -> String
    subexpression [] = ""
    subexpression (x:y:zs) = mov ++ b ++ op ++ (subexpression zs)
      where mov = emitLn "MOV ebx, eax"
            b = term y
            op = getOp x

So now we can process arbitrary length addition and subtraction and even a combination of the two.