#Let's build a compiler (in Haskell): Part 3 - Addition & Subtaction

## Binary addition and subtraction

Now that we can successfully detect single digits, the next challenge is to extend this to simple addition and subtraction with only two terms. i.e. 
  
    <expression> ::= <term>+/-<term>

A term in our compiler, at least at this stage, is just a single digit, so we can rename the existing `expression` to `term`. Next we define the add and minus operations. As you can see, they are very simple instructions.

    add = emitLn "ADD eax, ebx"
    sub = emitLn "SUB eax, ebx" ++ emitLn "NEG eax"

Crenshaws notes that an extra operation to negate the result of subtraction is needed because the operands are in the wrong order. 

Then we define a function for determining which operation to perform. Again this is quite simple, and can be quite obviously extended to further operations such as multiplication and division.

    getOp :: Char -> String
    getOp x
      | x == '+'  = add
      | x == '-'  = sub
      | otherwise = error (expected "Addop")

Now that we have the necessary functions, we need to figure out the assembly isntructions to add or subtract two numbers together.  The basic assembler instructions are:

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

That is enough now, to 'compile' any single digit binary addition or subtraction expression. You can see that it does not handle algebra or more than two terms in the expression.

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

[Download cradle.binary.hs](http://github.com/alephnullplex/cradle/blob/master/part3/cradle.binary.hs)

## Extending binary addition to n-ary

The general expression for n-ary addition and subtraction is:

    <expression> ::= <term>[<addop><term>]*

Crenshaw implements this as a loop, but Haskell is more suited to doing this with recursion.  From the above definition you can see that the last section is repeated as often as needed so this becomes:

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

[Download cradle.n-ary.hs](http://github.com/alephnullplex/cradle/blob/master/part3/cradle.n-ary.hs)

## Using the stack

So far we have been directly using the two registers `eax` and `ebx` under the assumption that we can calculate the `addop` as soon as it comes up.  This works in the general addition and subtraction case, but when we move onto multiplication and division a problem arises.  Operateor precedence.  

Whoever decided that multiplication is more important than addition, and that this should be true even when you don't use parentheses, was a madman. Anyway, even without operator precedence, the current technique would not be able to handle expressions such as `5-(1+2)`. 

To handle this, we use a stack.  We can store an arbitraty number of items on the stack in a last-in first-out manner.  It is a relatively minor change to the existing code to use the stack. Instead of moving `eax` into `ebx` we `PUSH` it onto the stack.  Then `add` and `sub` simply `POP` the top term off before doing their calculations.

`add` and `sub` become:

    add = emitLn "POP ebx" ++ emitLn "ADD eax, ebx"
    sub = emitLn "POP ebx" ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"

And `subexpression` changes to:

    subexpression :: String -> String
    subexpression [] = ""
    subexpression (x:y:zs) = mov ++ b ++ op ++ (subexpression zs)
      where mov = emitLn "PUSH eax"
            b = term y
            op = getOp x

Now that we have our generated asm using a stack it will make it much easier to implement operator precedence and parentheses.
