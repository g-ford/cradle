#Let's build a compiler (in Haskell): Part 3 - Basic Expressions

## Binary addition and subtraction

Now that we can successfully detect single digits, we will extend that to simple addition and subtraction with only two terms. i.e. `<term>+/-<term>`

A term in our compiler, at this stage, is just a single digit, so we can rename the existing `expression` to `term`.

Next we define the add and minus operations. As you can see, they are very simple instructions.

    add = emitLn "ADD eax, ebx"
    sub = emitLn "SUB eax, ebx"

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

That is enough now, to 'compile' any single digit, binary addition or subtract term. 

    Prelude> :l cradle.hs
    [1 of 1] Compiling Main             ( cradle.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> putStr $ expression "3-1"
            MOV eax, 3
            MOV ebx, eax
            MOV eax, 1
            SUB eax, ebx
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

