#Let's build a compiler (in Haskell): Part 2 - The Cradle

Crenshaw starts off with a simple collection of utility functions: error handler; a routine to read from standard in; some simple matching functions for digits and letters; and some routines to emit assembly.  

There is an assumption here that you will start off using the command line to test your compiler.  However GHC has an interactive mode that I will be using.  This makes it easier to experiment and also means we don't really need to worry about IO just yet.

## The Cradle

This is the basic framework of utility functions that  Crenshaw calls the cradle.  Save this as `cradle.hs`.

    import Char

    -- Throw an expected error
    expected s = error (s ++ " expected")

    -- Tests two chars match
    matchChar :: Char -> Char -> Bool 
    matchChar x y = x == y

    -- Gets and identifier.
    -- Throws an expected error if it is not an alpha
    getName :: Char -> Either String Char
    getName x 
      | isAlpha x = Right x
      | otherwise = Left (expected "Name")

    -- Checks if the char is a digit.
    -- Throws an expected error if it is not a digit
    getNum :: Char -> Either String Char
    getNum x 
      | isDigit x = Right x
      | otherwise = Left (expected "Integer")

    -- Prefix a string with a tab
    emit s = "\t" ++ s

    -- Prefix a string with a tab and postfix it with a new line
    emitLn s = (emit s) ++ "\n"

As you can see this is a little bit simpler than the Turbo Pascal version.  We get `error` for free.  We don't need `abort` because we are starting with `ghci`.  `isAlpha` and `isDigit` come free with the `Char` module. 

## A note on error handling in Haskell

The hardest part of setting up the cradle was figuring out how to handle error conditions.  Apparently there is [8 different ways to report errors](http://www.randomhacks.net/articles/2007/03/10/haskell-8-ways-to-report-errors) most of which are based on Monads which I am hesitant to introduce at this stage - mostly because I have not actually groked them yet.

When I just used the basic `error` function it didn't really seem to be what I wanted as I was getting output like `"\tMOVE #*** Exception: Integer expected` which was clearly wrong.

For now I have settled on Eric Kidd's third method - using `Either String a`.  At some point in the future I intend on moving to the fourth method due to Kidd's plea.

> If you're writing new Haskell libraries for public consumption, and all your errors are strings, please consider using this error-reporting method.

## Single Digits

So let's use the cradle so far to create a simple single digit 'translator'.  Just about every programming language needs to support simple math primitives so it's a good place to start.  This will let us get some results without having to worry about what or how the source language will look like.

Put this at the end of `cradle.hs`.

    expression x = 
      case getNum x of
        Left msg  -> putStrLn msg
        Right num -> putStrLn $ emitLn ("MOVE #" ++ [num] ++ ",D0")

And then go ahead and test it.

    Prelude> :l cradle
    [1 of 1] Compiling Main             ( cradle.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> expression 'a'
    *** Exception: Integer expected
    *Main> expression '1'
        MOVE #1,D0


As you can see, it successfully detects whether the expression is an integer or not. If it is an integer, it emits some asm which I assume puts the number on the stack (I've yet to learn asm as well). Not very exciting, but it's a start. 

