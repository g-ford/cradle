#LBAC - Part 12 - Introducing the State Monad

In the [last installment]() you may have noticed that there was a lot of state threading in the emitter functions. This involved a lot of careful ordering of `s1`, `s2` etc.  which is quickly becoming a problem. Especially so when you aren't consistent with parameter ordering - `getLabel` took the counter last whilst `emitStatment` was taking the counter as the first parameter.

It is a misnomer that functional programming cannot have state, rather it cannot have side effects, which is a very different ideal.  If you need to maintain state in your application, and a compiler inherently does, then you need to continually pass this state from one function to the next.  Conversely, all your functions that modify the state must all return the entire state.

The State Monad was created for instances exactly like this. It defines an explicit interface for passing state from one function to the next, and provides several functions for executing the functions using this state.  I'm not going to provide yet another State Monad breakdown - for that I can recommend the following: [the official wiki](http://www.haskell.org/haskellwiki/State_Monad), [a great explaination of the mechanics](http://coder.bsimmons.name/blog/2009/10/the-state-monad-a-tutorial-for-the-confused/) and most recently [Learn you a Haskell](http://learnyouahaskell.com/for-a-few-monads-more#state).

## Some things I learned the hard way

### The State Monad and Record Syntax

I really struggled with using the State Monad in a situation where multiple peices of state are used.  As the compiler is going to need many peices of state such as the label counter, break label, symbol table etc., I started looking for info on record syntax and State Monads.  The best I could find was a few paragraphs in [Real World Haskell](http://book.realworldhaskell.org/read/monads.html#x_Wh) which was pretty clear if brief.

### Types for Functions that use the State Monad

Almost all the sample code and tutorials on using the state monad involved no parameters for the functions.  All the types where of `State s a` and almost no examples showed how to pass in extra parameters.  Thankfully Learn you a Haskell had some examples that used `b -> State s a` which lead me to figure out that functions using the State monad can have any type but final type of a function must be `State s a` and that the type of the function used with `runState` et.al. must be `State s a` only.  Took me a while to figure this one out.

### Random number examples

Annoyingly, 9 times out of 10, any example on the State Monad uses the random number example.  This example is next to useless as it neatly ties up the original state in `StdGen` which hides the actual usage of `State`. Please try to come up with something original and worthwhile that explains the whole thing.  Thanks to Learn you a Haskell, which uses a simple stack example, I was able to figure how and when the `State` is called.

## Simple Example

Rather than try to retro fit the State Monad throughout our emitter in this article, which will be complex and tedious, I will use a simplified example.

First thing we do is create a data type to collect all out stateful information, and a new type using `State` and our new data type.

    -- This will hold all of the data in our state
    data EmitStuff = EmitStuff {
        lblCounter :: Int,
        lastLabel :: String 
        } deriving (Show)
        
    -- It is common practice to make your own state type
    type MyStateMonad = State EmitStuff 
        
`lblCounter` will hold the current count for creating numbered labels, whilst `lastLabel` will be the last generated label.  

Next we will create a function that generates a new label.  This will solely use the state to calculate the new label, and then update the state. Do notation makes this much clearer to read.

    -- Generates a new label based on the current label counter
    -- Stores the generated label, and updates the label counter
    newLabel :: MyStateMonad String
    newLabel = do 
        st <- get
        let l = "L" ++ show(lblCounter st)
        put st { lblCounter = lblCounter st + 1, lastLabel = l}
        return l
        
Next we'll create an example that takes an extra parameter.  A function that generates a label with a comment.

    -- Emits a label followed by a comment 
    emitLabelAndComment :: String -> MyStateMonad String
    emitLabelAndComment x = do 
        l <- newLabel
        return (l ++ ":  #" ++ x)
        
Let's take a moment to understand what this one is doing.  You'll recall that the State Monad is a wrapper for the type `s -> (a, s)` which means that the above type signature expands into `String -> s -> (String, s)`.  This means that this function will also take the state as input, which is then passed onto `newLabel` through the State Monads internals. This is a key concept in turning your manual state management code into something that can be used with the state monad. [Mike Vanier explains](http://mvanier.livejournal.com/1901.html) how we can turn the 5 different type of functions using state into one standard format: `s -> (String, s)`. 

The next example is calling multiple functions that use state from one function. 

    -- Simple example showing how we no longer need to manually thread the state when creating labels 
    fakeIfStatement :: MyStateMonad String   
    fakeIfStatement = do 
        falseBranch <- newLabel
        endLabel <- newLabel
        return ("if not true then \njump " ++ falseBranch ++ "\nDo some true stuff\njump " ++ endLabel ++ "\n" ++ falseBranch ++ ": #some stuff to do if false\n" ++ endLabel) 
      
## Executing the State Monad      

Finally we need to tie it all together.  The state monad comes with three utility functions for executing the code.

1. `runState` returns the result and the state as a tuple
2. `evalState` returns the result and discards the state i.e. `(result, _) = runState`
3. `execState` returns the state only and discards the result i.e. `(_, state) = runState`

Each of these functions take a fucntion of type `State s a` and an intial state of type `s`.  So first we will create our initial state.

    initialState = EmitStuff { lblCounter = 0, lastLabel = "" }

We can then use any of the utility functions to execute our code against this state.

    runState fakeIfStatement initialState 
    -- or
    evalState newLabel initialState
    
But this is not very useful as the state is reset everytime we apply it to a new function.  We have already seen the answer in `fakeIfStatement`.  We simply create one function that executes all the other functions for the duration of the state.

    -- Tying it all together 
    emitAll = evalState emit EmitStuff { lblCounter = 0, lastLabel = "" } 
        where emit = do
                  a <- emitLabelAndComment "This is the first label"
                  b <- fakeIfStatement 
                  c <- emitLabelAndComment "This is the last label"
                  return (a ++ "\n" ++  b ++ "\n" ++ c)
                  
Now if you were to execute `putStrLn & emitAll` in `ghci` you will see labels nicely placed and incremented as expected.

You can download and play with all of the above from Github.

## Implementing in LBaCH

