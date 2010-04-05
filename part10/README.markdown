#LBAC - Part 10 - Control Structures

It's time to move on from basic mathmatical expressions and start looking at control structures.  

## The Program

First of all we will create some 'holding' functions. Let's define these first.

    <program>::=<block> END
    <block>::=[<statement>]*
    
What this says is that a `program` is a `block` followed by the `end` keyword, and that a `block` is any number of `statemtents`.

As usual this requires a type, a parser.  The types are very simple.

    data Program = Program Block deriving (Show)
    data Block = Block String deriving (Show)

### Parsing a Program

Because multi-character keywords are pretty simple using our techniques, we will skip the single character keyword versions. I will be using a function called `accept` which is a simple parser that reads the `letters` and then compares it to the given keyword.  I've also created a `letters'` that fails on an empty string. I'll leave the definition of these as an exercise.

    program :: Parser Program
    program = block <+-> accept "end" >>> Program 
    
    block :: Parser Block
    block =  block = iter statement
    
    statement :: Parser Statement
    statement =  other
    
    -- | Accepts any string except 'end'
    other :: Parser Block
    other = (token letters') <=> (/="end") >>> Statement

With these parsers you can write really cool programs like `something end` or `two statements end`.  Don't worry, it gets more exciting.

### Emitting a Program

Our emitter must now take a `Program`, and iterate over the `Statements` in it's `Block`. For now we will be outputting a place holder `<block>` for our other statements.

    emit :: Program -> String
    emit (Program b) = emitBlock b ++ emitLn "ret"
    
    emitBlock :: Block -> String
    emitBlock b = foldl' (++) [] (map emitStatement b)
    
    emitStatement (Statement b) = emitLn ("<block> " ++ b)

If you are not fully fluent in Haskell, `emitBlock` might be a bit daunting.  It basically iterates over each `Statement` in the `Block` (using `map`) and then folds the list into a single string.

So now we can parse and emit multi-statement programs.  Those statements might not do anything yet, but we'll get there. 

## If Then

The first control structure we will look at is the basic if statement.  This takes a condition, and if the condition is true, it will execute the body of the if statment.

To keep things simple, we will stick to Crenshaws recommended syntax of 

    IF <condition> <block> ENDIF
    
By having an explicit block terminator (ENDIF) we avoid the ambiguity of 'is this still part of the if body'.  This needs to be translated into the assembly:

    <condition> ;calculate the condition
    jne end     ;goto 'end' if the condition is NOT equal
    <block>     
    end: 
    
       