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

With these parsers you can write really cool programs like `something end` or `two statements end`.  Don't worry, it gets more exciting.  You might also want to update your `parse` function to use `program` instead of `assign`.

### Emitting a Program

Our emitter must now take a `Program`, and iterate over the `Statements` in it's `Block`. For now we will be outputting a place holder `<block>` for our other statements.

    type State = Int
    
    emit :: Program -> String
    emit (Program b) = emitBlock b ++ emitLn "ret"
    
    emitBlock :: Block -> String
    emitBlock b = result
        where (s, result) = emitBlock' 0 b

    emitBlock' :: State -> [Statement] -> (State, String)
    emitBlock' s [] = (s, "")
    emitBlock' s (b:bs) = (s', first ++ rest)
        where (s1, first) = emitStatement s b
              (s', rest) = emitBlock' s1 bs
    
    emitStatement s (Statement b) = (s, emitLn ("<block> " ++ b))

In case you are wondering why all `s` and `State`, this is how we will track the label counter, and other stateful items like the symbol table in the future. I originally had `emitBlock` using `fold'` and `map` in one line, but it caused complications when it came time to thread the label counter through the emit functions.  

So now we can parse and emit multi-statement programs.  Those statements might not do anything yet, but we'll get there. 

## If-Then Statements

The first control structure we will look at is the basic if statement.  This takes a condition, and if the condition is true, it will execute the body of the if statment.

The hardest part of this section is deciding what our syntax will be.  To keep things simple, we will stick to Crenshaws recommended syntax of 

    IF <condition> <block> ENDIF
    
By having an explicit block terminator (ENDIF) we avoid the ambiguity of 'is this still part of the if body'.  
    
### Parsing If-Then

We create a new type constructor, which I called `Branch`, which will hold the condition and the body of the if statement, which is really just another block. The parser is very similar to the syntax definition:

    data Statement = Statement String 
                   | Branch Condition Block
                   
    ifthen :: Parser Statement		
    ifthen = accept "if" <-+> condition <+> block <+-> accept "end" >>> buildBranch
        where buildBranch (c, b) = Branch c b
        
Because we use `block` as part of the function, we can parse multi-statement bodies, and even nested if statments such as:

    *Lbach.Parser> ifthen "if cond partA partB end"
    Just (Branch (Condition "cond") [Statement "partA",Statement "partB"],"")
    *Lbach.Parser> ifthen "if condA if condB bodyB end bodyA end"
    Just (Branch (Condition "condA") [Branch (Condition "condB") [Statement "bodyB"],Statement "bodyA"],"")

Gotta love the power of recursion.
    
Now to be able to use our new `ifthen` as part of a `program` we need to update `statement` to include it, using our good friend the alternate combinator `<|>`:

    statement :: Parser Statement
    statement =  ifthen <|> other

### Emitting If-Then

An if statement needs to be translated into the following assembly:

    <condition> ;calculate the condition
    jne end     ;goto 'end' if the condition is NOT equal
    <block>     
    end: 

We need to use a `jne`, which means jump if **not** equal, because we want to skip the body if the condition fails. We will continue to output the placeholder `<block>`.  We will also use a placeholder for `<condition>` as we haven't covered relational statements yet.  
   
When generating asm we can't call all our labels `end` so we have to create a function to generate a new label and we'll also create one to emit the label.  For labels we will use `L1`, `L2` etc, and to keep things nice we need to return the next label count.

    getLbl :: Int -> (String, Int)
    getLbl count = ("L" ++ (show count), count + 1)

    emitLbl :: String -> String
    emitLbl lbl = lbl ++ ":\n"
    
Unfortunately, the need to thread the label counter through the emitters complicates them a little, but they are reasonably simple.  We have to be careful to pass the `State` around in the right order so that our labels come out correct.

    emitStatement :: State -> Statement -> (State, String)
    emitStatement s (Branch cond b1) = (s', c ++ jmp ++ block1 ++ end)
        where (s1, c)       = emitCondition s cond 
              jmp           = emitLn ("jne " ++ lbl)
              (s', block1)  = emitBlock' s2 b1
              end           = emitLbl lbl
              (lbl, s2)     = getLbl s1
    emitStatement s (Statement b) = (s, emitLn ("<block> " ++ b))
        
    emitCondition s (Condition c) = (s, emitLn ("<condition> " ++ c))
    
The only tricky part is following the label counter through.  We first send the label counter through the condition in case it needs to use labels.  The counter is then used to get a new label for use in the jump and outputting the label to jump to. It then gets passed onto the body `block` which may use labels and therefore increment the counter.  This final number is then returned to the caller.  To see why we pass it in this order try parsing and emitting a nested if.

    *Main> let k = emit . parse
    *Main> putStr $ k "if condA if condB bodyB end bodyA end end"
            <condition> condA
            jne L0
            <condition> condB
            jne L1
            <block> bodyB
    L1:
            <block> bodyA
    L0:
            ret
    *Main>