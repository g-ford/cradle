#LBAC - Part 10 - Basic Control Structures

It's time to move on from basic mathmatical expressions and start looking at control structures.  

## The Program

First of all we will create some 'holding' functions. Let's define these first.

    <program>::=<block> END
    <block>::=[<statement>]*
    
What this says is that a `program` is a `block` followed by the `end` keyword, and that a `block` is any number of `statemtents`.

As usual this requires a type, a parser and an emitter.  

### Parsing a Program

The types are very simple.

    data Program = Program Block deriving (Show)
    type Block = [Statement]
    data Statement = Statement String deriving (Show)

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
    
## If-Else

Now that we have the basic concepts of creating branches, extending it to the if-else statement not so hard. Extending the definition of if to include the else statment looks like:

    IF <condition> <block> [ ELSE <block>] ENDIF

Before we start, we need to modify `other` to ignore the `else` keyword.  I chose to use a keyword list to ignore rather than test for individual keywords. 
    
    keywords = ["if", "else", "end"]

    other :: Parser Statement
    other = (token letters') <=> (\x -> not $ any (==x) keywords) >>> Statement
    
### Parsing If-Else

You could treat `else` as an optional part of `ifthen` but I chose to seperate them into seperate type constructors and functions. 

    data Statement = Statement String 
                  | Branch Condition Block
                  | Branch2 Condition Block Block
                  deriving (Show)
                  
    statement :: Parser Statement
    statement =  ifelse <|> ifthen <|> other
                  
    ifelse :: Parser Statement		
    ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> br
        where br ((c, b1), b2) = Branch2 c b1 b2
    
As you can see, it is much the same as the basic if statement. The only difference is that we add a second `Block` to the type constructor to allow for the else body.  Again, recusrion allows for nested if statements in nested if-else statements all the way down to turtles.

### Emitting If-Else

The asm for and if-else is not much more complicated than the if version, but it does entail another jump.

    <condition> ;calculate the condition
    jne else    ;goto 'else' if the condition is NOT equal
    <block>     ;the block to execute if the condition is true
    jmp end     ;an unconditional jump to skip the else block
    else:
    <block>     ;the block to execute if the condition is false (the else block)
    end: 

This is where we really need to keep track of our label counter.  Because we need two labels for an if-else, one for the jump to the else portion and another for the jump to the end, the threading gets kind of hairy.  I'll just show it to you and let you figure out where it winds it's way through.

    emitStatement s (Branch2 cond b1 b2) = (s', c ++ jmpElse ++ block1 ++ jmpEnd ++ el ++ block2 ++ end)
        where (s1, c)       = emitCondition s cond 
              jmpElse       = emitLn ("jne " ++ lblElse)
              (s4, block1)  = emitBlock' s3 b1
              jmpEnd        = emitLn ("jmp " ++ lblEnd)
              el            = emitLbl lblElse
              (s', block2)  = emitBlock' s4 b2
              end           = emitLbl lblEnd
              (lblElse, s2) = getLbl s1
              (lblEnd, s3)  = getLbl s2
              
As I said, with 4 intermediate counters, the threading does get a bit hairy.  I took a quick look at the [State Monad](http://www.haskell.org/all_about_monads/html/statemonad.html) which looks like a nice way to simplify this issue, but it was a bit too much to introduce just yet.  I might retro-fit the State Monad as a seperate article outside this series.

## While

Third time round we should be able to fly through this. The definition of while is 

    WHILE  <condition> <block> END
    
Which is very similar to the if definition.  The difference is in the generated asm.

### Parsing While

By now we don't really need an explaination for this.

    data Statement = Statement String 
                  | Branch Condition Block
                  | Branch2 Condition Block Block
                  | While Condition Block
                  deriving (Show)
                  
    statement :: Parser Statement
    statement =  while <|> ifelse <|> ifthen <|> other

    while :: Parser Statement
    while = accept "while" <-+> condition <+> block <+-> accept "end" >>> buildWhile
            where buildWhile (c, b) = While c b
        
No surprises there.

### Emitting While

The asm for a while statement is different from a while in that we need a start label to come back to at the end of the loop.  We jump to the end label if the condition is **true** where as in an if we were jump if false.

The desired asm looks like:

    L1: 
    <condition>
    je L2
    <block>
    jmp L1
    L2:
    
Once again, the lable counter threading is tedious, but other than that the function is the same as we've seen before and is dictated by the asm structure.

    emitStatement s (While c b) = (s', start ++ cond ++ jmp ++ block ++ loop ++ end)
        where start = emitLbl startLbl
              (s1, cond) = emitCondition s c
              jmp = emitLn ("je " ++ endLbl)
              (s', block) = emitBlock' s3 b
              loop = emitLn ("jmp " ++ startLbl)
              end = emitLbl endLbl
              (endLbl, s2) = getLbl s1
              (startLbl, s3) = getLbl s2
              
## Conclusions

For those that haven't been following along, you can [get all the code from github](http://github.com/alephnullplex/cradle/blob/master/part10/Lbach.zip). Let's try some complicated samples.

    C:\cradle\code\src>runhaskell Lbach.hs "while a if b c else d end e end f end"
    L1:
            <condition> a
            je L0
            <condition> b
            jne L2
            <block> c
            jmp L3
    L2:
            <block> d
    L3:
            <block> e
            jmp L1
    L0:
            <block> f
            ret
    C:\cradle\code\src>runhaskell Lbach.hs "if a while b c d e end f g else h i if j k end end end"
            <condition> a
            jne L0
    L3:
            <condition> b
            je L2
            <block> c
            <block> d
            <block> e
            jmp L3
    L2:
            <block> f
            <block> g
            jmp L1
    L0:
            <block> h
            <block> i
            <condition> j
            jne L4
            <block> k
    L4:
    L1:
            ret
    
Apparently, with only and if and a while statement, what we have is capable of producing almost any program.  I wouldn't like to test that theory though. In the next article we will be creating a few more control structures such as for loops and a break statement.