#LBAC - Part 12 - Loops

There are a few kinds of loops in your average imperitive langauage.  There are pre and post-condition loops, loops which you can break out of the middle and then there a loops that iterate over a collection.

We'll progress through these in increasing difficulty. Starting with an infinite loop, through to your standard `for` loop.

## The Grammar

First of all we will define the grammar for all out types of loops.

    //Infinite loop
    LOOP        { L = getLbl;
                  emitLbl L }
    <block>
    END     { Emit(jmp L }
    
    //Post coditional
    DO          { L = getLbl
                  emitLbl L }
    <block>
    UNTIL
    <condition> { Emit(JNE L) }
    
    //For Loop
    FOR <ident> = <expr1> TO <expr2> <block> END

These translate into the follow data type

    data Statement = Statement String 
                  | Branch Condition Block
                  | Branch2 Condition Block Block
                  | While Condition Block
                  | Loop Block
                  | DoUntil Block Condition
                  | For Expression Expression Block
                  deriving (Show)

## Infinite Loops

An infinite loop by itself is pretty useless, but combined with a `break` statement it can be pretty handy for when you don't really know how long you need to be going through your loop. A typical example of an infinite loop is a game loop or an event loop, that essentially loops while true until a particular state, event or message terminates the program.

The paser is very simple. We just drop the keywords and pass the block onto the `Loop` data constructor.

    -- |Parses loop..end statments
    loop :: Parser Statement
    loop = accept "loop" <-+> block <+-> accept "end" >>> Loop

The assembly to create an infinite loop is just an unconditional jump back to the start of the loop.  This means we will require a label at the start of the block and a simple `jmp` after our block.

    emitStatement (Loop b) = do
        startLbl <- getLbl
        block <- emitBlock' b
        let jmp = emitLn ("je " ++ startLbl)
        return ((emitLbl startLbl) ++ block ++ jmp)
          
## Post Conditonal Loop

We already have a preconditional loop with `while`, so now we will add it's cousin the `do...until`. We use the keyword `until` to avoid some issues with the `while` keyword. This loop is useful where you would like something to happen at least once.

On the face of it, this is just `while` with the `condition` and `block` reversed. That's exactly how we will treat it. The recogniser is the same as `while` with minor order adjustments, as is the emitter.

    -- |Parses do..until statments
    dountil :: Parser Statement
    dountil = accept "do" <-+> block <+-> accept "until" <+> condition +>> DoUntil
    
    emitStatement (DoUntil b cond) = do
        startLbl <- getLbl
        c <- emitCondition cond
        block <- emitBlock' b
        let jmp = emitLn ("je " ++ startLbl)
        return ((emitLbl startLbl) ++ block ++ c ++ jmp)
        
## For loop

The for loop construct we are going to build here is not the c-style one with a condition and increment function. We are going to use a simpler Basic style construct that is typically used like:

    FOR x=1 TO 10
        <block>

There are a few assumptions with this construct.  It is assumed that the 'counter' is an integer that incements by one on each pass through the loop.

You may have noticed in the first section that I (intentionally) did not add any psuedo-syntax to the for loop syntax.  If we were to try to directly translate a for loop into assembly it would be a bit tiresome and quite ugly.  Instead, we will translate the source into an alternative construct.
    
    // alternative for loop construct
    <ident> = <expr1>
    TEMP = <expr2>
    WHILE <ident> <= TEMP
    <block>
    <ident> = <ident> + 1
    END
    
So we will parse the construct, but rather than emit the assembly directly, we will actually just 're-write' the AST to this new construct, which we can then just emit with our existing code.  

Aside: The 're-writeing' of the AST is quite common in programming languages and is often called 'syntactical sugar'.  In a 'real' compiler, there are often several optimisation steps that re-write the AST that sits between the parser and emitter.  Our 'toy' compiler omits these optimisation passes.

To do this we will need to remediate our expression parser and emitter so that they work in the larger context of our program parser.

We'll bring the `Assign` type into be a data constructor for `Statement`, add the assignment option to `statement` add a new pattern to `emitStatment`, and (for now) ignore the sections and just emit the text section.

    -- Lbach.Grammar.Basics
    data Statement = Statement String 
                  | Branch Condition Block
                  | Branch2 Condition Block Block
                  | While Condition Block
                  | Loop Block
                  | DoUntil Block Condition
                  | Assign Assignment
                  deriving (Show)
                  
    data Assignment = Assignment String Expression 
                    deriving (Show)

    -- Lbach.Parser.Control
    statement = loop 
                <|> dountil
                <|> while 
                <|> ifelse 
                <|> ifthen 
                <|> assign
                <|> other
                
    -- Lbach.Parser.Expressions
    assign = token letters <+-> token (literal '=') <+> expr +>> Assign

    --Lbach.Emitter.Control
    emitStatement (Assign s expr) = do
        return $ emitText expr
    
You'll also need to remove `emit2` from `Lbach.Emitter` and update some patterns in `Lbach.Emitter.Expressions` to get it compile.

Now that's out of the way, we can get down to business on the for loop parser.  Let's revisit the grammar.

FOR <ident> = <expr1> TO <expr2> <block> END
or
FOR <assign> TO <expr> <block> END

This leads to a very natual data type and parser.

    -- Add this to Statement
        | For Statement Expression Block 

    -- |Parse a for loop
    forloop :: Parser Statement		
    forloop = accept "for" <-+> assign <+-> accept "to" <+> expr <+> block <+-> accept "end" >>> br
        where br ((a, e), b) = For a e b

As you can see I chose not to transform the AST at the parser level. Therefore we have to do it at the emitter level.  This is surprisingly quite straight forward.

    emitStatement (For (Assign (Assignment s e1)) e2 b) = do
        let var1 = s
        let var2 = "temp"
        line1 <- emitStatement $ Assign (Assignment s e1)
        line2 <- emitStatement $ Assign (Assignment var2 e2)
        rest <- emitStatement (While (Condition (var1 ++ "<=" ++ var2)) b)
        return $ line1 ++ line2 ++ rest
        
## Testing some samples

*Main> p "loop block end end"
L0:
        <block> block
        jmp L0
        ret

*Main> p "do block until condition end end"
L0:
        <block> block
        <condition> condition
        je L0
        ret

*Main> p "for a=1 to 2 block end end"
        MOV eax, 1
        MOV [a], eax
        MOV eax, 2
        MOV [temp], eax
L0:
        <condition> a<=temp
        je L1
        <block> block
        jmp L0
L1:
        ret
*Main> p "for a=1 to 1000 if a b end d = 3+2*c end end"
        MOV eax, 1
        MOV [a], eax
        MOV eax, 1000
        MOV [temp], eax
L0:
        <condition> a<=temp
        je L1
        <condition> a
        jne L2
        <block> b
L2:
        MOV eax, 3
        PUSH eax
        MOV eax, 2
        PUSH eax
        MOV eax, [c]
        POP ebx
        MUL ebx
        POP ebx
        ADD eax, ebx
        MOV [d], eax
        jmp L0
L1:
        ret

*Main>