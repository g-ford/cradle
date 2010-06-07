#LBAC - Part 11 - Loops

There are a few kinds of loops in your average imperitive langauage.  There are pre and post-condition loops, loops which you can break out of the middle and then there a loops that iterate over a collection.

We'll progress through these in increasing difficulty. Starting with an infinite loop, through to your standard `for` loop.

## Infinite Loops

An infinite loop by itself is pretty useless, but combined with a `break` statement it can be pretty handy for when you don't really know how long you need to be going through your loop. I'm sure you've all seen game loops and the like that essentially loop while true.

By know you should know that we need a type constructor, a recogniser and an emitter.  The constructor and recogniser are trivially added as a `Statement`, so we'll focus on the emitter.

The assembly to create an infinite loop is to essentially an unconditional jump back to the start of the loop.  This means we will require a label and a simple `jmp` after our block.

    emitStatement s (Loop b) = (s', start ++ block ++ jmp) 
        where start          = emitLbl startLbl
              (s', block)    = emitBlock' s1 b
              jmp 		  	 = emitLn ("jmp " ++ startLbl)
              (startLbl, s1) = getLbl s
          
## Post Conditonal Loop

We already have a preconditional loop with `while`, so now we will add it's cousin the `do...until`. We use the keyword `until` to avoid some issues with the `while` keyword. This loop is useful where you would like something to happen at least once.

On the face of it, this is just `while` with the `condition` and `block` reversed. That's exactly how we will treat it. The recogniser is the same as `while` with minor order adjustments, as is the emitter.

    dountil :: Parser Statement
    dountil = accept "do" <-+> block <+-> accept "until" <+> condition +>> DoUntil
    
    emitStatement s (DoUntil b c) = (s', start ++ block ++ cond ++ jmp ++ loop ++ end) 
        where start 	  	 = emitLbl startLbl
              (s1, cond)  	 = emitCondition s c
              jmp 		  	 = emitLn ("je " ++ endLbl)
              (s', block) 	 = emitBlock' s3 b
              loop 		  	 = emitLn ("jmp " ++ startLbl)
              end 		  	 = emitLbl endLbl
              (endLbl, s2) 	 = getLbl s1
              (startLbl, s3) = getLbl s2