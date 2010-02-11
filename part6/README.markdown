#Let's build a compiler (in Haskell): Part 6 - Variables and Assignements

## Testing the generated code

So far all the code generation has been pretty academic because we haven't been able to actually run the code.  That won't change anytime soon, because I don't want to worry about IO and running other processes in Haskell just yet.  But here is how you can test what we have done so far, before we dive into some more complex stuff.

### Using NASM

Actually getting this to work on OS X and Win32 taught me a lot more about assembly.  I had to actually understand the `div` instruction to get things working, and as such the `divide` function in the compiler had to be changed. 

Now instead of simply pop and div in `divide`, we have to move the second operator into the right register before poping the stack to get them in the right order.  We then have to make sure `edx` is zero or, more precisely, promote the value of eax to 64 bits with the most significant bits in edx and the least significant bits in eax.  It just so happens that with our single digit limitation, the most significant 32 bits will always be 0, so we can just zero out the edx register.

[Download lbahc-calc.hs](http://github.com/alephnullplex/cradle/blob/master/part6/calc_test.asm)

First of all make sure you have [NASM](http://www.nasm.us/) installed.  On Mac I used MacPorts to install 2.07.  I used the [2.07 Win32 installer](http://www.nasm.us/pub/nasm/releasebuilds/2.07/win32/) on windows. 

You will also need [gcc](http://gcc.gnu.org/). For OS X, gcc comes with XCode, and for Windows I used [MinGW](http://www.mingw.org/) 

Download this [template ASM](http://github.com/alephnullplex/cradle/blob/master/part6/calc_test.asm) and add your generated code where indicated.  You can then run the following commands to finish off the compilation and linking.

OS X:

    ~> nasm -f macho calc_test.asm 
    ~> gcc -arch i386 -o calc_test calc_test
    ~> ./calc_test 
    Result: 7

Win:

    ~> nasm -f win32 calc_test.asm 
    ~> gcc -o calc_test calc_test
    ~> ./calc_test 
    Result: 7
   
Have a play with a few different sums and make sure the results are what you expect them to be.  Just remember that we are doing integer division, which means the result will always be rounded down e.g. 3/2 = 1.

## Parsing variables and assignments

We'll deviate from the Crenshaw script a bit and skip parentheses for now.  

So far our compiler can handle expressions with numbers, but thats of limited value.  So lets add some variables.  

We want to be able to parse expressions like:
    
    a+b*c

This is actually quite easy to support in the parse phase with 2 additional lines of code.  First we need to define a type constructor for variables in `Expression` and then add a pattern to `parse` that can detect the varibles. As we will, once again, only be supporting single characters, we will define the type constructor using `Char`.  

    data Expression = Num Int
                    | Add Expression Expression
                    | Sub Expression Expression
                    | Mul Expression Expression
                    | Div Expression Expression
                    | Var Char
                    deriving (Show)

And then we can modify `parse` to detect a single alpha character using `isAlpha` and then create a new `Var` expression.

    parse (x:[])
      | isDigit x = Num (digitToInt x)
      | isAlpha x = Var x

And that's all we have to do to support simple variables in our parser.  Now we can parse statments like `1+a-3*c` and the corresponding `Expression` is `Sub (Add (Num 1) (Var 'a')) (Mul (Num 3) (Var 'c'))`. I don't know about you, but that was easier than I expected.

Assignment, unsurprisingly, is similarly two extra lines of code. Again, a new type constructor and a new pattern for the parser.

Assignment is usually defined as:

    <Assignment>::=<Ident>=<Expression>
    
And as usually we will limit ourselves to one character for identities, so our type constructor will use `Char`:

    data Expression = Num Int
                    | Add Expression Expression
                    | Sub Expression Expression
                    | Mul Expression Expression
                    | Div Expression Expression
                    | Var Char
                    | Assign Char Expression
                    deriving (Show)
              
And the `parse` pattern is also pretty straight forward:

    parse (x:'=':zs) 
        | isAlpha x = Assign x (parse zs)
        | otherwise = error "expected identifier"
    parse (a:b:c:d:ds) ...
	
This allows us to parse not only simple assignments like `a=1` but also ones with complicated right hand sides such as `a=1+2*3` and even ones with other variables in it `a=1+b`. We have to check that we are assigning to an identifier, otherwise statements such as `3=2` would be parsed as legal.

	*Main> parse "a=1"
	Assign 'a' (Num 1)
	*Main> parse "a=1"
	Assign 'a' (Num 1)
	*Main> parse "a=1+2*3"
	Assign 'a' (Add (Num 1) (Mul (Num 2) (Num 3)))
	*Main> parse "a=1+b"
	Assign 'a' (Add (Num 1) (Var 'b'))
    *Main> parse "3=2"
    *** Exception: expected identifier
    
## Emitting variables and assignments

Variables and assignments in x86 is actually a two step process. You need to allocate or reserve some memory for them before they can be used.

For example, a simple program for `c=a+b` where `a=1` and `b=2` would require us to allocate `a` and `b` and reserve space for `c`.  This would look like:

    ; Note: this is not a complete program 
    ; dd and resd are used for dword (32 bit) allocations
    section .data
        a	dd	1   ; a=1 
        b   dd  2   ; b=2

    section .bss
        c   resd    ; reserve dword (32bits) for c

    section .text
        mov eax, [a]    ; put the value of a into eax
        add eax, [b]    ; add eax and the value of b
        mov [c], eax    ; c = eax

As you can see we now have three sections to output to.  Data is for intialised variables, whilst bss is for uninitialized variables, which is all an assignment is.  And text is where the actual operations occur.

To emit the assembly in the text section is pretty straight forward. We simple need to add two new case statements to the `emit` function:

emit expr = case expr of 
     Num a      -> emitLn ("MOV eax, " ++ (show a))
     Add a b    -> emit a ++  pushEax ++ emit b ++ add
     Sub a b    -> emit a ++  pushEax ++ emit b ++ sub
     Mul a b    -> emit a ++  pushEax ++ emit b ++ mul
     Div a b    -> emit a ++  pushEax ++ emit b ++ divide
     Var a      -> emitLn ("MOV eax, [" ++ [a] ++ "]")
     Assign a b -> emit b ++ emitLn ("MOV [" ++ [a] ++ "], eax")
     
You'll notice that the output for variables (`Var a`) and integers (`Num a`) is pactically the same, but instead of loading a literal integer into the register we load the value located at the label.

Assignment simple loads whatever is in the `eax` register, after all the operations have occured, into the label.

## Outputing the sections

To get all the sections to output correctly we need to define a function for each section that takes an `Expression` and produces the relevant lines for that section.

We already have one for the text section, so we'll rename the exiting `emit` to `emitText`.

For the data section, we are only intereted in `Var` expressions.  Unfortunately we have not yet allowed for our variables to have an initial value yet, so we will default everything to zero - this is a sensible default considering we are only dealing with single digit integers.
    
    -- Generates the contents of section .data
    emitData :: Expression -> String
    emitData expr = case expr of
        Var a       -> emitLn ([a] ++ "\tdd\t0")
        Add a b     -> emitData a ++ emitData b
        Sub a b     -> emitData a ++ emitData b
        Mul a b     -> emitData a ++ emitData b
        Div a b     -> emitData a ++ emitData b
        Assign a b  -> emitData b 
        otherwise   -> ""

As you can see the only one that actually creates output is `Var` and the rest just pass through.

The bss section is pretty similar except this time we are only really interested in the first part of `Assign`.

    -- Generates the contents of section .bss
    emitBss ::: Expression -> String
    emitBss expr = case expr of
        Assign a b  -> emitLn ([a] ++ "\tresd") ++ emitBss b
        Add a b     -> emitBss a ++ emitBss b
        Sub a b     -> emitBss a ++ emitBss b
        Mul a b     -> emitBss a ++ emitBss b
        Div a b     -> emitBss a ++ emitBss b
        otherwise   -> ""

And then we can tie it all together with a new `emit` function.  This will create the section labels and then hand off to the relevant emit function for that section.

    -- Turns an expression into the equvilent assembly
    emit :: Expression -> String
    emit expr= "section .data\n" ++ emitData expr 
                ++ "section .bss\n" ++ emitBss expr 
                ++ "section .text\n" ++emitText expr

And a play in `ghci` shows that we can now generate the three sections for both general expressions and assignments.

    *Main> putStr $ parseAndEmit "a=1+b"
    section .data
            b       dd      0
    section .bss
            a       resd
    section .text
            MOV eax, 1
            PUSH eax
            MOV eax, [b]
            POP ebx
            ADD eax, ebx
            MOV [a], eax
    *Main> putStr $ parseAndEmit "1+2*3+a"
    section .data
            a       dd      0
    section .bss
    section .text
            MOV eax, 1
            PUSH eax
            MOV eax, 2
            PUSH eax
            MOV eax, 3
            POP ebx
            MUL ebx
            PUSH eax
            MOV eax, [a]
            POP ebx
            ADD eax, ebx
            POP ebx
            ADD eax, ebx
            
I should note that this output, as it stands, wont actually assemble as we haven't defined an entry point or taken care of setting up and taking down the stack frame.  But you can take the output for each section and plug it into the relevant parts in `calc_test.asm`.  

[Download lbach-assign.hs'](http://github.com/alephnullplex/cradle/blob/master/part6/lbach-assign.hs)