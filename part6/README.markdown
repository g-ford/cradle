## Testing the generated code

So far all the code generation has been pretty academic because we haven't been able to actually run the code.  That won't change anytime soon, because I don't want to worry about IO and running other processes in Haskell just yet.  But here is how you can test what we have done so far, before we dive into some more complex stuff.

### Using NASM

First of all make sure you have [NASM](http://www.nasm.us/) installed.  On Mac I used MacPorts to install 2.07.  I used the [2.07 Win32 installer](http://www.nasm.us/pub/nasm/releasebuilds/2.07/win32/) on windows.

Download this [template ASM] and add your generated code where indicated.  You can the run the following commands to finish off the compilation and linking.

OS X:

    ~/Projects/asm> nasm -f macho calc_test.asm 
    ~/Projects/asm> gcc -arch i386 -o calc_test calc_test
    ~/Projects/asm> ./calc_test 
    Result: 7

Win:


## Adding variables

We'll deviate from the Crenshaw script a bit and skip parenthese for now.  

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


