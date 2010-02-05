We'll deviate from the Crenshaw script a bit and skip parenthese for now.  

## Adding variables

So far our compiler can handle expressions with numbers, but thats of limited value.  So lets add some variables.  

We want to be able to parse and compile expressions like:
    
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


