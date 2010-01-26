import Char

-- Throw an expected error
expected s = s ++ " expected"

-- Gets and identifier.
-- Throws an expected error if it is not an alpha
getName :: Char -> Char
getName x 
  | isAlpha x = x
  | otherwise = error (expected "Name")

-- Checks if the char is a digit.
-- Throws an expected error if it is not a digit
getNum :: Char -> Char
getNum x 
  | isDigit x = x
  | otherwise = error (expected "Integer")

-- Prefix a string with a tab
emit s = "\t" ++ s

-- Prefix a string with a tab and postfix it with a new line
emitLn s = (emit s) ++ "\n"

-- Basic math functions
popEbx = emitLn "POP ebx"
pushEax = emitLn "PUSH eax"
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
mul = popEbx ++ emitLn "MUL ebx"
divide = popEbx ++ emitLn "DIV ebx" 

-- A single digit
factor :: Char -> String
factor x = emitLn ("MOV eax, " ++ [num])
    where num = getNum x

-- A term is a multiplication operation involving 1 or more factors
term :: String -> String
term (x:xs) = (factor x) ++ (subterm xs) 

subterm :: String -> String  
subterm [] = ""
subterm (x:y:zs) 
    | x == '*'  = pushEax ++ factor y ++ mul ++ subterm zs
    | x == '/'  = pushEax ++ factor y ++ divide ++ subterm zs
    | x == '+' || x == '-' = subexpression (x:y:zs)
    | otherwise = error (expected "MulOp")

-- An expression is an add operation involving one or more terms
expression :: String -> String
expression x = term x

subexpression :: String -> String
subexpression [] = ""
subexpression (x:ys)
    | x == '+'  = pushEax ++ term ys ++ add
    | x == '-'  = pushEax ++ term ys ++ sub
    | otherwise = error (expected "AddOp")
