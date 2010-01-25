import Char

-- Throw an expected error
expected s = s ++ " expected"

-- Tests two chars match
matchChar :: Char -> Char -> Bool 
matchChar x y = x == y

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

-- A single digit
factor x = emitLn ("MOV eax, " ++ [num])
    where num = getNum x

-- Basic math functions
popEbx = emitLn "POP ebx"
pushEax = emitLn "PUSH eax"
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
mul = popEbx ++ emitLn "MUL ebx"
divide = popEbx ++ emitLn "DIV ebx" 

term :: String -> String
term (x:xs) = a ++ (subterm xs)
  where   a = factor x 
          subterm [] = ""
          subterm (x:y:zs) 
            | x == '*'  = pushEax ++ factor y ++ mul ++ (subterm zs)
            | x == '/'  = pushEax ++ factor y ++ divide ++ (subterm zs)
            | otherwise = error (expected "MulOp")
