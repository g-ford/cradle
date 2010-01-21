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

term x = emitLn ("MOV eax, " ++ [num])
    where num = getNum x

getOp :: Char -> String
getOp x
  | x == '+'  = add
  | x == '-'  = sub
  | otherwise = error (expected "Addop")

add = emitLn "ADD eax, ebx"
sub = emitLn "SUB eax, ebx" ++ emitLn "NEG eax"

expression :: String -> String
expression (x:y:z:[]) = a ++ mov ++ b ++ op
  where a = term x
        mov = emitLn "MOV ebx, eax"
        b = term z
        op = getOp y
            
 