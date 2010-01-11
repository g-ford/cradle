import Char

-- Throw an expected error
expected s = error (s ++ " expected")

-- Tests two chars match
matchChar :: Char -> Char -> Bool 
matchChar x y = x == y

-- Gets and integer from a char.
-- Throws an expected error if it is not a digit
getNum x 
  | isDigit x = digitToInt x
  | otherwise = expected "Integer"

-- Prefix a string with a tab
emit s = "\t" ++ s

-- Prefix a string with a tab and postfix it with a new line
emitLn s = (emit s) ++ "\n"

expression x = putStr $ emitLn ("MOVE #" ++ show (getNum x) ++ ",D0")