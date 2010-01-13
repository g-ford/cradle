import Char

-- Throw an expected error
expected s = s ++ " expected"

-- Tests two chars match
matchChar :: Char -> Char -> Bool 
matchChar x y = x == y

-- Gets and identifier.
-- Throws an expected error if it is not an alpha
getName :: Char -> Either String Char
getName x 
  | isAlpha x = Right x
  | otherwise = Left (expected "Name")

-- Checks if the char is a digit.
-- Throws an expected error if it is not a digit
getNum :: Char -> Either String Char
getNum x 
  | isDigit x = Right x
  | otherwise = Left (expected "Integer")

-- Prefix a string with a tab
emit s = "\t" ++ s

-- Prefix a string with a tab and postfix it with a new line
emitLn s = (emit s) ++ "\n"

expression x = 
  case getNum x of
    Left msg  -> putStrLn msg
    Right num -> putStrLn $ emitLn ("MOVE #" ++ [num] ++ ",D0")