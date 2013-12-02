import Data.Char

expected x = error $ x ++ " expected"

term x
  | isDigit x = [x]
  | otherwise = expected "Digit" 

addOperation x
  | x == '+' = "+"
  | x == '-' = "-"
  | otherwise = expected "AddOp"

expression (x:[]) = term x
expression (x:y:zs) = (term x) ++ (addOperation y) ++ (expression zs)


