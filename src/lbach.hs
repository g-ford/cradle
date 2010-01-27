import Char

data Expression = Num Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                deriving (Show)

parse :: String -> [Expression]
parse [] = []
parse (x:xs)
    | isDigit x = Num (digitToInt x) : parse xs