data Expression = Num Int
                    | Add Expression Expression
                    | Sub Expression Expression
                    | Mul Expression Expression
                    | Div Expression Expression
                    deriving (Show)

