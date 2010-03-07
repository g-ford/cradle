module Lbach.Grammar.Basics
where

-- |A type for general mathmatical expressions including variables.
data Expression = Num Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Var String
                deriving (Show)

-- |A type for assignment statments                
data Assign = Assign String Expression
              deriving (Show)