module Lbach.Grammar.Basics
where

data Program = Program Block deriving (Show)

data Block = Block String 
		   | Branch Condition Block
		   deriving (Show)

data Condition = Condition String deriving (Show)

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