module Lbach.Grammar.Basics
where

data Program = Program Block deriving (Show)

type Block = [Statement] 

data Statement = Statement String 
		      | Branch Condition Block
		      | Branch2 Condition Block Block
              | While Condition Block
			  | Loop Block
              | DoUntil Block Condition
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