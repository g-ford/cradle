module Cradle.Grammar.Control

where

import Cradle.Parser
import Cradle.Grammar.Expressions
import Cradle.Grammar.Boolean

data Program = Program Block deriving (Show)
newtype Block = Block [Statement] deriving (Show)
data Statement = 
	Statement Assign 
  | Branch BoolExpression Block
  | Branch2 BoolExpression Block Block
  | While BoolExpression Block
  | Break
  deriving (Show)

program :: Parser Program
program = block <+-> accept "end" >>> Program

block :: Parser Block
block = iterS statement >>> Block

statement :: Parser Statement
statement = assign >>> Statement
  <|> ifelse
  <|> ifthen 
  <|> while
  <|> breakSt

while :: Parser Statement		
while = accept "while" <-+> condition <+> block <+-> accept "end" >>> buildWhile
    where buildWhile (c, b) = While c b

ifthen :: Parser Statement		
ifthen = accept "if" <-+> condition <+> block <+-> accept "end" >>> buildBranch
    where buildBranch (c, b) = Branch c b

ifelse :: Parser Statement		
ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> buildBranch
    where buildBranch ((c, b1), b2) = Branch2 c b1 b2

breakSt :: Parser Statement
breakSt = accept "break" >>> \_ -> Break

condition = boolExpression

-- |This is a temporary parser that accepts anything except keywords
tempPlaceholder :: Parser String
tempPlaceholder = token letters <=> (\x -> not $ any (==x) keywords) 
  where keywords = ["if", "else", "end", "endif", "while", "until"]