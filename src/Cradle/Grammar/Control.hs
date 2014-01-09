module Cradle.Grammar.Control

where

import Cradle.Parser
import Cradle.Grammar.Expressions

data Program = Program Block deriving (Show)
type Block = [Statement]
data Statement = 
	Statement Assign 
  | Branch Condition Block
  | Branch2 Condition Block Block
  deriving (Show)

type Condition = String

program :: Parser Program
program = block <+-> accept "end" >>> Program

block :: Parser Block
block = iterS statement

statement :: Parser Statement
statement = assign >>> Statement
  <|> ifelse
  <|> ifthen 

ifthen :: Parser Statement		
ifthen = accept "if" <-+> condition <+> block <+-> accept "end" >>> buildBranch
    where buildBranch (c, b) = Branch c b

ifelse :: Parser Statement		
ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> buildBranch
    where buildBranch ((c, b1), b2) = Branch2 c b1 b2

condition = tempPlaceholder

-- |This is a temporary parser that accepts anything except keywords
tempPlaceholder :: Parser String
tempPlaceholder = token letters <=> (\x -> not $ any (==x) keywords) 
  where keywords = ["if", "else", "end", "endif", "while", "until"]