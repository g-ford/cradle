module Cradle.Grammar.Control

where

import Cradle.Parser
import Cradle.Grammar.Expressions

data Program = Program Block deriving (Show)
type Block = [Statement]
data Statement = Statement Assign 
  | Branch Condition Block
  deriving (Show)

type Condition = String

program :: Parser Program
program = block <+-> accept "end" >>> Program

block :: Parser Block
block = iterS statement

statement :: Parser Statement
statement = assign >>> Statement
