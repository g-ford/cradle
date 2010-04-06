module Lbach.Parser
where

import Lbach.Grammar.Basics
import Lbach.Parser.Core
import Lbach.Parser.Expressions

-- |Attempts to parse the string as an assignment
parse2 :: String -> Assign
parse2 s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)

parse s = case program s of
    Nothing -> error "Invalid program"
    Just (a, _) -> a

program :: Parser Program
program = block <+-> accept "end" >>> Program

block :: Parser Block
block = iter statement

statement :: Parser Statement
statement =  ifelse <|> ifthen <|> other

keywords = ["if", "else", "end"]

other :: Parser Statement
other = (token letters') <=> (\x -> not $ any (==x) keywords) >>> Statement

ifthen :: Parser Statement		
ifthen = accept "if" <-+> condition <+> block <+-> accept "end" +>> Branch

ifelse :: Parser Statement		
ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> br
    where br ((c, b1), b2) = Branch2 c b1 b2


condition :: Parser Condition
condition = token letters' >>> Condition

