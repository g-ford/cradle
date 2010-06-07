module Lbach.Parser.Control where

import Lbach.Grammar.Basics
import Lbach.Parser.Core

program :: Parser Program
program = block <+-> accept "end" >>> Program

block :: Parser Block
block = iter statement

statement :: Parser Statement
statement = loop 
            <|> dountil
            <|> while 
            <|> ifelse 
            <|> ifthen 
            <|> other

other :: Parser Statement
other = (token letters') <=> (\x -> not $ any (==x) keywords) >>> Statement

ifthen :: Parser Statement		
ifthen = accept "if" <-+> condition <+> block <+-> accept "end" +>> Branch

ifelse :: Parser Statement		
ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> br
    where br ((c, b1), b2) = Branch2 c b1 b2

while :: Parser Statement
while = accept "while" <-+> condition <+> block <+-> accept "end" +>> While

condition :: Parser Condition
condition = token letters' <=> (\x -> not $ any (==x) keywords) >>> Condition

loop :: Parser Statement
loop = accept "loop" <-+> block <+-> accept "end" >>> Loop

dountil :: Parser Statement
dountil = accept "do" <-+> block <+-> accept "until" <+> condition +>> DoUntil