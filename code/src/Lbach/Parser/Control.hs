module Lbach.Parser.Control where

import Lbach.Grammar.Basics
import Lbach.Parser.Core

-- |Top level parser and main entry point
program :: Parser Program
program = block <+-> accept "end" >>> Program

-- |A block is a collection of statments
block :: Parser Block
block = iter statement

-- |A statment is a basic line of code, and can consitute any number of control structures and expressions.
statement :: Parser Statement
statement = loop 
            <|> dountil
            <|> while 
            <|> ifelse 
            <|> ifthen 
            <|> other

-- |This is a temporary parser that accepts anything except keywords
other :: Parser Statement
other = (token letters') <=> (\x -> not $ any (==x) keywords) >>> Statement

-- |Parses if..then..end statments
ifthen :: Parser Statement		
ifthen = accept "if" <-+> condition <+> block <+-> accept "end" +>> Branch

-- |Parses if..then..else..end statments
ifelse :: Parser Statement		
ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> br
    where br ((c, b1), b2) = Branch2 c b1 b2

-- |Parses while..end statments
while :: Parser Statement
while = accept "while" <-+> condition <+> block <+-> accept "end" +>> While

-- |Will eventually parse a boolean logic statment. Currently accepts anything expect keywords.
condition :: Parser Condition
condition = token letters' <=> (\x -> not $ any (==x) keywords) >>> Condition

-- |Parses loop..end statments
loop :: Parser Statement
loop = accept "loop" <-+> block <+-> accept "end" >>> Loop

-- |Parses do..until statments
dountil :: Parser Statement
dountil = accept "do" <-+> block <+-> accept "until" <+> condition +>> DoUntil