module Lbach.Parser.Control where

import Lbach.Grammar.Basics
import Lbach.Parser.Core
import Lbach.Parser.Expressions

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
            <|> forloop
            <|> assign
            <|> other

-- |This is a temporary parser that accepts anything except keywords
other :: Parser Statement
other = token letters' <=> (\x -> not $ any (==x) keywords) >>> Statement

-- |Parses if..then..end statments
ifthen :: Parser Statement		
ifthen = acceptWord "if" <-+> condition <+> block <+-> acceptWord "end" +>> Branch

-- |Parses if..then..else..end statments
ifelse :: Parser Statement		
ifelse = acceptWord "if" <-+> condition <+> block <+-> acceptWord "else" <+> block <+-> acceptWord "end" >>> br
    where br ((c, b1), b2) = Branch2 c b1 b2

-- |Parses while..end statments
while :: Parser Statement
while = acceptWord "while" <-+> condition <+> block <+-> acceptWord "end" +>> While

-- |Will eventually parse a boolean logic statment. Currently accepts anything expect keywords.
condition :: Parser Condition
condition = token letters' <=> (\x -> not $ any (==x) keywords) >>> Condition

-- |Parses loop..end statments
loop :: Parser Statement
loop = acceptWord "loop" <-+> block <+-> acceptWord "end" >>> Loop

-- |Parses do..until statments
dountil :: Parser Statement
dountil = acceptWord "do" <-+> block <+-> acceptWord "until" <+> condition +>> DoUntil

-- |Parse a for loop
forloop :: Parser Statement		
forloop = acceptWord "for" <-+> assign <+-> acceptWord "to" <+> expr <+> block <+-> acceptWord "end" >>> br
    where br ((s, e), b) = For s e b
