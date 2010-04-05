module Lbach.Parser 
    (module Lbach.Parser.Expressions, module Lbach.Parser.Core, other, program)
where

import Lbach.Grammar.Basics
import Lbach.Parser.Core
import Lbach.Parser.Expressions

program :: Parser Program
program = block <+-> accept "end" >>> Program

block :: Parser Block
block = iter statement

statement :: Parser Statement
statement =  ifelse <|> ifthen <|> other    

other :: Parser Statement
other = (token letters') <=> (/="end") >>> Statement

ifthen :: Parser Statement		
ifthen = accept "if" <-+> condition <+> block <+-> accept "end" +>> Branch 

ifelse :: Parser Statement		
ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> br
    where br ((c, b1), b2) = Branch2 c b1 b2


condition :: Parser Condition
condition = token letters' >>> Condition

