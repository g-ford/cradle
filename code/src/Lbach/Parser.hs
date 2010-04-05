module Lbach.Parser 
    (module Lbach.Parser.Expressions 
    ,blocks)
where

import Lbach.Grammar.Basics
import Lbach.Parser.Core
import Lbach.Parser.Expressions

program :: Parser Program
program = block <+-> literal 'e' >>> Program

blocks = iter block

block :: Parser Block
block =  ifelse <|> ifthen2 <|> other    

other :: Parser Block
other = token letters' >>> Block

ifthen :: Parser Block		
ifthen = token (literal 'i') <-+> condition <+> block <+-> token (literal 'e') +>> Branch

ifthen2 :: Parser Block		
ifthen2 = accept "if" <-+> condition <+> block <+-> accept "end" +>> Branch 

-- ifelse :: Parser Block		
ifelse = accept "if" <-+> condition <+> block <+-> accept "else" <+> block <+-> accept "end" >>> br
    where br ((c, b1), b2) = Branch2 c b1 b2


condition :: Parser Condition
condition = token letters' >>> Condition

accept :: String -> Parser String
accept w = token (letters <=> (==w))

err :: String -> Parser a
err m cs = error (m ++ " near " ++ cs) 

