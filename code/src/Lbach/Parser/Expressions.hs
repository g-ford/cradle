module Lbach.Parser.Expressions
where

import Lbach.Parser.Core
import Lbach.Grammar.Basics


-- |A parser that identifies assigment statments.
assign :: Parser Statement                  
assign = token letters <+-> token (literal '=') <+> expr >>> br
    where br (v, e) = Assign $ Assignment v e

-- |A parser to detect a factor, the basic token of an expression
factor :: Parser Expression
factor = token number 
     <|> token (literal '(') <-+> token expr <+-> token (literal ')')
	 <|> token var

-- |A number token parser 
number :: Parser Expression
number = integer >>> Num

-- |A variable token parser
var :: Parser Expression
var = letters >>> Var

-- |Parsers multiplication and division, giving the appropriate data constructor
mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = token (literal '*') >>> (\_ -> Mul)
	<|> token (literal '/') >>> (\_ -> Div)

term = factor +> term'	

term' e = mulOp <+> factor >>> buildOp e +> term'
      <|> result e

addOp = token (literal '+') >>> (\_ -> Add)
	<|> token (literal '-') >>> (\_ -> Sub)
	
expr :: Parser Expression
expr = term +> expr'

expr' :: Expression -> Parser Expression
expr' e = addOp <+> term >>> buildOp e +> expr'
      <|> result e
	  
buildOp e (op, e') = op e e'

integer :: Parser Int
integer = digitVal +> integer'

integer' :: Int -> Parser Int
integer' n = digitVal >>> buildNumber n +> integer'
         <|> result n

buildNumber :: Int -> Int -> Int
buildNumber n d = 10 * n + d