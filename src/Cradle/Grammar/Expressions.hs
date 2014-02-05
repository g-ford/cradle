module Cradle.Grammar.Expressions

where

import Cradle.Parser

-- BNF
-- <b-expression>  ::= <b-term> [<orop> <b-term>]*
-- <b-term>￼	   ::= <not-factor> [AND <not-factor>]*
-- <not-factor>    ::= [NOT] <b-factor>	
-- <b-factor>	   ::= <b-literal> | <b-variable> | <relation>
-- <relation>	   ::= | <expression> [<relop> <expression]

data Expression = 
  Num Integer 
  | Var String
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  deriving (Show)

data Assign = Assign String Expression 
	deriving Show

assign :: Parser Assign
assign = token(letters) <+-> token(literal '=') <+> expression >>> (\(x, y) -> Assign x y)

expression :: Parser Expression
expression = token(term) +> expression'
expression' e = addOp <+> term >>> buildOp e +> expression'
            <|> result e

term :: Parser Expression
term = token(factor) +> term'
term' e = mulOp <+> term >>> buildOp e +> term'
      <|> result e

factor :: Parser Expression
factor = token (literal '(') <-+> token(expression) <+-> token(literal ')')
	 <|> number >>> Num
	 <|> letters >>> Var

buildOp :: Expression -> ((Expression -> Expression -> Expression), Expression) -> Expression
buildOp expressionA (op, expressionB) = op expressionA expressionB

addOp :: Parser (Expression -> Expression -> Expression)
addOp = token(literal '+') >>> (\_ -> Add)
    <|> token(literal '-') >>> (\_ -> Sub)

mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = token(literal '*') >>> (\_ -> Mul)
    <|> token(literal '/') >>> (\_ -> Div)