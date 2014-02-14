module Cradle.Grammar.Boolean

where

import Cradle.Parser
import Cradle.Grammar.Expressions

data BoolExpression = 
	BVar String
	| BTrue
	| BFalse
	| BAnd BoolExpression BoolExpression
	| BOr BoolExpression BoolExpression
	| BNot BoolExpression
	| BExp Expression
	| REqual BoolExpression BoolExpression
	| RNotEqual BoolExpression BoolExpression
	| RGreaterThan BoolExpression BoolExpression
	| RLessThan BoolExpression BoolExpression
	| RGreaterThanOrEqualTo BoolExpression BoolExpression
	| RLessThanOrEqualTo BoolExpression BoolExpression
	deriving (Show)

boolExpression :: Parser BoolExpression
boolExpression = token(bFactor) +> boolExpression'
boolExpression' e = boolOp <+> bFactor >>> buildRelOp e +> boolExpression'
            <|> result e

bFactor :: Parser BoolExpression
bFactor = token(literal '(') <-+> boolExpression <+-> token(literal ')')
	  <|> relExpression
	  <|> bNot <+> bLiteral >>> (\(n, lit) -> n lit)
      <|> bLiteral
      <|> bNot <+> bVar >>> (\(n, lit) -> n lit)
      <|> bVar
      


relExpression :: Parser BoolExpression
relExpression = bExpression +> relExpression'
relExpression' e = relOp <+> bExpression >>> buildRelOp e +> relExpression'
               <|> result e

bExpression :: Parser BoolExpression
bExpression = token expression >>> BExp

bNot :: Parser(BoolExpression -> BoolExpression)
bNot = token(literal '!') >>> (\_ -> BNot)

bVar :: Parser BoolExpression
bVar = letters >>> BVar

bLiteral :: Parser BoolExpression
bLiteral = acceptWord "true"  >>> (\_ -> BTrue)
       <|> acceptWord "false" >>> (\_ -> BFalse)


boolOp :: Parser (BoolExpression -> BoolExpression -> BoolExpression)
boolOp = token(accept "&&") >>> (\_ -> BAnd)
    <|> token(accept "||") >>> (\_ -> BOr)

relOp :: Parser (BoolExpression -> BoolExpression -> BoolExpression)
relOp = token(accept ">=") >>> (\_ -> RGreaterThanOrEqualTo)
    <|> token(accept "<=") >>> (\_ -> RLessThanOrEqualTo)
    <|> token(literal '>') >>> (\_ -> RGreaterThan)
    <|> token(literal '<') >>> (\_ -> RLessThan)
    <|> token(accept "==") >>> (\_ -> REqual)
    <|> token(accept "!=") >>> (\_ -> RNotEqual)

buildRelOp :: BoolExpression -> ((BoolExpression -> BoolExpression -> BoolExpression), BoolExpression) -> BoolExpression
buildRelOp expressionA (op, expressionB) = op expressionA expressionB