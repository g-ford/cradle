module Cradle.Grammar.RelationalAlgebra

where

import Cradle.Parser
import Cradle.Grammar.Expressions

data RelExpression = 
	BVar String
	| BTrue
	| BFalse
	| BAnd RelExpression RelExpression
	| BOr RelExpression RelExpression
	| BNot RelExpression
	| BExp Expression
	| REqual RelExpression RelExpression
	| RNotEqual RelExpression RelExpression
	| RGreaterThan RelExpression RelExpression
	| RLessThan RelExpression RelExpression
	| RGreaterThanOrEqualTo RelExpression RelExpression
	| RLessThanOrEqualTo RelExpression RelExpression
	deriving (Show)

boolExpression :: Parser RelExpression
boolExpression = token(bFactor) +> boolExpression'
boolExpression' e = boolOp <+> bFactor >>> buildRelOp e +> boolExpression'
            <|> result e

bFactor :: Parser RelExpression
bFactor = relExpression
	  <|> bNot <+> bLiteral >>> (\(n, lit) -> n lit)
      <|> bLiteral
      <|> bNot <+> bVar >>> (\(n, lit) -> n lit)
      <|> bVar
      


relExpression :: Parser RelExpression
relExpression = bExpression +> relExpression'
relExpression' e = relOp <+> bExpression >>> buildRelOp e +> relExpression'
               <|> result e

bExpression :: Parser RelExpression
bExpression = token expression >>> BExp

bNot :: Parser(RelExpression -> RelExpression)
bNot = token(literal '!') >>> (\_ -> BNot)

bVar :: Parser RelExpression
bVar = letters >>> BVar

bLiteral :: Parser RelExpression
bLiteral = accept "true"  >>> (\_ -> BTrue)
       <|> accept "false" >>> (\_ -> BFalse)


boolOp :: Parser (RelExpression -> RelExpression -> RelExpression)
boolOp = token(accept "&&") >>> (\_ -> BAnd)
    <|> token(accept "||") >>> (\_ -> BOr)

relOp :: Parser (RelExpression -> RelExpression -> RelExpression)
relOp = token(accept ">=") >>> (\_ -> RGreaterThanOrEqualTo)
    <|> token(accept "<=") >>> (\_ -> RLessThanOrEqualTo)
    <|> token(literal '>') >>> (\_ -> RGreaterThan)
    <|> token(literal '<') >>> (\_ -> RLessThan)
    <|> token(accept "==") >>> (\_ -> REqual)
    <|> token(accept "!=") >>> (\_ -> RNotEqual)

buildRelOp :: RelExpression -> ((RelExpression -> RelExpression -> RelExpression), RelExpression) -> RelExpression
buildRelOp expressionA (op, expressionB) = op expressionA expressionB