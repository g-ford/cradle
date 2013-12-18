import Data.Char

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

type Parser a = String -> Maybe (a, String)


expected x = error $ x ++ " expected"

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)

assign :: Parser (String, Expression)
assign = token(letters) <+-> token(literal '=') <+> expression

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

char :: Parser Char
char [] = Nothing
char (x:xs) = Just(x, xs)

digit :: Parser Char
digit = char <=> isDigit

digits :: Parser String
digits = iter digit 

number :: Parser Integer
number = digits >>> (\n -> read n :: Integer)

space :: Parser Char
space = char <=> isSpace    

letter :: Parser Char
letter = char <=> isAlpha

letters :: Parser String
letters = iter letter

alphanum :: Parser Char
alphanum = digit <|> letter

literal :: Char -> Parser Char
literal c = char <=> (==c)

result :: a -> Parser a
result a cs = Just(a,cs)

addOp :: Parser (Expression -> Expression -> Expression)
addOp = token(literal '+') >>> (\_ -> Add)
    <|> token(literal '-') >>> (\_ -> Sub)

mulOp :: Parser (Expression -> Expression -> Expression)
mulOp = token(literal '*') >>> (\_ -> Mul)
    <|> token(literal '/') >>> (\_ -> Div)

iter :: Parser Char -> Parser String
iter m = (iterS m) <=> (/="")
iterS m = m <+> iterS m >>> (\(x, y) -> x:y)
	 <|> result []

token :: Parser a -> Parser a
token = (<+-> iterS space)

-- Given a parser and a predicate return the parser only if it satisfies the predicate.
infix 7 <=> 
(<=>) :: Parser a -> (a -> Bool) -> Parser a 
(parser <=> predicate) input =
    case parser input of 
        Nothing       -> Nothing 
        Just(a,rest)  -> if (predicate a) then Just(a,rest) else Nothing


-- Combine two parser together pairing their results up in a tuple
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(parserA <+> parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
        	Just (resultB, cs) -> Just((resultA, resultB), cs)

-- Sequence operator that discards the second result 
infixl 6 <+-> 
(<+-> ) :: Parser a -> Parser b -> Parser a
(parserA <+->  parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
	        Just (_, cs) -> Just(resultA, cs)

-- Sequence operator that discards the first result       
infixl 6 <-+> 
(<-+> ) :: Parser a -> Parser b -> Parser b
(parserA <-+>  parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
	        Just (resultB, cs) -> Just(resultB, cs) 

-- Transform a parsers result
infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b
(parser >>> transformer) input = 
	case parser input of
		Nothing -> Nothing
		Just (resultA, remainder) -> Just ((transformer resultA), remainder)

-- Extract a parsers result	
infix 4 +>
(+>) :: Parser a -> (a -> Parser b) -> Parser b
(parser +> function) input = 
	case parser input of
		Nothing -> Nothing
		Just (a, cs) -> function a cs

-- Combine two parsers using a 'or' type operation        
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(parserA <|> parserB) input = 
	case parserA input of 
    	Nothing -> parserB input 
    	result -> result