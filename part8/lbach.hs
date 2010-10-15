import Char

data Expression = Num Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Var String
                deriving (Show)
                
data Assign = Assign String Expression
              deriving (Show)

type Parser a = String -> Maybe (a, String)

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)
                   
assign = letters <+-> literal '=' <+> expr

-- Expression parsers
factor :: Parser Expression
factor = token number 
     <|> token (literal '(') <-+> token expr <+-> token (literal ')')
	 <|> token var

number :: Parser Expression
number = integer >>> Num

var :: Parser Expression
var = letters >>> Var

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

-- General Parsers
char :: Parser Char
char [] = Nothing
char (c:cs) = Just (c, cs)

digit :: Parser Char
digit = char <=> isDigit

digitVal :: Parser Int
digitVal = digit >>> digitToInt

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

iter :: Parser a -> Parser [a]
iter m = m <+> iter m >>> cons 
        <|> result []
		
cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

token :: Parser a -> Parser a
token = (<+-> iter space)

-- Combine two parsers using a 'or' type operation        
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(m <|> n) cs = case m cs of 
    Nothing -> n cs 
    mcs -> mcs

-- Extract a parsers result	
infix 4 +>
(+>) :: Parser a -> (a -> Parser b) -> Parser b
(m +> k) cs = case m cs of
	Nothing -> Nothing
	Just (a, cs') -> k a cs'

-- Transform a parsers result	
infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b
(m >>> n) cs = case m cs of
	Nothing -> Nothing
	Just (a, cs') -> Just (n a, cs')

-- Sequence operator that pairs up two parsers
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(m <+> n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just((a, b), cs2) 

-- Sequence operator that discards the first result       
infixl 6 <-+> 
(<-+> ) :: Parser a -> Parser b -> Parser b
(m <-+>  n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(b, cs2) 

-- Sequence operator that discards the second result         
infixl 6 <+-> 
(<+-> ) :: Parser a -> Parser b -> Parser a
(m <+->  n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(a, cs2) 

-- Given a parser and a predicate, return the result of the parser only if
-- it also satisfies the predicate.    
infix 7 <=> 
(<=>) :: Parser a -> (a -> Bool) -> Parser a 
(m <=> p) cs =
    case m cs of 
        Nothing     -> Nothing 
        Just(a,cs)  -> if p a then Just(a,cs) else Nothing