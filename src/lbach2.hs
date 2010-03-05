module Main
where

import Char

data Expression = Num Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Var String
                deriving (Show)
                
data Assign = Assign Char Expression
              deriving (Show)

type Parser a = String -> Maybe (a, String)

parse = assign

assign :: String -> Assign
assign s = Assign id expr
    where (id, expr) = case assign' s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)
                   
assign' = token letter <+-> token (literal '=') <+> expr

-- Expression parsers
mulOp = token (literal '*') >>> (\_ -> Mul)
	<|> token (literal '/') >>> (\_ -> Div)

addOp = token (literal '+') >>> (\_ -> Add)
	<|> token (literal '-') >>> (\_ -> Sub)

var :: Parser Expression
var = token (letters >>> Var)

num :: Parser Expression
num = token (integer >>> Num)

factor :: Parser Expression
factor = num
     <|> literal '(' <-+> expr <+-> literal ')'
	 <|> var

term :: Parser Expression
term = factor +> term'
	 
term' e = mulOp <+> factor >>> buildOp e +> term'
      <|> Main.return e
		 
expr :: Parser Expression
expr = term +> expr'

expr' :: Expression -> Parser Expression
expr' e = addOp <+> term >>> buildOp e +> expr'
      <|> Main.return e
		 
buildOp expr (op, expr') = op expr expr'
	 
-- Basic parsers (Building Blocks)
ident :: Parser String
ident = token (letter <+> iter alphanum >>> cons)

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

return :: a -> Parser a
return a cs = Just(a,cs)

fail :: Parser a
fail _ = Nothing

token :: Parser a -> Parser a
token = (<+-> iter space)

cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

integer :: Parser Int
integer = digitVal +> integer'

integer' :: Int -> Parser Int
integer' n = digitVal >>> buildNumber n +> integer'
         <|> Main.return n

buildNumber :: Int -> Int -> Int
buildNumber n d = 10 * n + d


-- Parser combinators (Operations)   
iterate :: Parser a -> Int -> Parser [a]
iterate m 0 = Main.return []
iterate m x = m <+> Main.iterate m (x-1) >>> cons

iter :: Parser a -> Parser [a]
iter m = m <+> iter m >>> cons 
        <|> Main.return []

-- Combine two parsers using a 'or' type operation        
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(m <|> n) cs = case m cs of 
    Nothing -> n cs 
    mcs -> mcs

infix 4 +>
(+>) :: Parser a -> (a -> Parser b) -> Parser b
(m +> k) cs = case m cs of
	Nothing -> Nothing
	Just (a, cs') -> k a cs'
    
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
        
infixl 6 <-+> 
(<-+> ) :: Parser a -> Parser b -> Parser b
(m <-+>  n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(b, cs2) 
        
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
(m <=> p) cs =    case m cs of 
        Nothing     -> Nothing 
        Just(a,cs)  -> if p a then Just(a,cs) else Nothing



