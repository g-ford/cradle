module Lbach.Parser.Core where

import Char

-- |A general parser type
type Parser a = String -> Maybe (a, String) 

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

letters' :: Parser String
letters' = iter' letter

alphanum :: Parser Char
alphanum = digit <|> letter

literal :: Char -> Parser Char
literal c = char <=> (==c)

result :: a -> Parser a
result a cs = Just(a,cs)

iter :: Parser a -> Parser [a]
iter m = m <+> iter m >>> cons 
        <|> result []
        
iter' :: Parser a -> Parser [a]
iter' m = m <+> iter m >>> cons 
        <|> failed
		
cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

token :: Parser a -> Parser a
token = (<+-> iter space)

failed :: Parser a
failed cs = Nothing

accept :: String -> Parser String
accept w = token (letters <=> (==w))

err :: String -> Parser a
err m cs = error (m ++ " near " ++ cs) 

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
	
-- Transform a paired parsers result	
infixl 5 +>>
(+>>) :: Parser (a, b) -> (a -> b -> c) -> Parser c
(m +>> n) cs = case m cs of
	Nothing -> Nothing
	Just ((a, b), cs') -> Just ((n a b), cs')

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