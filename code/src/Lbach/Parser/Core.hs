module Lbach.Parser.Core where

import Char

-- |A general parser type
type Parser a = String -> Maybe (a, String) 

keywords = ["if", "else", "end", "while", "until"]

-- |Parsers a single character.
char :: Parser Char
char [] = Nothing
char (c:cs) = Just (c, cs)

-- |Determines if the parsed character is a digit, but does no conversion.
digit :: Parser Char
digit = char <=> isDigit

-- |Parsers a single char to Int
digitVal :: Parser Int
digitVal = digit >>> digitToInt

-- |A whitespace parser
space :: Parser Char
space = char <=> isSpace    

-- |A alpha parser
letter :: Parser Char
letter = char <=> isAlpha

-- |A word parser that only accepts alpha characters.
letters :: Parser String
letters = iter letter

-- |A word parser that only accepts alpha characters and fails on empty.
letters' :: Parser String
letters' = iter' letter

-- |A parser that accepts a digit or letter
alphanum :: Parser Char
alphanum = digit <|> letter

-- |A parser that accepts only a specific character
literal :: Char -> Parser Char
literal c = char <=> (==c)

-- |Creates a Parser from any result
result :: a -> Parser a
result a cs = Just(a,cs)

-- |Repeats the given parser and joins the results together. Returns an empty list by default. 
iter :: Parser a -> Parser [a]
iter m = m <+> iter m >>> cons 
        <|> result []

-- |Repeats the given parser and joins the results together. Will fail if it cannot parse at least one item.       
iter' :: Parser a -> Parser [a]
iter' m = m <+> iter m >>> cons 
        <|> failed

-- |Utility to join lists		
cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

-- |A parser that strips any trailing whitespace
token :: Parser a -> Parser a
token = (<+-> iter space)

-- |Force a parser to fail
failed :: Parser a
failed cs = Nothing

-- |A parser that will accept a given alpha string
accept :: String -> Parser String
accept w = token (letters <=> (==w))

-- |Cause an error
err :: String -> Parser a
err m cs = error (m ++ " near " ++ cs) 

-- |Combine two parsers using a 'or' type operation. The second Parser will only be evaluated if the first fails.        
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(m <|> n) cs = case m cs of 
    Nothing -> n cs 
    mcs -> mcs

-- |Extract a parsers result and passes that result into the next parser.	
infix 4 +>
(+>) :: Parser a -> (a -> Parser b) -> Parser b
(m +> k) cs = case m cs of
	Nothing -> Nothing
	Just (a, cs') -> k a cs'

-- |Transform a parsers result
infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b
(m >>> n) cs = case m cs of
	Nothing -> Nothing
	Just (a, cs') -> Just (n a, cs')
	
-- |Transform a paired parsers result	
infixl 5 +>>
(+>>) :: Parser (a, b) -> (a -> b -> c) -> Parser c
(m +>> n) cs = case m cs of
	Nothing -> Nothing
	Just ((a, b), cs') -> Just ((n a b), cs')

-- |Sequence operator that pairs up two parsers
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(m <+> n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just((a, b), cs2) 

-- |Sequence operator that discards the first result       
infixl 6 <-+> 
(<-+> ) :: Parser a -> Parser b -> Parser b
(m <-+>  n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(b, cs2) 

-- |Sequence operator that discards the second result         
infixl 6 <+-> 
(<+-> ) :: Parser a -> Parser b -> Parser a
(m <+->  n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(a, cs2) 

-- |Given a parser and a predicate, return the result of the parser only if
-- it also satisfies the predicate.    
infix 7 <=> 
(<=>) :: Parser a -> (a -> Bool) -> Parser a 
(m <=> p) cs =
    case m cs of 
        Nothing     -> Nothing 
        Just(a,cs)  -> if p a then Just(a,cs) else Nothing