import Data.Char

data Expression = 
  Num Char 
  | Add Expression Expression
  | Sub Expression Expression
  deriving (Show) 

data Assign = Assign Char Expression 
	deriving Show

type Parser a = String -> Maybe (a, String)


expected x = error $ x ++ " expected"


parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, Num b)



char :: Parser Char
char [] = Nothing
char (x:xs) = Just(x, xs)

digit :: Parser Char
digit = char <=> isDigit

space :: Parser Char
space = char <=> isSpace    

letter :: Parser Char
letter = char <=> isAlpha

alphanum :: Parser Char
alphanum = digit <|> letter

literal :: Char -> Parser Char
literal c = char <=> (==c)

assign :: Parser (Char, Char)
assign = letter <+-> literal '=' <+> digit


-- Given a parser and a predicate return the parser only if it satisfies the predicate.
infix 7 <=> 
(<=>) :: Parser a -> (a -> Bool) -> Parser a 
(parser <=> predicate) input =
    case parser input of 
        Nothing       -> Nothing 
        Just(a,rest)  -> if (predicate a) then Just(a,rest) else Nothing

infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(m <+> n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just((a, b), cs2)

-- Sequence operator that discards the second result 
infixl 6 <+-> 
(<+-> ) :: Parser a -> Parser b -> Parser a
(m <+->  n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(a, cs2)

-- Sequence operator that discards the first result       
infixl 6 <-+> 
(<-+> ) :: Parser a -> Parser b -> Parser b
(m <-+>  n) cs = case m cs of
    Nothing -> Nothing
    Just (a, cs') -> case n cs' of
        Nothing -> Nothing
        Just (b, cs2) -> Just(b, cs2) 

-- Combine two parsers using a 'or' type operation        
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(parserA <|> parserB) input = 
	case parserA input of 
    	Nothing -> parserB input 
    	result -> result