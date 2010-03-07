module Main
where

import System.Environment
import Lbach.Parser
import Lbach.Parser.Expressions

main :: IO ()
main = getArgs >>= print . parse . head

-- |Attempts to parse the string as an assignment
parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)

-- |A parser that identifies assigment statments.
assign :: Parser (String, Expression)                   
assign = letters <+-> literal '=' <+> expr



