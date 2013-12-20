module Main
where

import System.Environment
import Cradle.Grammar

main :: IO ()
main = getArgs >>= print . parse . head

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)





