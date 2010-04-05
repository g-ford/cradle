module Main
where

import System.Environment
import Lbach.Parser
import Lbach.Grammar.Basics
import Lbach.Emitter

main :: IO ()
main = getArgs >>= putStrLn . parseAndEmit . head

-- |Attempts to parse the string as an assignment
parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)


-- Shotcut to parse and emit with one call 
parseAndEmit :: String -> String
parseAndEmit = emit . parse
