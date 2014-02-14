module Main
where

import System.Environment
import Cradle.Grammar.Control
import Cradle.Generator.Nasm

main :: IO ()
main = getArgs >>= e . head

parse :: String -> Program
parse s = case program s of 
            Nothing -> error "Invalid program"
            Just (a, b) -> a

-- | Parse and print. Utility and test function for use in @ghci@.
p = putStrLn . show . parse

-- | Parse and emit.
e = putStrLn . emit . parse