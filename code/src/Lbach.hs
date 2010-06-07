module Main
where

import System.Environment
import Lbach.Parser
import Lbach.Grammar.Basics
import Lbach.Emitter

main :: IO ()
main = getArgs >>= putStrLn . emit . parse . head

p = putStrLn . emit . parse
