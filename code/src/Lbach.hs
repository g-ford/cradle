{-|
A toy compiler.

When used in @ghci@ you can test the compiler using @p@ e.g.

> p "a = 1 end"

When used on the command line, you currently have to pass the text in as the first argument. I do 
plan on takeing filenames as arguments in the future.

> lbach "a = 2 end" 
-}
module Main
where

import System.Environment
import Lbach.Parser
import Lbach.Grammar.Basics
import Lbach.Emitter

main :: IO ()
main = getArgs >>= putStrLn . emit . parse . head

-- | Test function foe use in @ghci@.  Parse and emits in a nice fashion
p = putStrLn . emit . parse

-- | Test function for use in @ghci@. Parses only
a = parse

