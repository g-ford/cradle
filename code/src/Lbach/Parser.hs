module Lbach.Parser
where

import Lbach.Grammar.Basics
import Lbach.Parser.Core
import Lbach.Parser.Expressions
import Lbach.Parser.Control

-- |Attempts to parse the string as an assignment
parse2 :: String -> Assign
parse2 s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)

parse s = case program s of
    Nothing -> error "Invalid program"
    Just (a, _) -> a


