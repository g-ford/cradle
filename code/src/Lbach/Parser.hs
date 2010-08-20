module Lbach.Parser
where

import Lbach.Grammar.Basics
import Lbach.Parser.Core
import Lbach.Parser.Expressions
import Lbach.Parser.Control

parse s = case program s of
    Nothing -> error "Invalid program"
    Just (a, _) -> a


