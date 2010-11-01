module Lbach.Emitter
where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core
import Lbach.Emitter.Expressions
import Lbach.Emitter.Control

-- | Puts a parse tree through the emitter
emit :: Program -> String
emit (Program b) = emitBlock b ++ emitLn "ret"