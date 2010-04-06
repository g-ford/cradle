module Lbach.Emitter.Control where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core
import Data.List

type State = Int

emitBlock :: Block -> String
emitBlock b = result
	where (s, result) = emitBlock' 0 b

emitBlock' :: State -> [Statement] -> (State, String)
emitBlock' s [] = (s, "")
emitBlock' s (b:bs) = (s2, first ++ rest)
	where (s1, first) = emitStatement s b
	      (s2, rest) = emitBlock' s1 bs

emitStatement :: State -> Statement -> (State, String)
emitStatement s (Branch cond b1) = (s', c ++ jmp ++ block1 ++ end)
    where c             = emitCondition cond 
          jmp           = emitLn ("jne " ++ lbl)
          (s', block1)  = emitBlock' s1 b1
          end           = emitLbl lbl
          (lbl, s1)     = getLbl s
emitStatement s (Branch2 cond b1 b2) = (s', c ++ jmpElse ++ block1 ++ jmpEnd ++ el ++ block2 ++ end)
    where c             = emitCondition cond 
          jmpElse       = emitLn ("jne " ++ lblElse)
          (s3, block1)  = emitBlock' s2 b1
          jmpEnd        = emitLn ("jmp " ++ lblEnd)
          el            = emitLbl lblElse
          (s', block2)  = emitBlock' s3 b2
          end           = emitLbl lblEnd
          (lblElse, s1) = getLbl s
          (lblEnd, s2)  = getLbl s1
emitStatement s (Statement b) = (s, emitLn ("<block> " ++ b))
    
emitCondition (Condition s) = emitLn ("<condition> " ++ s)
