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
emitBlock' s (b:bs) = (s', first ++ rest)
	where (s1, first) = emitStatement s b
	      (s', rest) = emitBlock' s1 bs

emitStatement :: State -> Statement -> (State, String)
emitStatement s (Branch cond b1) = (s', c ++ jmp ++ block1 ++ end)
    where (s1, c)       = emitCondition s cond 
          jmp           = emitLn ("jne " ++ lbl)
          (s', block1)  = emitBlock' s2 b1
          end           = emitLbl lbl
          (lbl, s2)     = getLbl s1
emitStatement s (Branch2 cond b1 b2) = (s', c ++ jmpElse ++ block1 ++ jmpEnd ++ el ++ block2 ++ end)
    where (s1, c)       = emitCondition s cond 
          jmpElse       = emitLn ("jne " ++ lblElse)
          (s4, block1)  = emitBlock' s3 b1
          jmpEnd        = emitLn ("jmp " ++ lblEnd)
          el            = emitLbl lblElse
          (s', block2)  = emitBlock' s4 b2
          end           = emitLbl lblEnd
          (lblElse, s2) = getLbl s1
          (lblEnd, s3)  = getLbl s2
emitStatement s (While c b) = (s', start ++ cond ++ jmp ++ block ++ loop ++ end)
    where start = emitLbl startLbl
          (s1, cond) = emitCondition s c
          jmp = emitLn ("je " ++ endLbl)
          (s', block) = emitBlock' s3 b
          loop = emitLn ("jmp " ++ startLbl)
          end = emitLbl endLbl
          (endLbl, s2) = getLbl s1
          (startLbl, s3) = getLbl s2
emitStatement s (Statement b) = (s, emitLn ("<block> " ++ b))
    
emitCondition s (Condition c) = (s, emitLn ("<condition> " ++ c))
