module Lbach.Emitter.Control where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core
import Data.List

emitBlock :: Block -> String
emitBlock b = foldl' (++) [] (map emitStatement b)

emitStatement :: Statement -> String
emitStatement (Branch cond b1) = 
    emitCondition cond 
    ++ emitLn "jne end"
    ++ emitBlock b1
    ++ emitLbl "end"
emitStatement (Branch2 cond b1 b2) = 
    emitCondition cond 
    ++ emitLn "jne else"
    ++ emitBlock b1
    ++ emitLn "jmp end"
    ++ emitLbl "else"
    ++ emitBlock b2
    ++ emitLbl "end"
emitStatement (Statement b) = 
    emitLn ("<block> " ++ b)
    

emitCondition (Condition s) = 
    emitLn ("<condition> " ++ s)
