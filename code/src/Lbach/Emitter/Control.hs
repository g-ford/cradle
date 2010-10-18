module Lbach.Emitter.Control where

import Lbach.Grammar.Basics
import Lbach.Emitter.Core
import Data.List
import Control.Monad.State

emitBlock :: Block -> String
emitBlock b = evalState e EmitterData { lblCounter = 0, lastLabel = "" }
	where e = do
              a <- emitBlock' b
              return a

emitBlock' :: Block -> EmitterState String
emitBlock' [] = do 
                  return ""
emitBlock' (st:bs) = do
    a <- emitStatement st
    b <- emitBlock' bs
    return (a ++ b)

 
emitStatement :: Statement -> EmitterState String

emitStatement (Branch cond b1) = do
    endLbl <- getLbl
    c <- emitCondition cond
    let jmp = emitLn ("jne " ++ endLbl)
    b <- emitBlock' b1
    return (c ++ jmp ++ b ++ (emitLbl endLbl))

emitStatement (Branch2 cond b1 b2) = do
    endLbl <- getLbl
    elseLbl <- getLbl
    c <- emitCondition cond
    block1 <- emitBlock' b1
    block2 <- emitBlock' b2
    let jmpElse = emitLn ("jne " ++ elseLbl)
    let jmpEnd = emitLn ("jmp " ++ endLbl)
    return (c ++ jmpElse ++ block1 ++ jmpEnd ++ (emitLbl elseLbl) ++ block2 ++ (emitLbl endLbl))

emitStatement (While cond b) = do
    startLbl <- getLbl
    endLbl <- getLbl
    c <- emitCondition cond
    block <- emitBlock' b
    let jmp = emitLn ("je " ++ endLbl)
    let loop = emitLn ("jmp " ++ startLbl)
    return ((emitLbl startLbl) ++ c ++ jmp ++ block ++ loop ++ (emitLbl endLbl))

emitStatement (Loop b) = do
    startLbl <- getLbl
    block <- emitBlock' b
    let jmp = emitLn ("je " ++ startLbl)
    return ((emitLbl startLbl) ++ block ++ jmp)

emitStatement (DoUntil b cond) = do
    startLbl <- getLbl
    endLbl <- getLbl
    c <- emitCondition cond
    block <- emitBlock' b
    let jmp = emitLn ("je " ++ endLbl)
    let loop = emitLn ("jmp " ++ startLbl)
    return ((emitLbl startLbl) ++ block ++ c ++ jmp ++ loop ++ (emitLbl endLbl))

emitStatement (Statement b) = do
    return (emitLn ("<block> " ++ b))
    
emitCondition (Condition c) = do
    return (emitLn ("<condition> " ++ c))
