module Lbach.Emitter.Core where

data EmitterData = EmitterData {
    lblCounter :: Int,
    lastLabel :: String
}

type EmitterState = State EmitterData

-- Prefix a string with a tab
emitSt s = "\t" ++ s
 
-- Prefix a string with a tab and postfix it with a new line
emitLn s = (emitSt s) ++ "\n"

getLbl :: EmitterState String
getLbl = do 
    st <- get
    let l = "L" ++ (show lblCounter st)
    put st { lblCounter = 1 + lblCounter st }
    return l

emitLbl :: String -> String
emitLbl lbl = lbl ++ ":\n"
 
-- Basic math functions
popEbx = emitLn "POP ebx"
popEax = emitLn "POP eax"
pushEax = emitLn "PUSH eax"  
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
mul = popEbx ++ emitLn "MUL ebx"
divide = emitLn "MOV ebx, eax" ++ popEax ++ emitLn "MOV edx, 0" ++ emitLn "DIV ebx"