import Data.Char

data Expression = 
  Num Char 
  | Add Expression Expression
  | Sub Expression Expression
  deriving (Show) 

expected x = error $ x ++ " expected"

term x
  | isDigit x = Num x
  | otherwise = expected "Digit"

addOperation x
  | x == '+' = Add
  | x == '-' = Sub
  | otherwise = expected "AddOp"


expression (x:[]) = term x
expression (x:y:zs) = (addOperation y) (expression [x]) (expression zs)


-- Emitter Funtions

emit expr = case expr of
	Num x -> [x]
	Add x y -> emit x ++ " + " ++ emit y
	Sub x y -> emit x ++ " - " ++ emit y

emitLn s = "\t" ++ s ++ "\n"

popEbx = emitLn "POP ebx"
pushEax = emitLn "PUSH eax"
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"

emitAsm expr = case expr of 
	Num a   -> emitLn ("MOV eax, " ++ [a])
	Add a b -> emitAsm b ++  pushEax ++ emitAsm a ++ add
	Sub a b -> emitAsm b ++  pushEax ++ emitAsm a ++ sub

parseAndEmit = emitAsm . expression
