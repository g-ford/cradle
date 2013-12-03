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

emit expr = case expr of
	Num x -> [x]
	Add x y -> emit x ++ " + " ++ emit y
	Sub x y -> emit x ++ " - " ++ emit y

emitLn s = "\t" ++ s ++ "\n"

add = emitLn "ADD eax, ebx"
sub = emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
pushEax = emitLn "MOV ebx, eax"

emitAsm expr = case expr of 
	Num a   -> emitLn ("MOV eax, " ++ [a])
	Add a b -> emitAsm a ++  pushEax ++ emitAsm b ++ add
	Sub a b -> emitAsm a ++  pushEax ++ emitAsm b ++ sub
