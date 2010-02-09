import Char

data Expression = Num Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Var Char
                deriving (Show)

-- Turns a string into an Expression 
parse :: String -> Expression
parse (a:b:c:d:ds) 
    | d == '+'  = Add (parse [a,b,c]) (parse ds)
    | d == '-'  = Sub (parse [a,b,c]) (parse ds)
parse (x:y:zs) 
    | y == '*'  = Mul (parse [x]) (parse zs)
    | y == '/'  = Div (parse [x]) (parse zs)
    | y == '+'  = Add (parse [x]) (parse zs)
    | y == '-'  = Sub (parse [x]) (parse zs)
parse (x:[])
    | isDigit x = Num (digitToInt x)
    | isAlpha x = Var x
        
-- Prefix a string with a tab
emitSt s = "\t" ++ s

-- Prefix a string with a tab and postfix it with a new line
emitLn s = (emitSt s) ++ "\n"

-- Turns an expression into the equvilent assembly
emit :: Expression -> String
emit expr = case expr of 
     Num a   -> emitLn ("MOV eax, " ++ (show a))
     Add a b -> emit a ++  pushEax ++ emit b ++ add
     Sub a b -> emit a ++  pushEax ++ emit b ++ sub
     Mul a b -> emit a ++  pushEax ++ emit b ++ mul
     Div a b -> emit a ++  pushEax ++ emit b ++ divide

-- Shotcut to parse and emit with one call 
parseAndEmit :: String -> String
parseAndEmit = emit . parse

-- Basic math functions
popEbx = emitLn "POP ebx"
popEax = emitLn "POP eax"
pushEax = emitLn "PUSH eax"
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"
mul = popEbx ++ emitLn "MUL ebx"
divide = pushEax ++ popEbx ++ popEax ++ emitLn "MOV edx, 0" ++ emitLn "DIV ebx"
    