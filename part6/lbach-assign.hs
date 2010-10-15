import Char

data Expression = Num Int
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Var Char
                | Assign Char Expression
                deriving (Show)

-- Turns a string into an Expression 
parse :: String -> Expression
parse (x:'=':zs) 
    | isAlpha x = Assign x (parse zs)
    | otherwise = error "expected identifier"
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
emit expr= "section .data\n" ++ emitData expr 
            ++ "section .bss\n" ++ emitBss expr 
            ++ "section .text\n" ++emitText expr

-- Generates the contents of section .text
emitText :: Expression -> String
emitText expr = case expr of 
     Num a      -> emitLn ("MOV eax, " ++ (show a))
     Add a b    -> emitText a ++  pushEax ++ emitText b ++ add
     Sub a b    -> emitText a ++  pushEax ++ emitText b ++ sub
     Mul a b    -> emitText a ++  pushEax ++ emitText b ++ mul
     Div a b    -> emitText a ++  pushEax ++ emitText b ++ divide
     Var a      -> emitLn ("MOV eax, [" ++ [a] ++ "]")
     Assign a b -> emitText b ++ emitLn ("MOV [" ++ [a] ++ "], eax")
     
-- Generates the contents of section .data
emitData :: Expression -> String
emitData expr = case expr of
    Var a       -> emitLn ([a] ++ "\tdd\t0")
    Add a b     -> emitData a ++ emitData b
    Sub a b     -> emitData a ++ emitData b
    Mul a b     -> emitData a ++ emitData b
    Div a b     -> emitData a ++ emitData b
    Assign a b  -> emitData b 
    otherwise   -> ""

-- Generates the contents of section .bss
emitBss ::: Expression -> String    
emitBss expr = case expr of
    Assign a b  -> emitLn ([a] ++ "\tresd") ++ emitBss b
    Add a b     -> emitBss a ++ emitBss b
    Sub a b     -> emitBss a ++ emitBss b
    Mul a b     -> emitBss a ++ emitBss b
    Div a b     -> emitBss a ++ emitBss b
    otherwise   -> ""

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
divide = emitLn "MOV ebx, eax" ++ popEax ++ emitLn "MOV edx, 0" ++ emitLn "DIV ebx"
    