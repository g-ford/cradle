import Control.Monad.State
          
-- This will hold all of the data in our state
data EmitStuff = EmitStuff {
    lblCounter :: Int,
    lastLabel :: String 
    } deriving (Show)

-- It is common practice to make your own state type
type MyStateMonad = State EmitStuff 

-- Generates a new label based on the current label counter
-- Stores the generated label, and updates the label counter
newLabel :: MyStateMonad String
newLabel = do 
    st <- get
    let l = "L" ++ show(lblCounter st)
    put st { lblCounter = lblCounter st + 1, lastLabel = l}
    return l
    
-- Emits a label followed by a comment 
emitLabelAndComment :: String -> MyStateMonad String
emitLabelAndComment x = do 
    l <- newLabel
    return (l ++ ":  #" ++ x)

-- Simple example showing how we no longer need to manually thread the state when creating labels    
fakeIfStatement :: MyStateMonad String     
fakeIfStatement = do 
    falseBranch <- newLabel
    endLabel <- newLabel
    return ("if not true then \njump " ++ falseBranch ++ "\nDo some true stuff\njump " ++ endLabel ++ "\n" ++ falseBranch ++ ": #some stuff to do if false\n" ++ endLabel)

-- Tying it all together 
emitAll = evalState emit EmitStuff { lblCounter = 0, lastLabel = "" } 
    where emit = do
              a <- emitLabelAndComment "This is the first label"
              b <- fakeIfStatement 
              c <- emitLabelAndComment "This is the last label"
              return (a ++ "\n" ++  b ++ "\n" ++ c)
              
