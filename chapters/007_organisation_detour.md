---
layout: layout
title: "An organisational detour"
group: book
permalink: organisation_detour.html
---

# An organisational detour

My `Main.hs` is running out to about ~150 lines and things are starting to get a bit cumbersome.  Now is a good time to start thinking about modularising the compiler and introducing a bit of seperation and organisation.

The preferred method for creating distributable Haskell packages, whether they are libraries or programs, is with [the Cabal](http://www.haskell.org/cabal/). The Cabal is a small wrapper around the build tools, and allows some standardised dependancy management, license distribution etc.

## Packaging

Cabal uses a `<project>.cabal` file to define the parameters for your project to build and distribute.  The easiest way to create this file is to run at the prompt : 

~~~ Haskell
cabal init
~~~

A series of questions are used to gather the information and a `.cabal` file is created in the directory you ran it in.  Yours may look different to mine depending on the answeres and choices you made.  

## Creating an executable

Assuming you answered `cabal.install` with mostly defaults you will need to make a few small changes to your program to be able to build it in cabal.

Here are the relevant parts that will determine the changes you need to make.

~~~ Haskell
executable cradle
  main-is:             Cradle.hs
  -- other-modules:       
  build-depends:       base ==4.6.*, QuickCheck ==2.6.*, HUnit ==1.2.*, mtl ==2.1.*
  hs-source-dirs:      src
~~~

This says that we are building an executable and the main entry point into the program is located in `src/Cradle.hs`.
You will need to edit `src/Cradle.hs` to contain a module called `Main` and a function called `main`, which all Haskell programs must have as their entry point.  

~~~ Haskell
module Main
where

import Data.Char
import System.Environment

main :: IO ()
main = getArgs >>= print . parse . head

...remainder of file...
~~~

Once this change has been made we can either build or install and then test our new program.

~~~ Haskell
~/cradle ➤ cabal install --prefix=$HOME --user 
Resolving dependencies...
Configuring cradle-0.1.0.0...
Building cradle-0.1.0.0...
Preprocessing executable 'cradle' for cradle-0.1.0.0...
Warning: No documentation was generated as this package does not contain a
library. Perhaps you want to use the --executables flag.
Installing executable(s) in /Users/geoffford/bin
Installed cradle-0.1.0.0
~/cradle ➤ ~/bin/cradle "test = 123 + a"        
Assign "test" (Add (Num 123) (Var "a"))
~~~

## Modularisation

To keep our program source files from running to unmanageable lengths we will need to break them up into separate files.  This is also a good time to think about how we are going to split the compiler up into modules.  

We immediately know that there are three basic modules - the Parser, the Generator and the Grammar or abstract syntax.  This is the main phases of nearly all compilers, although many will have some other phases that we havn't covered such as optimisers.  I'll also prefix everything with Cradle namespace so as not to get in the way of any existing namespaces. 

With Haskell, modules are expected to be in files and folders that reflect their name.  For example the module `My.Great.Module` should be in the file `My/Great/Module.hs`. This gives the following file structure.

    src
      |- Cradle.hs
      |- Cradle
        |- Parser.hs
        |- Grammar.hs
        |- Generator.hs
        
The reason we have all the code under a `src` directory is because of the common practice of also having a `test` directory at the same level.  This keeps the 'real' code and the test code nicely separated. 

This modularisation will be fine for now but we may need to separate it even further as the size and compexity of the compiler grows.  For now we'll keep it as simple as possible.

With these changes the main file at `src/Cradle.hs` is very simple and straight forward.

~~~ Haskell
module Main
where

import System.Environment
import Cradle.Grammar

main :: IO ()
main = getArgs >>= p . head

parse :: String -> Assign
parse s = Assign id expr
    where (id, expr) = case assign s of 
            Nothing -> error "Invalid assignment"
            Just ((a, b), _) -> (a, b)

-- | Parse and print. Utility and test function for use in @ghci@.
p = putStrLn . show . parse
~~~