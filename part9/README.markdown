Creating a Haskell Program

The preferred method for creating distributable Haskell packages, whether they are libraries or programs, is with [the Cabal](http://www.haskell.org/cabal/). The Cabal is a small wrapper around the build tools, and allows some standardised dependancy management, license distribution etc.

## Setting up our compiler to use Cabal

Unfortunately the [mkcabal](http://hackage.haskell.org/package/mkcabal) package, which automates the creation of a basic cabal enabled haskell package, won't install under OS X Snow Leopard yet, so we are going to have to do this by hand.  Don't worry - it's not that hard.

Create the following as `lbach.cabal`.

    Name:                lbach
    Version:             0.1
    Description:         Toy compiler
    License:             MIT
    License-file:        LICENSE
    Author:              Geoff Ford
    Maintainer:          g_ford@hotmail.com
    Build-Type:          Simple
    Cabal-Version:       >=1.2
    Executable lbach
      Main-is:           Lbach.hs
      Build-Depends:     base >= 3,haskell98
      hs-source-dirs:    src
      
The paramaters in this file should be pretty self explainatory, with the main ones to keep an eye being the dependencies and the entry file. You'll also need to create a file called `LICENSE` and create `setup.hs` as: 

    import Distribution.Simple
    main = defaultMain

Once this is done, move `lbach.hs` to `src\Lbach.hs` and then we need to modify it so that it will run from the command line. For now, we'll just make it so it tries to parse the string passed in as the command line argument.  Add the following at the top of your file in place of `import Char`:

    module Main
    where
    
    import Char
    import System.Environment
    
    main :: IO ()
    main = getArgs >>= print . parse . head

This simply creates a module called `Main` and a function called `main`, which all Haskell programs must have as their entry point.  Before we test the package, we should test that the program works.

    ~/Projects/compilers/cradle/code> runhaskell src/Lbach.hs "test=1+2 / counter"
    Assign "test" (Add (Num 1) (Div (Num 2) (Var "counter")))
    
Awesome.  Now let's test id `cabal` can package it up and install it for us.  We'll only install it for ourselves just to be on the safe side.

    ~/Projects/compilers/cradle/code> cabal install --prefix=$HOME --user
    Resolving dependencies...
    Configuring lbach-0.1...
    Warning: 'license: MIT' is not a recognised license.
    Preprocessing executables for lbach-0.1...
    Building lbach-0.1...
    Installing executable(s) in /Users/geoff/bin 
    ~/Projects/compilers/cradle/code> ~/bin/lbach "test=1+2 / counter"
    Assign "test" (Add (Num 1) (Div (Num 2) (Var "counter")))
    
## Modularisation



