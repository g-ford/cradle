module Main where 
	
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Lbach.Tests.Parser.Expressions

{-|
Runs all tests.  
The best way to run these is:
@
cabal clean && cabal configure -ftest && cabal build && cabal test
@
-}
main = defaultMain tests

tests = [
        testGroup "Parser" [
                testProperty "number2" prop_numLeadingAlpha
			,	testProperty "number1" prop_numEmpty
            ]
        ]