module Tests.Lbach.Parser.Expressions where

import Test.QuickCheck
import Data.Char
import Lbach.Parser.Expressions

prop_numLeadingAlpha :: String -> Property
prop_numLeadingAlpha x = not(null x) && isAlpha (head x) ==> show (number x) == "Nothing"

prop_numEmpty x = null x ==> show (number x) == "Nothing"