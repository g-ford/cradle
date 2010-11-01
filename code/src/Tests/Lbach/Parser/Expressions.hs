module Tests.Lbach.Parser.Expressions where

import Test.QuickCheck
import Data.Char
import Lbach.Parser.Expressions

numberIsNothingWhenStartsWithAlpha :: String -> Property
numberIsNothingWhenStartsWithAlpha x = not(null x) && isAlpha (head x) ==> show (number x) == "Nothing"

numberIsNothingWhenEmpty x = null x ==> show (number x) == "Nothing"