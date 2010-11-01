import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Tests.Lbach.Parser.Expressions

main = defaultMain tests

tests = [
        testGroup "Parser" [
                testProperty "number1" numberIsNothingWhenEmpty,
                testProperty "number2" numberIsNothingWhenStartsWithAlpha
            ]
        ]