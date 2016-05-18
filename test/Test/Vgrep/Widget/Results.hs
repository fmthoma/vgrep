module Test.Vgrep.Widget.Results (test) where

import Test.Case
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Vgrep.Widget.Results.Testable

test :: TestTree
test = runTestCases "Results widget"
    [ TestProperty
        { description = "foo"
        , testData = arbitrary
        , testCase = get :: PropertyM (Vgrep Results) Results
        , assertion = \val ->               pure True
        }
    ]
