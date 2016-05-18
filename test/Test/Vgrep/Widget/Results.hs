{-# LANGUAGE LambdaCase #-}
module Test.Vgrep.Widget.Results (test) where

import Control.Lens
import Test.Case
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Vgrep.Widget.Results.Testable

test :: TestTree
test = runTestCases "Results widget"
    [ TestInvariant
        { description = "Scrolling one line down and up keeps selected line"
        , testData = arbitrary `suchThat` (not . lastLine)
        , testCase = run (nextLine >> prevLine)
        , invariant = selectedLine
        }
    , TestInvariant
        { description = "Scrolling one page down and up keeps selected line"
        , testData = arbitrary
            `suchThat` lastLineOnScreen
            `suchThat` \(results, env) -> linesBelowCurrent (> screenHeight env) results
        , testCase =  run (pageDown >> pageUp)
        , invariant = selectedLine
        }
    ]

selectedLine :: Getter Results (Maybe FileLineReference)
selectedLine = to $ \case
    EmptyResults -> Nothing
    Results _ _ c _ _ -> Just c

linesBelowCurrent :: (Int -> Bool) -> Results -> Bool
linesBelowCurrent p = \case
    EmptyResults        -> p 0
    Results _ _ _ ds es -> p (length ds + length es)

screenHeight :: Environment -> Int
screenHeight = view (region . to regionHeight)

lastLineOnScreen :: (Results, Environment) -> Bool
lastLineOnScreen (results, _env) = case results of
    EmptyResults        -> True
    Results _ _ _ ds _ -> null ds

lastLine :: (Results, Environment) -> Bool
lastLine (results, _env) = case results of
    EmptyResults        -> True
    Results _ _ _ ds es -> null ds && null es
