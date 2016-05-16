{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Vgrep.Widget.Pager (test) where

import Control.Lens
import Data.Foldable
import Data.Text.Lazy.Testable ()
import Test.Case
import Test.QuickCheck.Monadic as Q
import Test.Tasty
import Test.Tasty.QuickCheck   as Q

import Vgrep.Widget.Pager.Testable


test :: TestTree
test = runTestCases "Pager widget"
    [ TestInvariant
        { description = "Scrolling up and down leaves pager invariant"
        , testData = arbitrary `suchThat` (not . atTop)
                               `suchThat` coversScreen
        , testCase = run (void (scroll (-1) >> scroll 1))
        , invariant = id
        }
    , TestInvariant
        { description = "Scrolling right and left leaves pager invariant"
        , testData = arbitrary
        , testCase = run (void (hScroll 1 >> hScroll (-1)))
        , invariant = id
        }
    , TestProperty
        { description = "Position is in sync with number of lines above"
        , testData = arbitrary
        , testCase = do
            amounts :: [Int] <- pick arbitrary
            run (for_ amounts scroll)
        , assertion = const (position ~~ above . to length)
        }
    , TestProperty
        { description = "Position is in sync with number of lines above"
        , testData = arbitrary
        , testCase = do
            amounts :: [Int] <- pick arbitrary
            run (for_ amounts scrollPage)
        , assertion = const (position ~~ above . to length)
        }
    , TestProperty
        { description = "MoveToLine displays the line on screen"
        , testData = arbitrary `suchThat` (not . emptyPager)
        , testCase = do
            numLines <- liftA2 (+) (uses above length) (uses visible length)
            line <- pick ( arbitrary `suchThat` (> 0)
                                     `suchThat` (<= numLines) )
            run (void (moveToLine line))
            pure line
        , assertion = \line -> do
            pos <- use position
            let posOnScreen = line - pos
            height <- view (region . to regionHeight)
            pure $ counterexample
                ("Failed: 0 <= " ++ show posOnScreen ++ " <= " ++ show height)
                (posOnScreen >= 0 .&&. posOnScreen <= height)
        }
    , TestProperty
        { description = "Scrolling stays within bounds"
        , testData = arbitrary `suchThat` coversScreen
        , testCase = do
            amount <- pick (scale (*10) arbitrary)
            run (void (scroll amount))
        , assertion = const $ do
            pos <- use position
            linesVisible <- uses visible length
            height <- view (region . to regionHeight)
            pure (pos >= 0 .&&. linesVisible >= height)
        }
    ]


emptyPager :: (Pager, Environment) -> Bool
emptyPager (pager, _env) = views visible length pager == 0
                        && views above   length pager == 0

coversScreen :: (Pager, Environment) -> Bool
coversScreen (pager, env) = length (view visible pager) >= view (region . _2) env

atTop :: (Pager, Environment) -> Bool
atTop (pager, _env) = view position pager == 0
