{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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
test = testGroup "Pager widget"
    [ runTestCase TestInvariant
        { description = "Scrolling up and down leaves pager invariant"
        , testData = arbitrary `suchThat` (not . emptyPager)
                               `suchThat` coversScreen 
        , testCase = run (void (scroll (-1) >> scroll 1))
        , invariant = id }

    , runTestCase TestInvariant
        { description = "Scrolling right and left leaves pager invariant"
        , testData = arbitrary
        , testCase = run (void (hScroll 1 >> hScroll (-1)))
        , invariant = id }

    , runTestCase TestProperty
        { description = "Position is in sync with number of lines above"
        , testData = arbitrary
        , testCase = do
            amounts :: [Int] <- pick arbitrary
            run (for_ amounts scroll)
        , assertion = position ~~ above . to length }

    , runTestCase TestProperty
        { description = "Position is in sync with number of lines above"
        , testData = arbitrary
        , testCase = do
            amounts :: [Int] <- pick arbitrary
            run (for_ amounts scrollPage)
        , assertion = position ~~ above . to length }
    ]


emptyPager :: (Pager, Environment) -> Bool
emptyPager (pager, _env) = view position pager == 0


coversScreen :: (Pager, Environment) -> Bool
coversScreen (pager, env) = length (view visible pager) >= view (region . _2) env
