{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Vgrep.Widget.Pager (test) where

import           Control.Applicative
import           Control.Lens.Compat
import           Control.Monad
import qualified Data.Sequence           as S
import           Data.Text.Testable      ()
import qualified Data.Text.Testable      as T
import           Test.Case
import           Test.QuickCheck         as Q
import           Test.QuickCheck.Monadic as Q

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
    , TestProperty
        { description = "Scrolling n pages at once is the same as scrolling n times one page"
        , testData = arbitrary
        , testCase = do
            n <- pick (arbitrary `suchThat` (/= 0))
            initialState <- get
            run (void (scrollPage n))
            nPagesAtOnce <- get
            put initialState
            replicateM_ (abs n) (run (scrollPage (signum n)))
            nTimesOnePage <- get
            pure (nPagesAtOnce, nTimesOnePage)
        , assertion = \(nPagesAtOnce, nTimesOnePage) ->
            pure (nPagesAtOnce === nTimesOnePage)
        }
    , TestProperty
        { description = "Scrolling by integral page fractions is the same as scrolling entire pages"
        , testData = arbitrary
        , testCase = do
            n <- pick (arbitrary `suchThat` (/= 0))
            initialState <- get
            run (void (scrollPageFraction (fromIntegral n)))
            scrollNFractionalPages <- get
            put initialState
            run (void (scrollPage n))
            scrollNPages <- get
            pure (scrollNFractionalPages, scrollNPages)
        , assertion = \(scrollNFractionalPages, scrollNPages) ->
            pure (scrollNFractionalPages === scrollNPages)
        }
    , TestInvariant
        { description = "Scrolling right and left leaves pager invariant"
        , testData = arbitrary
        , testCase = run (void (hScroll 1 >> hScroll (-1)))
        , invariant = id
        }
    , TestProperty
        { description = "MoveToLine displays the line on screen"
        , testData = arbitrary `suchThat` (not . emptyPager)
        , testCase = do
            numLines <- liftA2 (+) (use (above . to length)) (use (visible . to length))
            line <- pick ( arbitrary `suchThat` (> 0)
                                     `suchThat` (<= numLines) )
            run (void (moveToLine line))
            pure line
        , assertion = \line -> do
            pos <- use position
            let posOnScreen = line - pos
            height <- view viewportHeight
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
            linesVisible <- use (visible . to length)
            height <- view viewportHeight
            pure (pos >= 0 .&&. linesVisible >= height)
        }
    , TestProperty
        { description = "After replaceBufferContents the new content is visible"
        , testData = arbitrary
        , testCase = do
            newContent <- pick (fmap (S.fromList . map T.pack) arbitrary)
            run (replaceBufferContents newContent mempty)
            pure newContent
        , assertion = \expectedContent -> do
            actualContent <- use visible
            pure (actualContent === expectedContent)
        }
    ]


emptyPager :: (Pager, Environment) -> Bool
emptyPager (pager, _env) = view (visible . to length) pager == 0
                        && view (above   . to length) pager == 0

coversScreen :: (Pager, Environment) -> Bool
coversScreen (pager, env) = length (view visible pager) >= view viewportHeight env

atTop :: (Pager, Environment) -> Bool
atTop (pager, _env) = view position pager == 0
