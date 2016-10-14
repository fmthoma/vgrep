{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Test.Vgrep.Widget.Results (test) where

import           Control.Lens            (Getter, _1, over, to, view, views)
import           Data.Map.Strict         ((!))
import qualified Data.Map.Strict         as Map
import           Data.Monoid             ((<>))
import           Data.Sequence           (Seq, ViewR (..))
import qualified Data.Sequence           as Seq
import           Test.Case
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

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
        , testData = fmap moveToLastLineOnScreen arbitrary
            `suchThat` \(results, env) -> linesBelowCurrent (> screenHeight env) results
        , testCase = run (pageDown >> pageUp)
        , invariant = selectedLine
        }
    , TestProperty
        { description = "Scrolling one page down jumps to end of screen"
        , testData = arbitrary
        , testCase = run pageDown
        , assertion = const $ get >>= pure . \case
            EmptyResults       -> True
            Results _ _ _ ds _ -> null ds
        }
    , TestProperty
        { description = "Scrolling one page up jumps to start of screen"
        , testData = arbitrary
        , testCase = run pageUp
        , assertion = const $ get >>= pure . \case
            EmptyResults       -> True
            Results _ bs _ _ _ -> null bs
        }
    , TestProperty
        { description = "Number of lines on screen is bounded by screen height after resizing"
        , testData = arbitrary
        , testCase = run resizeToWindow
        , assertion = const assertWidgetFitsOnScreen
        }
    , TestProperty
        { description = "Number of lines on screen is bounded by screen height after each action"
        , testData = arbitrary
        , testCase = do
            run (void resizeToWindow)
                -- ^ Precondition: widget is resized to display height
            run =<< arbitraryAction
        , assertion = const assertWidgetFitsOnScreen
        }
    , TestInvariant
        { description = "Results do not change order"
        , testData = arbitrary
        , testCase = run =<< arbitraryAction
        , invariant = resultsAsList
        }
    ]

selectedLine :: Getter Results (Maybe FileLineReference)
selectedLine = to $ \case
    EmptyResults      -> Nothing
    Results _ _ c _ _ -> Just c

linesBelowCurrent :: (Int -> Bool) -> Results -> Bool
linesBelowCurrent p = \case
    EmptyResults        -> p 0
    Results _ _ _ ds es -> p (length ds + length es)

screenHeight :: Environment -> Int
screenHeight = view (region . to regionHeight)

moveToLastLineOnScreen :: (Results, Environment) -> (Results, Environment)
moveToLastLineOnScreen = over _1 $ \case
    EmptyResults          -> EmptyResults
    Results as bs c ds es -> case Seq.viewr ds of
        EmptyR   -> Results as bs c ds es
        ds' :> d -> Results as (Seq.reverse ds' <> pure c <> bs) d Seq.empty es

lastLine :: (Results, Environment) -> Bool
lastLine (results, _env) = case results of
    EmptyResults        -> True
    Results _ _ _ ds es -> null ds && null es

resultsAsList :: Getter Results (Seq FileLineReference)
resultsAsList = to $ \case
    EmptyResults -> mempty
    Results as bs c ds es -> mconcat
        [ Seq.reverse as, Seq.reverse bs, pure c, ds, es ]

arbitraryAction ::  Monad m => PropertyM m (Vgrep Results ())
arbitraryAction = do
    let actions = Map.fromList
            [ ("pageUp",   pageUp)
            , ("pageDown", pageDown)
            , ("prevLine", prevLine)
            , ("nextLine", nextLine) ]
    actionName <- pick (elements ["pageUp", "pageDown", "prevLine", "nextLine"])
    pure (actions ! actionName)

assertWidgetFitsOnScreen
    :: (MonadState Results m, MonadReader Environment m)
    => m Property
assertWidgetFitsOnScreen = do
    height <- views region regionHeight
    linesOnScreen <- numberOfLinesOnScreen
    pure $ counterexample
        (show linesOnScreen ++ " > " ++ show height)
        (linesOnScreen <= height)

numberOfLinesOnScreen :: MonadState Results m => m Int
numberOfLinesOnScreen = get >>= pure . \case
    EmptyResults        -> 0
    Results _ bs c ds _ -> length (mconcat [bs, pure c, ds])
