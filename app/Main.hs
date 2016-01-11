{-# LANGUAGE DisambiguateRecordFields #-}
module Main where

import Control.Monad.State
import Control.Lens
import Data.Monoid
import Data.Ratio
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Graphics.Vty hiding (resize)

import Vgrep.App
import Vgrep.Event
import Vgrep.Parser
import Vgrep.Widget as Widget
import Vgrep.Widget.HorizontalSplit
import Vgrep.Widget.Pager
import Vgrep.Widget.Results

main :: IO ()
main = do
    _finalState <- runApp app
    return ()


type MainWidget = HSplitWidget PagerWidget ResultsWidget

app :: App MainWidget
app = App { _initialize  = initSplitView
          , _handleEvent = eventHandler
          , _render      = picForImage . drawWidget }

initSplitView :: Vty -> IO MainWidget
initSplitView vty = do
    inputLines <- readFromStdIn
    displayRegion <- displayBounds (outputIface vty)
    let leftPager  = pagerWidget (replicate 100 "hello world") displayRegion
        rightPager = resultsWidget displayRegion inputLines
    displayRegion <- displayBounds (outputIface vty)
    return (hSplitWidget leftPager rightPager (1 % 3) displayRegion)

readFromStdIn :: IO [(String, [(Int, String)])]
readFromStdIn = fmap (foo . groupByFile . parseGrepOutput . T.lines)
                     T.getContents
  where
    -- TODO rewrite widgets in terms of Text
    foo :: [(Text, [(Maybe Int, Text)])] -> [(String, [(Int, String)])]
    foo = over (traverse . _1) T.unpack
        . over (traverse . _2 . traverse . _1) (maybe 0 id)
        . over (traverse . _2 . traverse . _2) T.unpack

-- | Assumption: Input is already grouped by file. Without this assumption
-- we would have to strictly traverse the entire input.
groupByFile :: [(Text, Maybe Int, Text)] -> [(Text, [(Maybe Int, Text)])]
groupByFile [] = []
groupByFile input =
    let (file, _, _) = head input
        (resultsInSameFile, rest) = span (\(f, _, _) -> f == file) input
    in  (file, map (\(a,b,c) -> (b,c)) resultsInSameFile) : groupByFile rest


eventHandler :: EventHandler MainWidget
eventHandler = exitOn (KChar 'q') []
            <> handleResizeEvent
            <> passEventsToWidget

handleResizeEvent :: EventHandler MainWidget
handleResizeEvent = EventHandler $ \event widget -> case event of
    EvResize w h -> Continue (execState (resizeWidget (w, h)) widget)
    _            -> Unchanged
