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


type MainWidget = HSplitWidget ResultsWidget PagerWidget

app :: App MainWidget
app = App { _initialize  = initSplitView
          , _handleEvent = eventHandler
          , _render      = picForImage . drawWidget }

initSplitView :: Vty -> IO MainWidget
initSplitView vty = do
    inputLines <- readGrepOutput T.getContents
    displayRegion <- displayBounds (outputIface vty)
    let leftPager  = resultsWidget displayRegion inputLines
        rightPager = pagerWidget (T.replicate 100 (T.pack "hello world\n")) displayRegion
    displayRegion <- displayBounds (outputIface vty)
    return (hSplitWidget leftPager rightPager (2 % 3) displayRegion)

readGrepOutput :: Functor f => f Text -> f [(Text, [(Maybe Int, Text)])]
readGrepOutput = fmap (groupByFile . parseGrepOutput . T.lines)

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
    <> handleKey (KChar '\t') [] keyTab
    <> handleKey KUp          [] keyUp
    <> handleKey KDown        [] keyDown
    <> handleKey KEnter       [] keyEnter
    <> handleKey KEsc         [] keyEsc
  where
    keyTab   = zoom widgetState switchFocus
    keyUp    = do zoom (currentLeftWidget  . widgetState) previousLine
                  zoom (currentRightWidget . widgetState) scrollUp
    keyDown  = do zoom (currentLeftWidget  . widgetState) nextLine
                  zoom (currentRightWidget . widgetState) scrollDown
    keyEnter = do zoom widgetState focusRight
                  Just lineNumber <- use (widgetState . leftWidget . currentLineNumber)
                  zoom (currentRightWidget . widgetState) (moveToLine lineNumber)
    keyEsc   = zoom widgetState focusLeft


handleResizeEvent :: EventHandler MainWidget
handleResizeEvent = mkEventHandler $ \event widget -> case event of
    EvResize w h -> Continue (execState (resizeWidget (w, h)) widget)
    _            -> Unchanged
