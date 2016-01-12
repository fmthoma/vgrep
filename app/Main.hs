{-# LANGUAGE DisambiguateRecordFields #-}
module Main where

import Control.Monad.State
import Control.Monad.State.Lift
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
        rightPager = pagerWidget T.empty displayRegion
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
    <> handleKey   (KChar '\t') [] keyTab
    <> handleKey   KUp          [] keyUp
    <> handleKey   KDown        [] keyDown
    <> handleKeyIO KEnter       [] keyEnter
    <> handleKey   KEsc         [] keyEsc
  where
    keyTab   = zoom widgetState switchFocus
    keyUp    = do zoom (leftWidgetFocused  . widgetState) previousLine
                  zoom (rightWidgetFocused . widgetState) scrollUp
    keyDown  = do zoom (leftWidgetFocused  . widgetState) nextLine
                  zoom (rightWidgetFocused . widgetState) scrollDown
    keyEnter = do loadSelectedFileToPager
                  liftState moveToSelectedLineNumber
                  liftState (zoom widgetState focusRight)
    keyEsc   = zoom widgetState focusLeft

loadSelectedFileToPager :: StateT MainWidget IO ()
loadSelectedFileToPager = zoom widgetState $ do
    Just fileName <- liftState (use (leftWidget . currentFileName))
    fileContent <- liftIO (T.readFile (T.unpack fileName))
    liftState $ zoom (rightWidget . widgetState)
                     (replaceBufferContents fileContent)

moveToSelectedLineNumber :: State MainWidget ()
moveToSelectedLineNumber = zoom widgetState $ do
    Just lineNumber <- use (leftWidget . currentLineNumber)
    zoom (rightWidget . widgetState) (moveToLine lineNumber)


handleResizeEvent :: EventHandler MainWidget
handleResizeEvent = mkEventHandler $ \event widget -> case event of
    EvResize w h -> Continue (execState (resizeWidget (w, h)) widget)
    _            -> Unchanged
