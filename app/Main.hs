module Main (main) where

import Control.Monad.State.Extended
import Control.Lens
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

main :: IO MainWidget
main = runApp app


type MainWidget = HSplitWidget ResultsWidget PagerWidget

app :: App MainWidget
app = App { _initialize  = initSplitView
          , _handleEvent = eventHandler
          , _render      = picForImage . drawWidget }

initSplitView :: Vty -> IO MainWidget
initSplitView vty = do
    inputLines <- readGrepOutput T.getContents
    bounds <- displayBounds (outputIface vty)
    let leftPager  = resultsWidget bounds inputLines
        rightPager = pagerWidget T.empty bounds
    return (hSplitWidget leftPager rightPager (2 % 3) bounds)

readGrepOutput :: Functor f => f Text -> f [FileLineReference]
readGrepOutput = fmap (parseGrepOutput . T.lines)


---------------------------------------------------------------------------
-- Events

eventHandler :: EventHandler MainWidget
eventHandler = mconcat
    [ exitOn (KChar 'q') []
    , handleResizeEvent
    , handleKey   (KChar '\t') [] keyTab
    , handleKey   KUp          [] keyUp
    , handleKey   (KChar 'k')  [] keyUp
    , handleKey   KDown        [] keyDown
    , handleKey   (KChar 'j')  [] keyDown
    , handleKey   KPageUp      [] keyPgUp
    , handleKey   KPageDown    [] keyPgDn
    , handleKeyIO KEnter       [] keyEnter
    , handleKey   KEsc         [] keyEsc ]
  where
    keyTab   = zoom widgetState switchFocus
    keyUp    = do whenS (has resultsFocused)
                        (zoom (results . widgetState) prevLine)
                  whenS (has pagerFocused)
                        (zoom (pager . widgetState) (scroll (-1)))
    keyDown  = do whenS (has resultsFocused)
                        (zoom (results . widgetState) nextLine)
                  whenS (has pagerFocused)
                        (zoom (pager . widgetState) (scroll 1))
    keyPgUp  = whenS (has pagerFocused)
                  (zoom (pager . widgetState) (scrollPage (-1)))
    keyPgDn  = whenS (has pagerFocused)
                  (zoom (pager . widgetState) (scrollPage 1))
    keyEnter = whenS (has resultsFocused) $ do
                  loadSelectedFileToPager
                  liftState moveToSelectedLineNumber
                  liftState (zoom widgetState focusRight)
    keyEsc   = whenS (has pagerFocused)
                  (zoom widgetState focusLeft)

loadSelectedFileToPager :: StateT MainWidget IO ()
loadSelectedFileToPager = zoom widgetState $ do
    fileName <- liftState (use (leftWidget . currentFileName))
    fileContent <- liftIO (T.readFile (T.unpack fileName))
    liftState $ zoom (rightWidget . widgetState)
                     (replaceBufferContents fileContent)

moveToSelectedLineNumber :: State MainWidget ()
moveToSelectedLineNumber = zoom widgetState $ do
    lineNumber <- use (leftWidget . currentLineNumber)
    zoom (rightWidget . widgetState) (moveToLine (maybe 0 id lineNumber))


---------------------------------------------------------------------------
-- Lenses

results :: Lens' MainWidget ResultsWidget
results = widgetState . leftWidget

pager :: Lens' MainWidget PagerWidget
pager = widgetState . rightWidget



resultsFocused :: Traversal' MainWidget ResultsWidget
resultsFocused = leftWidgetFocused

pagerFocused :: Traversal' MainWidget PagerWidget
pagerFocused = rightWidgetFocused
