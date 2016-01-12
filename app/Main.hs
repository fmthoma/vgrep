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

readGrepOutput :: Functor f => f Text -> f [(Text, [(Maybe Int, Text)])]
readGrepOutput = fmap (groupByFile . parseGrepOutput . T.lines)

-- | Input should already be grouped by file. Without this assumption
-- we would have to strictly traverse the entire input.
groupByFile :: [(Text, Maybe Int, Text)] -> [(Text, [(Maybe Int, Text)])]
groupByFile [] = []
groupByFile input =
    let (file, _, _) = head input
        (resultsInSameFile, rest) = span (\(f, _, _) -> f == file) input
    in  (file, map (\(_a,b,c) -> (b,c)) resultsInSameFile) : groupByFile rest


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
    , handleKeyIO KEnter       [] keyEnter
    , handleKey   KEsc         [] keyEsc ]
  where
    keyTab   = zoom widgetState switchFocus
    keyUp    = do modifyWhen (has resultsFocused)
                             (zoom (results . widgetState) previousLine)
                  modifyWhen (has pagerFocused)
                             (zoom (pager . widgetState) (scroll (-1)))
    keyDown  = do modifyWhen (has resultsFocused)
                             (zoom (results . widgetState) nextLine)
                  modifyWhen (has pagerFocused)
                             (zoom (pager . widgetState) (scroll 1))
    keyEnter = modifyWhen (has resultsFocused) $ do
                  loadSelectedFileToPager
                  liftState moveToSelectedLineNumber
                  liftState (zoom widgetState focusRight)
    keyEsc   = modifyWhen (has pagerFocused)
                  (zoom widgetState focusLeft)

loadSelectedFileToPager :: StateT MainWidget IO ()
loadSelectedFileToPager = zoom widgetState $ do
    Just fileName <- liftState (use (leftWidget . currentFileName))
    fileContent <- liftIO (T.readFile (T.unpack fileName))
    liftState $ zoom (rightWidget . widgetState)
                     (replaceBufferContents fileContent)

moveToSelectedLineNumber :: State MainWidget ()
moveToSelectedLineNumber = zoom widgetState $ do
    lineNumber <- use (leftWidget . currentLineNumber)
    zoom (rightWidget . widgetState) (moveToLine (maybe 0 id lineNumber))

modifyWhen :: MonadState s m => (s -> Bool) -> m () -> m ()
modifyWhen predicate action = do
    condition <- fmap predicate get
    when condition action


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
