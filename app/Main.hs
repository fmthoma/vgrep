{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.State.Extended
import Control.Lens
import Data.Maybe
import Data.Ratio
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Graphics.Vty hiding (resize)
import System.IO
import System.Exit
import System.Posix
import System.Process

import Vgrep.App
import Vgrep.Event
import Vgrep.Parser
import Vgrep.Widget as Widget
import Vgrep.Widget.HorizontalSplit
import Vgrep.Widget.Pager
import Vgrep.Widget.Results

main :: IO ()
main = do
    inputFromTerminal <- hIsTerminalDevice stdin
    outputToTerminal <- hIsTerminalDevice stdout
    case (inputFromTerminal, outputToTerminal) of
        (True,  False) -> dieNoStdin
        (True,  True)  -> printUsage >> dieNoStdin
        (False, False) -> interact id
        (False, True)  -> runApp_ app

printUsage :: IO ()
printUsage = putStrLn $ unlines
    [ "Usage:"
    , "    grep -rn PATTERN | vgrep"
    , "    grep -rn PATTERN | vgrep | COMMAND" ]

dieNoStdin :: IO ()
dieNoStdin = die "No stdin connected. Aborting."

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
    return (hSplitWidget leftPager rightPager bounds)

readGrepOutput :: Functor f => f Text -> f [FileLineReference]
readGrepOutput = fmap (parseGrepOutput . T.lines)


---------------------------------------------------------------------------
-- Events

eventHandler :: EventHandler MainWidget
eventHandler = mconcat
    [ handle (keyCharEvent 'q'   []) halt
    , handleResizeEvent
    , handle (keyCharEvent '\t'  []) (continue keyTab)
    , handle (keyEvent KUp       []) (continue keyUp)
    , handle (keyEvent KDown     []) (continue keyDown)
    , handle (keyCharEvent 'k'   []) (continue keyUp)
    , handle (keyCharEvent 'j'   []) (continue keyDown)
    , handle (keyEvent KPageUp   []) (continue keyPgUp)
    , handle (keyEvent KPageDown []) (continue keyPgDn)
    , handle (keyEvent KEnter    []) (continueIO keyEnter)
    , handle (keyCharEvent 'e'   []) (suspend keyEdit)
    , handle (keyEvent KEsc      []) (continue keyEsc) ]
  where
    keyTab   = zoom widgetState switchFocus
    keyUp    = do whenS (has resultsFocused) (zoom results prevLine)
                  whenS (has pagerFocused)   (zoom pager   (scroll (-1)))
    keyDown  = do whenS (has resultsFocused) (zoom results nextLine)
                  whenS (has pagerFocused)   (zoom pager   (scroll 1))
    keyPgUp  = do whenS (has resultsFocused) (zoom results pageUp)
                  whenS (has pagerFocused)   (zoom pager   (scrollPage (-1)))
    keyPgDn  = do whenS (has resultsFocused) (zoom results pageDown)
                  whenS (has pagerFocused)   (zoom pager   (scrollPage 1))
    keyEnter = whenS (has resultsFocused) $ do
                  loadSelectedFileToPager
                  liftState moveToSelectedLineNumber
                  liftState (zoom widgetState (splitFocusRight (1 % 3)))
    keyEdit  = zoom (widgetState . leftWidget) $ do
                  fileName <- uses currentFileName T.unpack
                  lineNumber <- uses currentLineNumber (fromMaybe 0)
                  invokeEditor fileName lineNumber
    keyEsc   = whenS (has pagerFocused)
                  (zoom widgetState leftOnly)

loadSelectedFileToPager :: StateT MainWidget IO ()
loadSelectedFileToPager = zoom widgetState $ do
    fileName <- use (leftWidget . currentFileName)
    fileContent <- liftIO (T.readFile (T.unpack fileName))
    liftState $ zoom (rightWidget . widgetState)
                     (replaceBufferContents fileContent)

moveToSelectedLineNumber :: State MainWidget ()
moveToSelectedLineNumber = zoom widgetState $ do
    lineNumber <- use (leftWidget . currentLineNumber)
    zoom (rightWidget . widgetState) (moveToLine (fromMaybe 0 lineNumber))

invokeEditor :: MonadIO io => FilePath -> Int -> io ()
invokeEditor file lineNumber = liftIO $ do
    maybeEditor <- getEnv "EDITOR"
    case maybeEditor of
            Just editor -> exec editor ['+' : show lineNumber, file]
            Nothing -> error "Environment variable $EDITOR not defined"

exec :: MonadIO io => FilePath -> [String] -> io ()
exec command args = liftIO $ do
    inHandle  <- fdToHandle =<< ttyIn
    outHandle <- fdToHandle =<< ttyOut
    (_,_,_,h) <- createProcess $ (proc command args)
        { std_in  = UseHandle inHandle
        , std_out = UseHandle outHandle }
    _ <- waitForProcess h
    return ()

---------------------------------------------------------------------------
-- Lenses

results :: Lens' MainWidget ResultsState
results = widgetState . leftWidget . widgetState

pager :: Lens' MainWidget PagerState
pager = widgetState . rightWidget . widgetState



resultsFocused :: Traversal' MainWidget ResultsWidget
resultsFocused = leftWidgetFocused

pagerFocused :: Traversal' MainWidget PagerWidget
pagerFocused = rightWidgetFocused
