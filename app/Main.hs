module Main (main) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Extended
import Data.Maybe
import Data.Ratio
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Graphics.Vty hiding (resize)
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Posix
import System.Process

import Vgrep.App
import Vgrep.Event
import Vgrep.Environment
import Vgrep.Parser
import Vgrep.System.Grep
import Vgrep.Type
import Vgrep.Widget as Widget
import Vgrep.Widget.HorizontalSplit
import Vgrep.Widget.Pager
import Vgrep.Widget.Results


main :: IO ()
main = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    environment <- Env <$> T.getContents
    inputFromTerminal <- hIsTerminalDevice stdin
    outputToTerminal  <- hIsTerminalDevice stdout
    args <- getArgs
    runVgrep environment $ case (inputFromTerminal, outputToTerminal) of
        (True,  False)  -> grepFiles           >>= liftIO . T.putStrLn
        (False, False)  -> view input >>= grep >>= liftIO . T.putStrLn
        (False, True)
            | null args -> view input          >>= runApp_ . app
            | otherwise -> view input >>= grep >>= runApp_ . app
        (True,  True)   -> grepFiles           >>= runApp_ . app

type MainWidget = HSplitWidget ResultsWidget PagerWidget

app :: Text -> App MainWidget
app grepOutput = App
    { _initialize  = initSplitView (parseGrepOutput (T.lines grepOutput))
    , _handleEvent = eventHandler
    , _render      = picForImage . drawWidget }

initSplitView :: [FileLineReference] -> Vty -> Vgrep MainWidget
initSplitView grepOutput vty = liftIO $ do
    bounds <- displayBounds (outputIface vty)
    let leftPager  = resultsWidget bounds grepOutput
        rightPager = pagerWidget T.empty bounds
    return (hSplitWidget leftPager rightPager bounds)


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

loadSelectedFileToPager :: StateT MainWidget Vgrep ()
loadSelectedFileToPager = zoom widgetState $ do
    fileName <- uses (leftWidget . currentFileName) T.unpack
    fileExists <- liftIO (doesFileExist fileName)
    fileContent <- if fileExists
        then liftIO (T.readFile fileName)
        else lift (view input)
    liftState $ zoom (rightWidget . widgetState)
                     (replaceBufferContents fileContent)

moveToSelectedLineNumber :: State MainWidget ()
moveToSelectedLineNumber = zoom widgetState $ do
    lineNumber <- use (leftWidget . currentLineNumber)
    zoom (rightWidget . widgetState) (moveToLine (fromMaybe 0 lineNumber))

invokeEditor :: MonadIO io => FilePath -> Int -> io ()
invokeEditor file lineNumber = liftIO $ do
    fileExists <- doesFileExist file
    maybeEditor <- getEnv "EDITOR"
    if | not fileExists
         -> hPutStrLn stderr ("File not found: " ++ show file)
       | Just editor <- maybeEditor
         -> exec editor ['+' : show lineNumber, file]
       | otherwise
         -> hPutStrLn stderr ("Environment variable $EDITOR not defined")

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
