module Main (main) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Extended
import Data.Maybe
import Data.Ratio
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Graphics.Vty hiding (resize)
import Pipes as P
import qualified Pipes.Prelude as P
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
import Vgrep.Text
import Vgrep.Type
import Vgrep.Widget as Widget
import Vgrep.Widget.HorizontalSplit
import Vgrep.Widget.Pager
import Vgrep.Widget.Results


main :: IO ()
main = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    environment <- Env <$> defaultConfig
    inputFromTerminal <- hIsTerminalDevice stdin
    outputToTerminal  <- hIsTerminalDevice stdout
    args <- getArgs
    case (inputFromTerminal, outputToTerminal) of
        (True,  False)  -> runEffect (recursiveGrep  >-> stdoutText)
        (False, False)  -> runEffect (grep stdinText >-> stdoutText)
        (False, True)
            | null args -> runApp_ app environment stdinText
            | otherwise -> runApp_ app environment (grepForApp stdinText)
        (True,  True)   -> runApp_ app environment recursiveGrep
  where
    stdinText  = P.stdinLn  >-> P.map T.pack
    stdoutText = P.stdoutLn <-< P.map T.unpack



type MainWidget = HSplitWidget ResultsWidget PagerWidget

data AppState = AppState { _appWidget  :: MainWidget
                         , _inputLines :: Seq Text }

appWidget :: Lens' AppState MainWidget
appWidget = lens _appWidget (\s w -> s { _appWidget = w })

app :: App AppState
app = App
    { initialize   = initSplitView
    , handleEvent  = eventHandler
    , render       = fmap picForImage . drawWidget . view appWidget }
  where
    initSplitView vty = do
        bounds <- liftIO (displayBounds (outputIface vty))
        let leftPager  = resultsWidget bounds
            rightPager = pagerWidget T.empty bounds
        liftIO . pure $ AppState
            { _appWidget  = hSplitWidget leftPager rightPager bounds
            , _inputLines = S.empty }


---------------------------------------------------------------------------
-- Events

eventHandler :: EventHandler AppState
eventHandler = mconcat
    [ handle (keyCharEvent 'q'   []) halt
    , handleResize                   (zoom appWidget . resizeWidget)
    , handleReceiveLine              feedLine
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
    feedLine line = do
        expandedLine <- (lift . expandLineForDisplay) line
        let maybeParsedLine = parseLine expandedLine
        when (isJust maybeParsedLine) $
            zoom (mainWidgetState . leftWidget)
                 (feedResult (fromJust maybeParsedLine))
    keyTab   = zoom mainWidgetState switchFocus
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
                  liftState (zoom mainWidgetState (splitFocusRight (1 % 3)))
    keyEdit  = zoom (mainWidgetState . leftWidget) $ do
                  maybeFileName <- uses currentFileName (fmap T.unpack)
                  when (isJust maybeFileName) $ do
                      lineNumber <- uses currentLineNumber (fromMaybe 0)
                      invokeEditor (fromJust maybeFileName) lineNumber
    keyEsc   = whenS (has pagerFocused)
                  (zoom mainWidgetState leftOnly)

loadSelectedFileToPager :: StateT AppState (VgrepT IO) ()
loadSelectedFileToPager = zoom mainWidgetState $ do
    maybeFileName <- uses (leftWidget . currentFileName) (fmap T.unpack)
    case maybeFileName of
        Just fileName -> do
            fileExists <- liftIO (doesFileExist fileName)
            fileContent <- if fileExists
                then liftIO (T.readFile fileName)
                else lift (pure (T.pack "FIXME")) -- FIXME
            displayContent <- lift (expandForDisplay (T.lines fileContent))
            liftState $ zoom (rightWidget . widgetState)
                             (replaceBufferContents  displayContent)
        Nothing -> pure ()

moveToSelectedLineNumber :: State AppState ()
moveToSelectedLineNumber = zoom mainWidgetState $ do
    lineNumber <- use (leftWidget . currentLineNumber)
    zoom (rightWidget . widgetState) (moveToLine (fromMaybe 0 lineNumber))

invokeEditor :: FilePath -> Int -> StateT ResultsWidget (VgrepT IO) ()
invokeEditor file lineNumber = do
    configuredEditor <- view (config . editor)
    liftIO $ doesFileExist file >>= \case
        True  -> exec configuredEditor ['+' : show lineNumber, file]
        False -> hPutStrLn stderr ("File not found: " ++ show file)

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

mainWidgetState :: Lens' AppState (HSplitState ResultsWidget PagerWidget)
mainWidgetState = appWidget . widgetState

results :: Lens' AppState ResultsState
results = appWidget . widgetState . leftWidget . widgetState

pager :: Lens' AppState PagerState
pager = appWidget . widgetState . rightWidget . widgetState


resultsFocused :: Traversal' AppState ResultsWidget
resultsFocused = appWidget . leftWidgetFocused

pagerFocused :: Traversal' AppState PagerWidget
pagerFocused = appWidget . rightWidgetFocused
