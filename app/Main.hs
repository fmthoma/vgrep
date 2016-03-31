module Main (main) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Extended
import Data.Foldable
import Data.Maybe
import Data.Ratio
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Graphics.Vty as Vty
import Graphics.Vty.Input.Events hiding (Event)
import Graphics.Vty.Output
import Graphics.Vty.Picture
import Pipes as P
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Posix
import System.Process

import Vgrep.App as App
import Vgrep.Event
import Vgrep.Environment
import Vgrep.Parser
import Vgrep.System.Grep
import Vgrep.Text
import Vgrep.Type
import Vgrep.Widget hiding (handle)
import qualified Vgrep.Widget as Widget
import Vgrep.Widget.HorizontalSplit
import Vgrep.Widget.Pager
import Vgrep.Widget.Results


main :: IO ()
main = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    config <- defaultConfig
    inputFromTerminal <- hIsTerminalDevice stdin
    outputToTerminal  <- hIsTerminalDevice stdout
    args <- getArgs
    case (inputFromTerminal, outputToTerminal) of
        (True,  False)  -> runHeadless (const recursiveGrep)
        (False, False)  -> runHeadless grep
        (False, True)
            | null args -> runGui environment id
            | otherwise -> runGui environment grepForApp
        (True,  True)   -> runGui environment (const recursiveGrep)
  where
    stdinText  = P.stdinLn  >-> P.map T.pack
    stdoutText = P.stdoutLn <-< P.map T.unpack
    runHeadless grepCommand = runEffect (grepCommand stdinText >-> stdoutText)
    runGui environment  grepCommand = withSpawn unbounded $
      \(evSink, evSource) -> do
        let stdinText' = stdinText >-> P.tee (P.map ReceiveInputEvent >-> toOutput evSink)
        grepThread <- async . runEffect $
            grepCommand stdinText' >-> P.map ReceiveResultEvent
                                   >-> toOutput evSink
        runApp_ app environment (fromInput evSource)
        cancel grepThread


type MainWidget  = HSplitWidget ResultsState PagerState
type WidgetState = HSplitState  ResultsState PagerState

data AppState = AppState { _appWidget   :: MainWidget
                         , _widgetState :: WidgetState
                         , _inputLines  :: Seq Text }

data Event = VtyEvent Vty.Event
           | ReceiveInputEvent  Text
           | ReceiveResultEvent Text

vtyEvent :: Prism' Event Vty.Event
vtyEvent = prism VtyEvent $ \case
    VtyEvent e -> Right e
    other      -> Left other

receiveInputEvent :: Prism' Event Text
receiveInputEvent = prism ReceiveInputEvent $ \case
    ReceiveInputEvent e -> Right e
    other               -> Left other

receiveResultEvent :: Prism' Event Text
receiveResultEvent = prism ReceiveResultEvent $ \case
    ReceiveResultEvent e -> Right e
    other                -> Left other


app :: App Event AppState
app = App
    { App.initialize  = initSplitView
    , App.liftEvent   = VtyEvent
    , App.handleEvent = eventHandler
    , App.render      = renderMainWidget }
  where
    initSplitView vty = do
        bounds <- liftIO (displayBounds (Vty.outputIface vty))
        let mainWidget = hSplitWidget resultsWidget pagerWidget
        liftIO . pure $ AppState
            { _appWidget   = mainWidget
            , _widgetState = Widget.initialize mainWidget bounds
            , _inputLines  = S.empty }
    renderMainWidget s =
        let mainWidget      = view appWidget   s
            mainWidgetState = view widgetState s
        in  fmap picForImage (draw mainWidget mainWidgetState)


---------------------------------------------------------------------------
-- Events

eventHandler :: EventHandler Event AppState
eventHandler = mconcat $
    [ liftEventHandler (preview vtyEvent) vtyEventHandler
    , handle (preview receiveInputEvent)  (continueIO . feedInputLine)
    , handle (preview receiveResultEvent) (continueIO . feedResultLine) ]
  where
    feedResultLine, feedInputLine :: Text -> StateT AppState (VgrepT IO) ()
    feedResultLine line = do
        expandedLine <- (lift . expandLineForDisplay) line
        let maybeParsedLine = parseLine expandedLine
        when (isJust maybeParsedLine) $
            zoom (widgetState . leftWidget)
                 (feedResult (fromJust maybeParsedLine))
    feedInputLine line = do
        expandedLine <- (lift . expandLineForDisplay) line
        modifying inputLines (|> expandedLine)

vtyEventHandler :: EventHandler Vty.Event AppState
vtyEventHandler = mconcat
    [ handle (keyCharEvent 'q'   []) (const halt)
    , handle resizeEvent             (continue . resizeMainWidget)
    , handle (keyCharEvent '\t'  []) (const (continue keyTab))
    , handle (keyEvent KUp       []) (const (continue keyUp))
    , handle (keyEvent KDown     []) (const (continue keyDown))
    , handle (keyCharEvent 'k'   []) (const (continue keyUp))
    , handle (keyCharEvent 'j'   []) (const (continue keyDown))
    , handle (keyEvent KPageUp   []) (const (continue keyPgUp))
    , handle (keyEvent KPageDown []) (const (continue keyPgDn))
    , handle (keyEvent KEnter    []) (const (continueIO keyEnter))
    , handle (keyCharEvent 'e'   []) (const (suspend keyEdit))
    , handle (keyEvent KEsc      []) (const (continue keyEsc)) ]
  where
    resizeMainWidget region = do
        mainWidget <- use appWidget
        zoom widgetState (Widget.resize mainWidget region)
    keyTab   = use appWidget >>= zoom widgetState . switchFocus
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
                  liftState $ do
                      moveToSelectedLineNumber
                      use appWidget >>= zoom widgetState . splitFocusRight (1 % 3)
    keyEdit  = zoom results $ do
                  maybeFileName <- uses currentFileName (fmap T.unpack)
                  when (isJust maybeFileName) $ do
                      lineNumber <- uses currentLineNumber (fromMaybe 0)
                      invokeEditor (fromJust maybeFileName) lineNumber
    keyEsc   = whenS (has pagerFocused)
                  (use appWidget >>= zoom widgetState . leftOnly)

loadSelectedFileToPager :: StateT AppState (VgrepT IO) ()
loadSelectedFileToPager = do
    maybeFileName <- uses (results . currentFileName)
                          (fmap T.unpack)
    case maybeFileName of
        Just fileName -> do
            fileExists <- liftIO (doesFileExist fileName)
            fileContent <- if fileExists
                then liftIO (fmap T.lines (T.readFile fileName))
                else uses inputLines toList
            displayContent <- lift (expandForDisplay fileContent)
            liftState $ zoom pager (replaceBufferContents  displayContent)
        Nothing -> pure ()

moveToSelectedLineNumber :: State AppState ()
moveToSelectedLineNumber = do
    lineNumber <- use (results . currentLineNumber)
    zoom pager (moveToLine (fromMaybe 0 lineNumber))

invokeEditor :: FilePath -> Int -> StateT ResultsState (VgrepT IO) ()
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

appWidget :: Lens' AppState MainWidget
appWidget = lens _appWidget (\s w -> s { _appWidget = w })

widgetState :: Lens' AppState WidgetState
widgetState = lens _widgetState (\s ws -> s { _widgetState = ws })

inputLines :: Lens' AppState (Seq Text)
inputLines = lens _inputLines (\s l -> s { _inputLines = l })

results :: Lens' AppState ResultsState
results = widgetState . leftWidget

pager :: Lens' AppState PagerState
pager = widgetState . rightWidget


resultsFocused :: Traversal' AppState ResultsState
resultsFocused = widgetState . leftWidgetFocused

pagerFocused :: Traversal' AppState PagerState
pagerFocused = widgetState . rightWidgetFocused
