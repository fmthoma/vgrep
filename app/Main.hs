{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Extended
import Data.Foldable
import Data.Maybe
import Data.Monoid
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
import System.Posix.IO
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
            | null args -> runGui config id
            | otherwise -> runGui config grepForApp
        (True,  True)   -> runGui config (const recursiveGrep)
  where
    stdinText  = P.stdinLn  >-> P.map T.pack
    stdoutText = P.stdoutLn <-< P.map T.unpack
    runHeadless grepCommand = runEffect (grepCommand stdinText >-> stdoutText)
    runGui config  grepCommand = withSpawn unbounded $
      \(evSink, evSource) -> do
        let stdinText' = stdinText >-> P.tee (P.map ReceiveInputEvent >-> toOutput evSink)
        grepThread <- async . runEffect $
            grepCommand stdinText' >-> P.map ReceiveResultEvent
                                   >-> toOutput evSink
        runApp_ app config (fromInput evSource)
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
    initSplitView :: MonadIO m => VgrepT m AppState
    initSplitView = do
        let mainWidget = hSplitWidget resultsWidget pagerWidget
        liftIO . pure $ AppState
            { _appWidget   = mainWidget
            , _widgetState = Widget.initialize mainWidget
            , _inputLines  = S.empty }
    renderMainWidget s =
        let mainWidget      = view appWidget   s
            mainWidgetState = view widgetState s
        in  fmap picForImage (draw mainWidget mainWidgetState)


---------------------------------------------------------------------------
-- Events

eventHandler :: MonadIO m => Event -> StateT AppState (VgrepT m) Next
eventHandler = \case
    ReceiveInputEvent line  -> handleFeedInput line
    ReceiveResultEvent line -> handleFeedResult line
    VtyEvent event          -> handleVty event
  where
    handleFeedResult, handleFeedInput :: MonadIO m
                                      => Text
                                      -> StateT AppState (VgrepT m) Next
    handleFeedResult line = do
        expandedLine <- (lift . expandLineForDisplay) line
        fmap Continue $ case parseLine expandedLine of
            Just line -> zoom (widgetState . leftWidget) (feedResult line)
            Nothing   -> pure Unchanged
    handleFeedInput line = do
        expandedLine <- (lift . expandLineForDisplay) line
        modifying inputLines (|> expandedLine)
        pure (Continue Unchanged)

delegateToWidget :: MonadIO m => Vty.Event -> StateT AppState (VgrepT m) Next
delegateToWidget event = do
    widget <- use appWidget
    redraw <- zoom widgetState (Widget.handle widget event)
    pure (Continue redraw)


handleVty :: MonadIO m
          => Vty.Event
          -> StateT AppState (VgrepT m) Next
handleVty = \case
    EvResize w h -> do lift (modifyEnvironment (set region (w, h)))
                    -- FIXME resizeToRegion
                       pure (Continue Redraw)
    EvKey (KChar 'q') [] -> pure (Interrupt Halt) -- FIXME this shadows other bindings!
    otherEvent -> delegateToWidget otherEvent


--
--vtyEventHandler :: MonadIO m => Vty.Event -> NextT (StateT AppState (VgrepT m)) Redraw
--vtyEventHandler = mconcat
--    [ handle (keyCharEvent 'q'   []) (const (interrupt Halt))
--    , handle resizeEvent             resizeMainWidget
--    , handle (keyCharEvent '\t'  []) (const (continue keyTab))
--    , handle (keyEvent KUp       []) (const (continue keyUp))
--    , handle (keyEvent KDown     []) (const (continue keyDown))
--    , handle (keyCharEvent 'k'   []) (const (continue keyUp))
--    , handle (keyCharEvent 'j'   []) (const (continue keyDown))
--    , handle (keyEvent KPageUp   []) (const (continue keyPgUp))
--    , handle (keyEvent KPageDown []) (const (continue keyPgDn))
--    , handle (keyEvent KEnter    []) (const (continue keyEnter))
--    , handle (keyCharEvent 'e'   []) (const (interrupt (Suspend keyEdit)))
--    , handle (keyEvent KEsc      []) (const (continue keyEsc)) ]
--  where
--    handleWidget e = use appWidget >>= \w -> zoom widgetState (Widget.handle w e)
--    resizeMainWidget newRegion = continue $ do
--        lift (modifyEnvironment (set region newRegion))
--        pure Unchanged
--    keyTab   = handleWidget SwitchFocus
--    keyUp    = handleWidget (FocusedWidgetEvent PrevLine (Scroll (-1)))
--    keyDown  = handleWidget (FocusedWidgetEvent NextLine (Scroll 1))
--    keyPgUp  = handleWidget (FocusedWidgetEvent PageUp   (ScrollPage (-1)))
--    keyPgDn  = handleWidget (FocusedWidgetEvent PageDown (ScrollPage 1))
--    keyEnter = whenS (has resultsFocused) $ do
--                  loadSelectedFileToPager
--                  liftState $ do
--                      moveToSelectedLineNumber
--                      use appWidget >>= zoom widgetState . _splitFocusRight (1 % 3)
--    keyEdit  = zoom results $ do
--                  maybeFileName <- uses currentFileName (fmap T.unpack)
--                  when (isJust maybeFileName) $ do
--                      lineNumber <- uses currentLineNumber (fromMaybe 0)
--                      invokeEditor (fromJust maybeFileName) lineNumber
--    keyEsc   = handleWidget LeftWidget


--loadSelectedFileToPager :: StateT AppState (VgrepT IO) ()
--loadSelectedFileToPager = do
--    maybeFileName <- uses (results . currentFileName)
--                          (fmap T.unpack)
--    case maybeFileName of
--        Just fileName -> do
--            fileExists <- liftIO (doesFileExist fileName)
--            fileContent <- if fileExists
--                then liftIO (fmap T.lines (T.readFile fileName))
--                else uses inputLines toList
--            displayContent <- lift (expandForDisplay fileContent)
--            liftState $ zoom pager (replaceBufferContents  displayContent)
--        Nothing -> pure ()

--moveToSelectedLineNumber :: Monad m => StateT AppState m Redraw
--moveToSelectedLineNumber = do
--    lineNumber <- use (results . currentLineNumber)
--    widget <- use appWidget
--    zoom widgetState (Widget.handle widget (RightEvent (MoveToLine (fromMaybe 0 lineNumber))))

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
