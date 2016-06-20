{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as S
import           Data.Text.Lazy                     (Text)
import qualified Data.Text.Lazy                     as T
import qualified Data.Text.Lazy.IO                  as T
import           Distribution.PackageDescription.TH
import qualified Graphics.Vty                       as Vty
import           Graphics.Vty.Input.Events          hiding (Event)
import           Graphics.Vty.Picture
import           Pipes                              as P
import           Pipes.Concurrent
import qualified Pipes.Prelude                      as P
import           System.Directory
import           System.Environment                 (getArgs)
import           System.Exit
import           System.IO
import           System.Posix.IO
import           System.Process

import           Vgrep.App                    as App
import           Vgrep.Environment
import           Vgrep.Event
import           Vgrep.Parser
import           Vgrep.System.Grep
import           Vgrep.Text
import           Vgrep.Type
import           Vgrep.Widget                 hiding (handle)
import qualified Vgrep.Widget                 as Widget
import           Vgrep.Widget.HorizontalSplit
import           Vgrep.Widget.Pager
import           Vgrep.Widget.Results


main :: IO ()
main = do
    args <- getArgs
    when ("-V" `elem` args || "--version" `elem` args) (printVersion >> exitSuccess)

    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    cfg <- withConfiguredEditor defaultConfig
    inputFromTerminal <- hIsTerminalDevice stdin
    outputToTerminal  <- hIsTerminalDevice stdout
    case (inputFromTerminal, outputToTerminal) of
        (True,  False)  -> runHeadless (const recursiveGrep)
        (False, False)  -> runHeadless grep
        (False, True)
            | null args -> runGui cfg id
            | otherwise -> runGui cfg grepForApp
        (True,  True)   -> runGui cfg (const recursiveGrep)
  where
    stdinText  = P.stdinLn  >-> P.map T.pack
    stdoutText = P.stdoutLn <-< P.map T.unpack
    runHeadless grepCommand = runEffect (grepCommand stdinText >-> stdoutText)
    runGui cfg  grepCommand = withSpawn unbounded $
      \(evSink, evSource) -> do
        let stdinText' = stdinText >-> P.tee (P.map ReceiveInputEvent >-> toOutput evSink)
        grepThread <- async . runEffect $
            grepCommand stdinText' >-> P.map ReceiveResultEvent
                                   >-> toOutput evSink
        runApp_ app cfg (fromInput evSource)
        cancel grepThread
    printVersion = do
        let version = $(packageVariable (pkgVersion . package))
            name = $(packageVariable (pkgName . package))
        putStrLn (name <> " " <> version)
        putStrLn ""
        putStrLn "grep version: "
        runEffect (grepVersion >-> P.take 1 >-> P.map ("    " <>) >-> stdoutText)


type MainWidget  = HSplitWidget Results Pager
type WidgetState = HSplit Results Pager

data AppState = AppState { _widgetState :: WidgetState
                         , _inputLines  :: Seq Text }

data Event = VtyEvent Vty.Event
           | ReceiveInputEvent  Text
           | ReceiveResultEvent Text


app :: App Event AppState
app = App
    { App.initialize  = initSplitView
    , App.liftEvent   = VtyEvent
    , App.handleEvent = eventHandler
    , App.render      = renderMainWidget }
  where
    initSplitView :: MonadIO m => m AppState
    initSplitView = pure AppState
            { _widgetState = Widget.initialize mainWidget
            , _inputLines  = S.empty }
    renderMainWidget :: Monad m => VgrepT AppState m Vty.Picture
    renderMainWidget = fmap picForImage (zoom widgetState (draw mainWidget))

mainWidget :: MainWidget
mainWidget = hSplitWidget resultsWidget pagerWidget


---------------------------------------------------------------------------
-- Events

eventHandler
    :: MonadIO m
    => Event
    -> AppState
    -> Next (VgrepT AppState m Redraw)
eventHandler = \case
    ReceiveInputEvent line  -> const (handleFeedInput line)
    ReceiveResultEvent line -> const (handleFeedResult line)
    VtyEvent event          -> handleVty event
  where
    handleFeedResult, handleFeedInput
        :: MonadIO m
        => Text
        -> Next (VgrepT AppState m Redraw)
    handleFeedResult line = Continue $ do
        expandedLine <- expandLineForDisplay line
        case parseLine expandedLine of
            Just l  -> zoom results (feedResult l)
            Nothing -> pure Unchanged
    handleFeedInput line = Continue $ do
        expandedLine <- expandLineForDisplay line
        modifying inputLines (|> expandedLine)
        pure Unchanged

handleVty
    :: MonadIO m
    => Vty.Event
    -> AppState
    -> Next (VgrepT AppState m Redraw)
handleVty event = do
    localKeyBindings <- view (widgetState . currentWidget) >>= \case
        Left  _ -> pure resultsKeyBindings
        Right _ -> pure pagerKeyBindings
    (pure . localKeyBindings <> delegateToWidget <> globalEventBindings) event

delegateToWidget
    :: MonadIO m
    => Vty.Event
    -> AppState
    -> Next (VgrepT AppState m Redraw)
delegateToWidget event = fmap (zoom widgetState)
                       . Widget.handle mainWidget event
                       . view widgetState

resultsKeyBindings :: MonadIO m => Vty.Event -> Next (VgrepT AppState m Redraw)
resultsKeyBindings = dispatchMap $ fromList
    [ (EvKey KEnter      [], loadSelectedFileToPager) ]

pagerKeyBindings :: MonadIO m => Vty.Event -> Next (VgrepT AppState m Redraw)
pagerKeyBindings = dispatchMap $ fromList
    []

globalEventBindings
    :: MonadIO m
    => Vty.Event
    -> AppState
    -> Next (VgrepT AppState m Redraw)
globalEventBindings = \case
    EvResize w h         -> const . Continue $ do
        modifyEnvironment (set region (w, h))
        pure Redraw
    EvKey (KChar 'q') [] -> const (Interrupt Halt)
    EvKey (KChar 'e') [] -> invokeEditor
    _otherwise           -> const Skip


loadSelectedFileToPager :: MonadIO m => VgrepT AppState m Redraw
loadSelectedFileToPager = do
    maybeFileName <- uses (results . currentFileName)
                          (fmap T.unpack)
    whenJust maybeFileName $ \fileName -> do
        fileExists <- liftIO (doesFileExist fileName)
        fileContent <- if fileExists
            then liftIO (fmap (S.fromList . T.lines) (T.readFile fileName))
            else use inputLines
        displayContent <- expandForDisplay fileContent
        highlightLineNumbers <- use (results . currentFileResultLineNumbers)
        zoom pager (replaceBufferContents displayContent highlightLineNumbers)
        moveToSelectedLineNumber
        zoom widgetState (splitView FocusRight (1 % 3))

moveToSelectedLineNumber :: Monad m => VgrepT AppState m ()
moveToSelectedLineNumber =
    use (results . currentLineNumber)
    >>= (`whenJust` (void . zoom pager . moveToLine))

whenJust :: (Monoid r, Monad m) => Maybe a -> (a -> m r) -> m r
whenJust item action = maybe (pure mempty) action item

invokeEditor :: MonadIO m => AppState -> Next (VgrepT AppState m Redraw)
invokeEditor state = case views (results . currentFileName) (fmap T.unpack) state of
    Just file -> Interrupt $ Suspend $ \environment -> do
        let configuredEditor = view (config . editor) environment
            lineNumber = views (results . currentLineNumber) (fromMaybe 0) state
        liftIO $ doesFileExist file >>= \case
            True  -> exec configuredEditor ['+' : show lineNumber, file]
            False -> hPutStrLn stderr ("File not found: " ++ show file)
    Nothing -> Skip

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

widgetState :: Lens' AppState WidgetState
widgetState = lens _widgetState (\s ws -> s { _widgetState = ws })

inputLines :: Lens' AppState (Seq Text)
inputLines = lens _inputLines (\s l -> s { _inputLines = l })

results :: Lens' AppState Results
results = widgetState . leftWidget

pager :: Lens' AppState Pager
pager = widgetState . rightWidget
