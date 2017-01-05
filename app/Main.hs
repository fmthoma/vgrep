{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Control.Concurrent.Async
import           Control.Lens.Compat
import           Control.Monad.Reader
import qualified Data.Map.Strict                    as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as S
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as TL
import qualified Data.Text.Lazy.IO                  as TL
import           Distribution.PackageDescription.TH
import qualified Graphics.Vty                       as Vty
import           Graphics.Vty.Input.Events          hiding (Event)
import           Graphics.Vty.Picture
import           Language.Haskell.TH
import           Pipes                              as P
import           Pipes.Concurrent
import qualified Pipes.Prelude                      as P
import           System.Directory
import           System.Environment                 (getArgs)
import           System.Exit
import           System.IO
import           System.Process

import           Vgrep.App                    as App
import           Vgrep.Commands
import           Vgrep.Environment
import           Vgrep.Event
import qualified Vgrep.Keys                   as Key
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
    when ("--help" `elem` args) (printHelp >> exitSuccess)
    when ("--dump-default-config" `elem` args) (printDefaultConfig >> exitSuccess)

    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    cfg <- loadConfig mempty
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
    printHelp = putStrLn helpText
        where helpText = $(runIO (readFile "help.txt") >>= stringE)
    printDefaultConfig = putStrLn defaultConfigFile
        where defaultConfigFile = $(runIO (readFile "config.yaml.example") >>= stringE)


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
    -> Environment
    -> AppState
    -> Next (VgrepT AppState m Redraw)
eventHandler = \case
    ReceiveInputEvent line  -> \_ _ -> handleFeedInput line
    ReceiveResultEvent line -> \_ _ -> handleFeedResult line
    VtyEvent event          -> handleVty event
  where
    handleFeedResult, handleFeedInput
        :: MonadIO m
        => Text
        -> Next (VgrepT AppState m Redraw)
    handleFeedResult line = Continue $ case parseLine line of
        Just l  -> do
            l' <- traverseOf (lineReference . lineText) expandFormattedLine l
            zoom results (feedResult l')
        Nothing -> pure Unchanged
    handleFeedInput line = Continue $ do
        expandedLine <- expandLineForDisplay line
        modifying inputLines (|> expandedLine)
        pure Unchanged

handleVty
    :: MonadIO m
    => Vty.Event
    -> Environment
    -> AppState
    -> Next (VgrepT AppState m Redraw)
handleVty = \case
    EvResize w h                      -> \_ _ -> handleResizeEvent w h
    ev | Just chord <- Key.fromVty ev -> handleKeyEvent chord
       | otherwise                    -> \_ _ -> Skip

handleResizeEvent :: Monad m => Int -> Int -> Next (VgrepT AppState m Redraw)
handleResizeEvent w h = Continue $ do
    modifyEnvironment . set viewport $
        Viewport { _vpWidth = w, _vpHeight = h }
    pure Redraw

handleKeyEvent
    :: MonadIO m
    => Key.Chord
    -> Environment
    -> AppState
    -> Next (VgrepT AppState m Redraw)
handleKeyEvent chord environment state =
    executeCommand (lookupCmd (localBindings <> globalBindings)) state
  where
    globalBindings  = view (config . keybindings . globalKeybindings)  environment
    resultsBindings = view (config . keybindings . resultsKeybindings) environment
    pagerBindings   = view (config . keybindings . pagerKeybindings)   environment
    localBindings = case view (widgetState . currentWidget) state of
        Left  _ -> resultsBindings
        Right _ -> pagerBindings
    lookupCmd = fromMaybe None . M.lookup chord


executeCommand :: MonadIO m => Command -> AppState -> Next (VgrepT AppState m Redraw)
executeCommand = \case
    None               -> skip
    DisplayPagerOnly   -> continue (zoom widgetState rightOnly)
    DisplayResultsOnly -> continue (zoom widgetState leftOnly)
    SplitFocusPager    -> continue (zoom widgetState (splitView FocusRight (1 % 3)))
    SplitFocusResults  -> continue (zoom widgetState (splitView FocusLeft (2 % 3)))
    PagerUp            -> continue (zoom pager (scroll (-1)))
    PagerDown          -> continue (zoom pager (scroll 1))
    PagerPgUp          -> continue (zoom pager (scrollPage (-1)))
    PagerPgDown        -> continue (zoom pager (scrollPage 1))
    PagerScrollLeft    -> continue (zoom pager (hScroll (-1)))
    PagerScrollRight   -> continue (zoom pager (hScroll 1))
    ResultsUp          -> continue (zoom results prevLine >> pure Redraw)
    ResultsDown        -> continue (zoom results nextLine >> pure Redraw)
    ResultsPgUp        -> continue (zoom results pageUp   >> pure Redraw)
    ResultsPgDown      -> continue (zoom results pageDown >> pure Redraw)
    PrevResult         -> continue (zoom results prevLine >> loadSelectedFileToPager)
    NextResult         -> continue (zoom results nextLine >> loadSelectedFileToPager)
    PagerGotoResult    -> continue loadSelectedFileToPager
    OpenFileInEditor   -> invokeEditor
    Exit               -> halt
  where
    continue = const . Continue
    skip = const Skip
    halt = const (Interrupt Halt)

loadSelectedFileToPager :: MonadIO m => VgrepT AppState m Redraw
loadSelectedFileToPager = do
    maybeFileName <- uses (results . currentFileName)
                          (fmap T.unpack)
    whenJust maybeFileName $ \selectedFile -> do
        fileExists <- liftIO (doesFileExist selectedFile)
        fileContent <- if fileExists
            then readLinesFrom selectedFile
            else use inputLines
        displayContent <- expandForDisplay fileContent
        highlightedLines <- use (results . currentFileResults)
        zoom pager (replaceBufferContents displayContent highlightedLines)
        moveToSelectedLineNumber
        zoom widgetState (splitView FocusRight (1 % 3))
  where
    readLinesFrom f = liftIO $ do
        content <- TL.readFile f
        pure (fileLines content)
    fileLines = S.fromList . map TL.toStrict . TL.lines


moveToSelectedLineNumber :: Monad m => VgrepT AppState m ()
moveToSelectedLineNumber =
    use (results . currentLineNumber)
    >>= (`whenJust` (void . zoom pager . moveToLine))

whenJust :: (Monoid r, Monad m) => Maybe a -> (a -> m r) -> m r
whenJust item action = maybe (pure mempty) action item

invokeEditor :: AppState -> Next (VgrepT AppState m Redraw)
invokeEditor state = case views (results . currentFileName) (fmap T.unpack) state of
    Just selectedFile -> Interrupt $ Suspend $ \environment -> do
        let configuredEditor = view (config . editor) environment
            selectedLineNumber = views (results . currentLineNumber) (fromMaybe 0) state
        liftIO $ doesFileExist selectedFile >>= \case
            True  -> exec configuredEditor ['+' : show selectedLineNumber, selectedFile]
            False -> hPutStrLn stderr ("File not found: " ++ show selectedFile)
    Nothing -> Skip

exec :: MonadIO io => FilePath -> [String] -> io ()
exec command args = liftIO $ do
    (_,_,_,h) <- createProcess (proc command args)
    void (waitForProcess h)

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
