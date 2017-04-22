{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Control.Concurrent.Async
import           Control.Lens.Compat
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as S
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as TL
import qualified Data.Text.Lazy.IO                  as TL
import           Distribution.PackageDescription.TH
import qualified Graphics.Vty                       as Vty
import           Graphics.Vty.Input.Events          hiding (Event)
import           Language.Haskell.TH
import           Pipes                              as P
import           Pipes.Concurrent
import qualified Pipes.Prelude                      as P
import           System.Directory
import           System.Environment                 (getArgs)
import           System.Exit
import           System.IO
import           System.Process

import           Vgrep.App            (App (App), runApp_)
import qualified Vgrep.App            as App
import           Vgrep.Command
import           Vgrep.Environment
import           Vgrep.Event
import qualified Vgrep.Key            as Key
import qualified Vgrep.KeybindingMap  as KeybindingMap
import           Vgrep.Parser
import           Vgrep.Search
import           Vgrep.System.Grep
import           Vgrep.Text
import           Vgrep.Type
import qualified Vgrep.Widget         as Widget
import           Vgrep.Widget.EdLine
import           Vgrep.Widget.Layout
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


type MainWidget  = LayoutWidget (Layout Results Pager) EdLine
type WidgetState = Layout (Layout Results Pager) EdLine

data AppState = AppState { _widgetState :: WidgetState
                         , _inputLines  :: Seq Text }

data Event = VtyEvent Vty.Event
           | ReceiveInputEvent  Text
           | ReceiveResultEvent Text


app :: App Event AppState
app = App
    { App.initialize    = initSplitView
    , App.liftEvent     = VtyEvent
    , App.handleEvent   = eventHandler
    , App.displayStatus = displayStatus
    , App.render        = renderMainWidget }
  where
    initSplitView :: MonadIO m => m AppState
    initSplitView = pure AppState
            { _widgetState = Widget.initialize mainWidget
            , _inputLines  = S.empty }
    renderMainWidget :: Monad m => VgrepT AppState m Vty.Picture
    renderMainWidget = zoom widgetState $ do
        mainImage <- Widget.draw mainWidget
        mainCursor <- Widget.cursor mainWidget
        pure Vty.Picture
            { Vty.picCursor = toVtyCursor mainCursor
            , Vty.picLayers = [mainImage]
            , Vty.picBackground = Vty.ClearBackground }
    toVtyCursor :: Widget.Cursor -> Vty.Cursor
    toVtyCursor = \case
        Widget.NoCursor       -> Vty.NoCursor
        Widget.Cursor col row -> Vty.Cursor col row

mainWidget :: MainWidget
mainWidget = foo (hSplitWidget resultsWidget pagerWidget) edLineWidget
  where
    foo a b = layoutWidget a b Vertical (FixedSecondary 1) FocusPrimary


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
handleKeyEvent chord environment state = localBindings <> globalBindings
  where
    globalKeymap  = view (config . keybindings . globalKeybindings)  environment
    resultsKeymap = view (config . keybindings . resultsKeybindings) environment
    pagerKeymap   = view (config . keybindings . pagerKeybindings)   environment
    lookupCmd keymap = fromMaybe Unset . (`KeybindingMap.lookup` keymap)
    localBindings = case view (widgetState . focusedWidget) state of
        Left foo -> case view focusedWidget foo of
            Left  _ -> executeCommand (lookupCmd resultsKeymap chord) state
            Right _ -> executeCommand (lookupCmd pagerKeymap chord) state
        Right _ -> edlineBindings chord state
    globalBindings = executeCommand (lookupCmd globalKeymap chord) state

edlineBindings :: MonadIO m => Key.Chord -> AppState -> Next (VgrepT AppState m Redraw)
edlineBindings (Key.Chord mods key) state = case key of
    Key.Char c      | noModifiers -> Continue (zoom edline (insert c))
    Key.Space       | noModifiers -> Continue (zoom edline (insert ' '))
    Key.Backspace   | noModifiers -> Continue (zoom edline backspace)
                    | ctrlPressed -> Continue (zoom edline deletePrevWord)
    Key.Left        | noModifiers -> Continue (zoom edline moveLeft)
                    | ctrlPressed -> Continue (zoom edline moveWordLeft)
    Key.Right       | noModifiers -> Continue (zoom edline moveRight)
                    | ctrlPressed -> Continue (zoom edline moveWordRight)
    Key.Del         | noModifiers -> Continue (zoom edline delete)
                    | ctrlPressed -> Continue (zoom edline deleteWord)
    Key.Home        | noModifiers -> Continue (zoom edline moveHome)
    Key.End         | noModifiers -> Continue (zoom edline moveEnd)
    Key.Esc                       -> executeCommand EdlineLeave state
    Key.Enter       | noModifiers -> case view edline state of
        mode | Just pat <- preview searchText mode -> Continue (startSearch pat)
             | otherwise                           -> Skip
    _otherwise -> Skip
  where
    noModifiers = Set.null mods
    ctrlPressed = Key.Ctrl `Set.member` mods

startSearch :: Monad m => Text -> VgrepT AppState m Redraw
startSearch newSearchRegex = do
    assign (widgetState . focus) FocusPrimary
    modifyEnvironment (set searchRegex (Just (regex newSearchRegex, newSearchRegex)))
    zoom edline reset

executeCommand :: MonadIO m => Command -> AppState -> Next (VgrepT AppState m Redraw)
executeCommand = \case
    Unset              -> skip
    DisplayPagerOnly   -> continue (zoom resultsAndPager secondaryOnly)
    DisplayResultsOnly -> continue (zoom resultsAndPager primaryOnly)
    SplitFocusPager    -> continue splitViewPager
    SplitFocusResults  -> continue splitViewResults
    PagerUp            -> continue (zoom pager (scroll (-1)))
    PagerDown          -> continue (zoom pager (scroll 1))
    PagerPageUp        -> continue (zoom pager (scrollPage (-1)))
    PagerPageDown      -> continue (zoom pager (scrollPage 1))
    PagerHalfPageUp    -> continue (zoom pager (scrollPageFraction (-1%2)))
    PagerHalfPageDown  -> continue (zoom pager (scrollPageFraction (1%2)))
    PagerScrollLeft    -> continue (zoom pager (hScroll (-1)))
    PagerScrollRight   -> continue (zoom pager (hScroll 1))
    ResultsUp          -> continue (zoom results prevLine >> pure Redraw)
    ResultsDown        -> continue (zoom results nextLine >> pure Redraw)
    ResultsPrevMatch   -> continue (zoom results prevMatch >> pure Redraw)
    ResultsNextMatch   -> continue (zoom results nextMatch >> pure Redraw)
    ResultsPageUp      -> continue (zoom results pageUp   >> pure Redraw)
    ResultsPageDown    -> continue (zoom results pageDown >> pure Redraw)
    PrevResult         -> continue (zoom results prevLine >> loadSelectedFileToPager)
    NextResult         -> continue (zoom results nextLine >> loadSelectedFileToPager)
    PagerGotoResult    -> continue (loadSelectedFileToPager >> splitViewPager)
    EdlineEnterSearch  -> continue (zoom widgetState (assign focus FocusSecondary >> zoom secondary enterSearch) >> pure Redraw)
    EdlineLeave        -> continue (zoom widgetState (assign focus FocusPrimary >> zoom secondary reset) >> pure Redraw)
    OpenFileInEditor   -> invokeEditor
    Exit               -> halt
  where
    continue = const . Continue
    skip = const Skip
    halt = const (Interrupt Halt)

splitViewPager, splitViewResults :: Monad m => VgrepT AppState m Redraw
splitViewPager   = zoom resultsAndPager $ do
    void splitView
    assign focus FocusSecondary
    assign splitRatio (Dynamic(1%3))
    pure Redraw
splitViewResults = zoom resultsAndPager $ do
    void splitView
    assign focus FocusPrimary
    assign splitRatio (Dynamic(2%3))
    pure Redraw

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
        pure Redraw
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
exec path args = liftIO $ do
    (_,_,_,h) <- createProcess (proc path args)
    void (waitForProcess h)

displayStatus :: Monad m => Text -> VgrepT AppState m ()
displayStatus msg = void (zoom edline (putStatus msg))


---------------------------------------------------------------------------
-- Lenses

widgetState :: Lens' AppState WidgetState
widgetState = lens _widgetState (\s ws -> s { _widgetState = ws })

inputLines :: Lens' AppState (Seq Text)
inputLines = lens _inputLines (\s l -> s { _inputLines = l })

resultsAndPager :: Lens' AppState (Layout Results Pager)
resultsAndPager = widgetState . primary

results :: Lens' AppState Results
results = resultsAndPager . primary

pager :: Lens' AppState Pager
pager = resultsAndPager . secondary

edline :: Lens' AppState EdLine
edline = widgetState . secondary
