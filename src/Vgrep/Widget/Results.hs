{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE OverloadedStrings #-}

module Vgrep.Widget.Results
    ( ResultsState()
    , ResultsWidget
    , resultsWidget

    , feedResult
    , prevLine
    , nextLine
    , pageUp
    , pageDown

    , currentFileName
    , currentLineNumber
    , currentFileResultLineNumbers

    , module Vgrep.Results
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Extended
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Graphics.Vty.Image hiding ((<|>))
import Graphics.Vty.Input
import Graphics.Vty.Prelude
import Prelude

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Results
import Vgrep.Results.Buffer as Buffer
import Vgrep.Type
import Vgrep.Widget.Type


type ResultsState = Buffer

type ResultsWidget = Widget ResultsState

resultsWidget :: ResultsWidget
resultsWidget =
    Widget { initialize = initResults
           , draw       = renderResultList
           , handle     = fmap const resultsKeyBindings }

initResults :: ResultsState
initResults = emptyBuffer


resultsKeyBindings
    :: Monad m
    => Event
    -> Next (VgrepT ResultsState m Redraw)
resultsKeyBindings = dispatchMap $ fromList
    [ (EvKey KPageUp     [], pageUp   >> pure Redraw)
    , (EvKey KPageDown   [], pageDown >> pure Redraw)
    , (EvKey KPageUp     [], pageUp   >> pure Redraw)
    , (EvKey KPageDown   [], pageDown >> pure Redraw)
    , (EvKey KUp         [], prevLine >> pure Redraw)
    , (EvKey KDown       [], nextLine >> pure Redraw)
    , (EvKey (KChar 'k') [], prevLine >> pure Redraw)
    , (EvKey (KChar 'j') [], nextLine >> pure Redraw) ]

feedResult :: Monad m => FileLineReference -> VgrepT ResultsState m Redraw
feedResult line = do
    modify (feed line)
    resizeToWindow

pageUp, pageDown :: Monad m => VgrepT ResultsState m ()
pageUp = do
    unlessS (isJust . moveUp) $ do
        modify (repeatedly (hideNext >=> showPrev))
        void resizeToWindow
    modify (repeatedly moveUp)
pageDown = do
    unlessS (isJust . moveDown) $ do
        modify (repeatedly hidePrev)
        void resizeToWindow
    modify (repeatedly moveDown)

repeatedly :: (a -> Maybe a) -> a -> a
repeatedly f = go
  where
    go x | Just x' <- f x = go x'
         | otherwise      = x


prevLine, nextLine :: Monad m => VgrepT ResultsState m ()
prevLine = maybeModify tryPrevLine >> void resizeToWindow
nextLine = maybeModify tryNextLine >> void resizeToWindow

tryPrevLine, tryNextLine :: Buffer -> Maybe Buffer
tryPrevLine buf = moveUp   buf <|> (showPrev buf >>= tryPrevLine)
tryNextLine buf = moveDown buf <|> (showNext buf >>= tryNextLine)

maybeModify :: Monad m => (s -> Maybe s) -> VgrepT s m ()
maybeModify f = do
    s <- get
    case f s of
        Just s' -> put s'
        Nothing -> pure ()


renderResultList :: Monad m => VgrepT ResultsState m Image
renderResultList = do
    void resizeToWindow
    visibleLines <- use (to toLines)
    width <- views region regionWidth
    let render = renderLine width (lineNumberWidth visibleLines)
    renderedLines <- traverse render visibleLines
    pure (vertCat renderedLines)
  where lineNumberWidth
            = foldl' max 0
            . map (twoExtraSpaces . length . show)
            . mapMaybe lineNumber
        twoExtraSpaces = (+ 2)

renderLine
    :: Monad m
    => Int
    -> Int
    -> DisplayLine
    -> VgrepT ResultsState m Image
renderLine width lineNumberWidth displayLine = do
    fileHeaderStyle <- view (config . colors . fileHeaders)
    lineNumberStyle <- view (config . colors . lineNumbers)
    resultLineStyle <- view (config . colors . normal)
    selectedStyle   <- view (config . colors . selected)
    pure $ case displayLine of
        FileHeader file     -> renderFileHeader fileHeaderStyle file
        Line         (n, t) -> horizCat [ renderLineNumber lineNumberStyle n
                                        , renderLineText   resultLineStyle t ]
        SelectedLine (n, t) -> horizCat [ renderLineNumber lineNumberStyle n
                                        , renderLineText   selectedStyle   t ]
  where
    padWithSpace w = T.take (fromIntegral w)
                   . T.justifyLeft (fromIntegral w) ' '
                   . T.cons ' '
    justifyRight w s = T.justifyRight (fromIntegral w) ' ' (s <> " ")

    renderFileHeader :: Attr -> File -> Image
    renderFileHeader attr = text attr . padWithSpace width . getFileName

    renderLineNumber :: Attr -> Maybe Int -> Image
    renderLineNumber attr = text attr
                          . justifyRight lineNumberWidth
                          . maybe "" (T.pack . show)

    renderLineText :: Attr -> Text -> Image
    renderLineText attr = text attr
                        . padWithSpace (width - lineNumberWidth)


resizeToWindow :: Monad m => VgrepT ResultsState m Redraw
resizeToWindow = do
    height <- views region regionHeight
    currentBuffer <- get
    case Buffer.resize height currentBuffer of
        Just resizedBuffer -> put resizedBuffer >> pure Redraw
        Nothing            -> pure Unchanged


currentFileName :: Getter ResultsState (Maybe Text)
currentFileName =
    pre (to current . _Just . _1 . to getFileName)

currentLineNumber :: Getter ResultsState (Maybe Int)
currentLineNumber =
    pre (to current . _Just . _2 . _1 . _Just)

currentFileResultLineNumbers :: Getter ResultsState [Int]
currentFileResultLineNumbers =
    to (mapMaybe fst . currentFile)
