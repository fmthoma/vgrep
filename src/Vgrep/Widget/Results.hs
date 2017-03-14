{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Vgrep.Widget.Results (
    -- * Results list widget
      resultsWidget
    , ResultsWidget

    -- ** Internal widget state
    , Results ()

    -- ** Widget actions
    , feedResult
    , resizeToWindow
    , prevLine
    , nextLine
    , pageUp
    , pageDown

    -- ** Lenses
    , currentFileName
    , currentLineNumber
    , currentFileResults

    -- * Re-exports
    , module Vgrep.Results
    ) where

import           Control.Applicative
import           Control.Lens.Compat
import           Control.Monad.State.Extended
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image           hiding ((<|>))

import Vgrep.Ansi
import Vgrep.Environment
import Vgrep.Event
import Vgrep.Results
import Vgrep.Type
import Vgrep.Widget.Results.Internal as Internal
import Vgrep.Widget.Type


type ResultsWidget = Widget Results

-- | The results widget displays a list of lines with line numbers, grouped
-- by files.
--
-- * __Initial state__
--
--     The initial buffer is empty and can be filled line by line using
--     'feedResult'.
--
-- * __Drawing the results list__
--
--     Found matches are grouped by file name. Each file group has a header
--     and a list of result lines with line numbers. The result lines can
--     be selected with the cursor, the file group headers are skipped.
--     When only part of a file group is shown at the top of the screen,
--     the header is shown nevertheless.
resultsWidget :: ResultsWidget
resultsWidget =
    Widget { initialize = initResults
           , draw       = renderResultList
           , cursor     = const NoCursor }

initResults :: Results
initResults = EmptyResults


-- | Add a line to the results list. If the result is found in the same
-- file as the current last result, it will be added to the same results
-- group, otherwise a new group will be opened.
feedResult :: Monad m => FileLineReference -> VgrepT Results m Redraw
feedResult line = do
    modify (feed line)
    resizeToWindow

-- | Move up/down one results page. File group headers will be skipped.
pageUp, pageDown :: Monad m => VgrepT Results m ()
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

-- | Move up/down one results line. File group headers will be skipped.
prevLine, nextLine :: Monad m => VgrepT Results m ()
prevLine = maybeModify tryPrevLine >> void resizeToWindow
nextLine = maybeModify tryNextLine >> void resizeToWindow

tryPrevLine, tryNextLine :: Results -> Maybe Results
tryPrevLine buf = moveUp   buf <|> (showPrev buf >>= tryPrevLine)
tryNextLine buf = moveDown buf <|> (showNext buf >>= tryNextLine)

maybeModify :: Monad m => (s -> Maybe s) -> VgrepT s m ()
maybeModify f = do
    s <- get
    case f s of
        Just s' -> put s'
        Nothing -> pure ()


renderResultList :: Monad m => VgrepT Results m Image
renderResultList = do
    void resizeToWindow
    visibleLines <- use (to toLines)
    width <- view viewportWidth
    let render = renderLine width (lineNumberWidth visibleLines)
    renderedLines <- traverse render visibleLines
    pure (vertCat renderedLines)
  where lineNumberWidth
            = foldl' max 0
            . map (twoExtraSpaces . length . show)
            . mapMaybe displayLineNumber
        twoExtraSpaces = (+ 2) -- because line numbers are padded,
                               -- see `justifyRight` below

renderLine
    :: Monad m
    => Int
    -> Int
    -> DisplayLine
    -> VgrepT Results m Image
renderLine width lineNumberWidth displayLine = do
    fileHeaderStyle <- view (config . colors . fileHeaders)
    lineNumberStyle <- view (config . colors . lineNumbers)
    resultLineStyle <- view (config . colors . normal)
    selectedStyle   <- view (config . colors . selected)
    pure $ case displayLine of
        FileHeader (File f)
            -> renderFileHeader fileHeaderStyle f
        Line (LineReference n t)
            -> horizCat [ renderLineNumber lineNumberStyle n
                        , renderLineText   resultLineStyle t ]
        SelectedLine (LineReference n t)
            -> horizCat [ renderLineNumber lineNumberStyle n
                        , renderLineText   selectedStyle   t ]
  where
    padWithSpace w = T.take (fromIntegral w)
                   . T.justifyLeft (fromIntegral w) ' '
                   . T.cons ' '
    justifyRight w s = T.justifyRight (fromIntegral w) ' ' (s <> " ")

    renderFileHeader :: Attr -> Text -> Image
    renderFileHeader attr = text' attr . padWithSpace width

    renderLineNumber :: Attr -> Maybe Int -> Image
    renderLineNumber attr = text' attr
                          . justifyRight lineNumberWidth
                          . maybe "" (T.pack . show)

    renderLineText :: Attr -> AnsiFormatted -> Image
    renderLineText attr txt
        = renderAnsi attr
        . takeFormatted (width - lineNumberWidth)
        . padFormatted  (width - lineNumberWidth) ' '
        $ cat [ bare " ", txt, bare (T.replicate width " ") ]

resizeToWindow :: Monad m => VgrepT Results m Redraw
resizeToWindow = do
    height <- view viewportHeight
    currentBuffer <- get
    case Internal.resize height currentBuffer of
        Just resizedBuffer -> put resizedBuffer >> pure Redraw
        Nothing            -> pure Unchanged
