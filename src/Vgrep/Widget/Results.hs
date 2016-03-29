{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Vgrep.Widget.Results
    ( ResultsState
    , ResultsWidget
    , resultsWidget

    , feedResult
    , prevLine
    , nextLine
    , pageUp
    , pageDown

    , currentFileName
    , currentLineNumber

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
import Graphics.Vty.Prelude
import Prelude

import Vgrep.Environment
import Vgrep.Results
import Vgrep.Results.Buffer as Buffer
import Vgrep.Type
import Vgrep.Widget.Type


type ResultsState = Buffer

data ResultsEvent
    = FeedResult FileLineReference
    | PageUp   | PageDown
    | PrevLine | NextLine

type ResultsWidget = Widget ResultsEvent ResultsState

resultsWidget :: ResultsWidget
resultsWidget =
    Widget { initialize = initResults
           , draw       = renderResultList
           , handle     = handleResultsEvent }

initResults :: ResultsState
initResults = EmptyBuffer


handleResultsEvent :: ResultsEvent -> StateT ResultsState Vgrep ()
handleResultsEvent = \case
    FeedResult line -> feedResult line
    PageUp          -> pageUp
    PageDown        -> pageDown
    PrevLine        -> prevLine
    NextLine        -> nextLine

feedResult :: FileLineReference -> StateT ResultsState Vgrep ()
feedResult line = do
    modify (feed line)
    resizeToWindow

pageUp, pageDown :: StateT ResultsState Vgrep ()
pageUp = do
    unlessS (isJust . moveUp) $ do
        modify (repeatedly (hideNext >=> showPrev))
        resizeToWindow
    modify (repeatedly moveUp)
pageDown = do
    unlessS (isJust . moveDown) $ do
        modify (repeatedly hidePrev)
        resizeToWindow
    modify (repeatedly moveDown)

repeatedly :: (a -> Maybe a) -> a -> a
repeatedly f = go
  where
    go x | Just x' <- f x = go x'
         | otherwise      = x


prevLine, nextLine :: StateT ResultsState Vgrep ()
prevLine = maybeModify tryPrevLine >> resizeToWindow
nextLine = maybeModify tryNextLine >> resizeToWindow

tryPrevLine, tryNextLine :: Buffer -> Maybe Buffer
tryPrevLine buf = moveUp   buf <|> (showPrev buf >>= tryPrevLine)
tryNextLine buf = moveDown buf <|> (showNext buf >>= tryNextLine)

maybeModify :: Monad m => (s -> Maybe s) -> StateT s m ()
maybeModify f = do
    s <- get
    case f s of
        Just s' -> put s'
        Nothing -> pure ()


renderResultList :: ResultsState -> Vgrep Image
renderResultList buffer = do
      width <- views region regionWidth
      renderedLines <- renderLines width (toLines buffer)
      pure (vertCat renderedLines)

renderLines :: Int -> [DisplayLine] -> Vgrep [Image]
renderLines width ls = traverse (renderLine (width, lineNumberWidth)) ls
  where lineNumberWidth = foldl' max 0
                        . map (twoExtraSpaces . length . show)
                        . catMaybes
                        $ map lineNumber ls
        twoExtraSpaces = (+ 2)

renderLine :: (Int, Int) -> DisplayLine -> Vgrep Image
renderLine (width, lineNumberWidth) displayLine = do
    fileHeaderStyle <- view (config . colors . fileHeaders)
    lineNumberStyle <- view (config . colors . lineNumbers)
    resultLineStyle <- view (config . colors . normal)
    highlightStyle  <- view (config . colors . highlight)
    pure $ case displayLine of
        FileHeader file     -> renderFileHeader fileHeaderStyle file
        Line         (n, t) -> horizCat [ renderLineNumber lineNumberStyle n
                                        , renderLineText   resultLineStyle t ]
        SelectedLine (n, t) -> horizCat [ renderLineNumber lineNumberStyle n
                                        , renderLineText   highlightStyle  t ]
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


resizeToWindow :: StateT ResultsState Vgrep ()
resizeToWindow = do
    height <- views region regionHeight
    modify (Buffer.resize height)


currentFileName :: Getter ResultsState (Maybe Text)
currentFileName =
    pre (to (current) . _Just . _1 . to getFileName)

currentLineNumber :: Getter ResultsState (Maybe Int)
currentLineNumber =
    pre (to current . _Just . _2 . _1 . _Just)
