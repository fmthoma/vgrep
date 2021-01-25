{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Vgrep.Widget.Pager (
    -- * Pager widget
      pagerWidget
    , PagerWidget

    -- ** Internal state
    , Pager ()

    -- ** Widget actions
    , moveToLine
    , scroll
    , scrollPage
    , scrollPageFraction
    , hScroll
    , replaceBufferContents
    ) where

import           Control.Applicative     (liftA2)
import           Control.Lens.Compat
import           Data.Foldable
import qualified Data.IntMap.Strict      as Map
import           Data.Sequence           (Seq, (><))
import qualified Data.Sequence           as Seq
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Graphics.Vty.Attributes
import           Graphics.Vty.Image

import Vgrep.Ansi
import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type
import Vgrep.Widget.Pager.Internal
import Vgrep.Widget.Type


type PagerWidget = Widget Pager

-- | Display lines of text with line numbers
--
-- * __Initial state__
--
--     The pager is empty, i. e. no lines of text to display.
--
-- * __Drawing the pager__
--
--     The lines of text are printed, starting at the current scroll
--     position. If not enough lines are available, the scroll position is
--     adjusted until either the screen is filled, or the first line is
--     reached. Highlighted lines are displayed according to the config
--     values 'normalHl' and 'lineNumbersHl' (default: bold).
pagerWidget :: PagerWidget
pagerWidget = Widget
    { initialize = initPager
    , draw       = renderPager }

initPager :: Pager
initPager = Pager
    { _column      = 0
    , _highlighted = Map.empty
    , _above       = Seq.empty
    , _visible     = Seq.empty }


-- | Replace the currently displayed text.
replaceBufferContents
    :: Monad m
    => Seq Text -- ^ Lines of text to display in the pager (starting with line 1)
    -> Map.IntMap AnsiFormatted -- ^ Line numbers and formatted text for highlighted lines
    -> VgrepT Pager m ()
replaceBufferContents newContent newHighlightedLines = put initPager
    { _visible     = newContent
    , _highlighted = newHighlightedLines }

-- | Scroll to the given line number.
moveToLine :: Monad m => Int -> VgrepT Pager m Redraw
moveToLine n = view viewportHeight >>= \height -> do
    setPosition (n - height `div` 2)
    pure Redraw

-- | Scroll up or down one line.
--
-- > scroll (-1)  -- scroll one line up
-- > scroll 1     -- scroll one line down
scroll :: Monad m => Int -> VgrepT Pager m Redraw
scroll n = do
    pos <- use position
    setPosition (pos + n)
    pure Redraw

setPosition :: Monad m => Int -> VgrepT Pager m ()
setPosition n = view viewportHeight >>= \height -> do
    allLines <- liftA2 (+) (use (visible . to length)) (use (above . to length))
    let newPosition = if
            | n < 0 || allLines < height -> 0
            | n > allLines - height      -> allLines - height
            | otherwise                  -> n
    modify $ \pager@Pager{..} ->
        let (newAbove, newVisible) = Seq.splitAt newPosition (_above >< _visible)
        in  pager
            { _above    = newAbove
            , _visible  = newVisible }

-- | Scroll up or down one page. The first line on the current screen will
-- be the last line on the scrolled screen and vice versa.
--
-- > scrollPage (-1)  -- scroll one page up
-- > scrollPage 1     -- scroll one page down
scrollPage :: Monad m => Int -> VgrepT Pager m Redraw
scrollPage n = view viewportHeight >>= \height ->
    scroll (n * (height - 1))
  -- gracefully leave one ^ line on the screen

-- | Scroll up or down a fraction of a page. For integers,
-- 'scrollPageFraction n == scrollPage n'.
--
-- > scrollPageFraction (-1%2)            -- scroll one half page up
-- > scrollPageFraction (1%2)             -- scroll one half page down
-- > scrollPageFraction (fromRational 1)  -- scroll one page down
scrollPageFraction :: Monad m => Rational -> VgrepT Pager m Redraw
scrollPageFraction a = view viewportHeight >>= \height ->
    scroll (round (a * (fromIntegral height - 1)))
                      -- gracefully leave one ^ line on the screen

-- | Horizontal scrolling. Increment is one 'tabstop'.
--
-- > hScroll (-1)  -- scroll one tabstop left
-- > hScroll 1     -- scroll one tabstop right
hScroll :: Monad m => Int -> VgrepT Pager m Redraw
hScroll n = do
    tabWidth <- view (config . tabstop)
    modifying column $ \currentColumn ->
        let newColumn = currentColumn + n * tabWidth
        in  if newColumn > 0 then newColumn else 0
    pure Redraw


renderPager :: Monad m => VgrepT Pager m Image
renderPager = do
    textColor         <- view (config . colors . normal)
    textColorHl       <- view (config . colors . normalHl)
    lineNumberColor   <- view (config . colors . lineNumbers)
    lineNumberColorHl <- view (config . colors . lineNumbersHl)
    width             <- view viewportWidth
    height            <- view viewportHeight
    startPosition     <- use position
    startColumn       <- use (column . to fromIntegral)
    visibleLines      <- use (visible . to (Seq.take height) . to toList)
    highlightedLines  <- use highlighted

    let (renderedLineNumbers, renderedTextLines)
            = over both fold
            . unzip
            $ zipWith renderLine [startPosition+1..] visibleLines
          where
            renderLine :: Int -> Text -> (Image, Image)
            renderLine num txt = case Map.lookup num highlightedLines of
                Just formatted -> ( renderLineNumber lineNumberColorHl num
                                  , renderFormatted textColorHl formatted )
                Nothing        -> ( renderLineNumber lineNumberColor num
                                  , renderLineText textColor txt )

            renderLineNumber :: Attr -> Int -> Image
            renderLineNumber attr
                = text' attr
                . (`T.snoc` ' ')
                . T.cons ' '
                . T.pack
                . show

            renderLineText :: Attr -> Text -> Image
            renderLineText   attr
                = text' attr
                . T.justifyLeft width ' '
                . T.take width
                . T.cons ' '
                . T.drop startColumn

            renderFormatted :: Attr -> AnsiFormatted -> Image
            renderFormatted  attr
                = renderAnsi attr
                . padFormatted width ' '
                . takeFormatted width
                . (bare " " <>)
                . dropFormatted startColumn


    pure (resizeWidth width (renderedLineNumbers <|> renderedTextLines))
