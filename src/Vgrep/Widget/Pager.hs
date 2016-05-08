{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Widget.Pager (
    -- * Pager widget
      pagerWidget
    , PagerWidget

    -- ** Internal state
    , PagerState ()

    -- ** Widget actions
    , moveToLine
    , scroll
    , scrollPage
    , hScroll
    , replaceBufferContents
    ) where

import Control.Lens
import Control.Monad.State.Extended (put, modify)
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Graphics.Vty.Image hiding (resize)
import Graphics.Vty.Input
import Graphics.Vty.Prelude

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type
import Vgrep.Widget.Type


-- | Keeps track of the lines of text to display, the current scroll
-- positions, and the set of highlighted line numbers.
data PagerState = PagerState
    { _position    :: Int
    , _column      :: Int
    , _highlighted :: Set Int
    , _above       :: [Text]
    , _visible     :: [Text] }

makeLensesFor [ ("_position",    "position")
              , ("_column",      "column")
              , ("_visible",     "visible")
              , ("_highlighted", "highlighted") ] ''PagerState

type PagerWidget = Widget PagerState

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
--
-- * __Default keybindings__
--
--     @
--     ←↓↑→, hjkl    'hScroll' (-1), 'scroll' 1, 'scroll' (-1), 'hScroll' 1
--     PgUp, PgDn    'scrollPage' (-1), 'scrollPage' 1
--     @
pagerWidget :: PagerWidget
pagerWidget = Widget
    { initialize = initPager
    , draw       = renderPager
    , handle     = fmap const pagerKeyBindings }

initPager :: PagerState
initPager = PagerState
    { _position    = 0
    , _column      = 0
    , _highlighted = S.empty
    , _above       = []
    , _visible     = [] }


pagerKeyBindings
    :: Monad m
    => Event
    -> Next (VgrepT PagerState m Redraw)
pagerKeyBindings = dispatchMap $ fromList
    [ (EvKey KUp         [], scroll up      )
    , (EvKey KDown       [], scroll down    )
    , (EvKey (KChar 'k') [], scroll up      )
    , (EvKey (KChar 'j') [], scroll down    )
    , (EvKey KLeft       [], hScroll left   )
    , (EvKey KRight      [], hScroll right  )
    , (EvKey (KChar 'h') [], hScroll left   )
    , (EvKey (KChar 'l') [], hScroll right  )
    , (EvKey KPageUp     [], scrollPage up  )
    , (EvKey KPageDown   [], scrollPage down) ]
  where up = -1; down = 1; left = -1; right = 1

-- | Replace the currently displayed text.
replaceBufferContents
    :: Monad m
    => [Text] -- ^ Lines of text to display in the pager (starting with line 1)
    -> [Int]  -- ^ List of line numbers that should be highlighted
    -> VgrepT PagerState m ()
replaceBufferContents newContent newHighlightedLines = put initPager
    { _visible     = newContent
    , _highlighted = S.fromList newHighlightedLines }

-- | Scroll to the given line number.
moveToLine :: Monad m => Int -> VgrepT PagerState m Redraw
moveToLine n = view region >>= \displayRegion -> do
    let height = regionHeight displayRegion
    pos <- use position
    scroll (n - height `div` 2 - pos)

-- | Scroll up or down one line.
--
-- >>> scroll (-1)  -- scroll one line up
-- >>> scroll 1     -- scroll one line down
scroll :: Monad m => Int -> VgrepT PagerState m Redraw
scroll n = view region >>= \displayRegion -> do
    let height = regionHeight displayRegion
    linesVisible <- uses visible (length . take (height + 1))
    if | n > 0 && linesVisible > height
                   -> modify goDown >> scroll (n - 1)
       | n < 0     -> modify goUp   >> scroll (n + 1)
       | otherwise -> pure Redraw
  where
    goDown (PagerState l c h as     (b:bs)) = PagerState (l + 1) c h (b:as) bs
    goDown (PagerState l c h as     [])     = PagerState l       c h as     []
    goUp   (PagerState l c h (a:as) bs)     = PagerState (l - 1) c h as     (a:bs)
    goUp   (PagerState l c h []     bs)     = PagerState l       c h []     bs

-- | Scroll up or down one page. The first line on the current screen will
-- be the last line on the scrolled screen and vice versa.
--
-- >>> scrollPage (-1)  -- scroll one page up
-- >>> scrollPage 1     -- scroll one page down
scrollPage :: Monad m => Int -> VgrepT PagerState m Redraw
scrollPage n = view region >>= \displayRegion ->
    let height = regionHeight displayRegion
    in  scroll (n * (height - 1))
      -- gracefully leave one ^ line on the screen

-- | Horizontal scrolling. Increment is one 'tabstop'.
--
-- >>> hScroll (-1)  -- scroll one tabstop left
-- >>> hScroll 1     -- scroll one tabstop right
hScroll :: Monad m => Int -> VgrepT PagerState m Redraw
hScroll n = do
    tabWidth <- view (config . tabstop)
    modifying column $ \currentColumn ->
        let newColumn = currentColumn + n * tabWidth
        in  if newColumn > 0 then newColumn else 0
    pure Redraw


renderPager :: Monad m => VgrepT PagerState m Image
renderPager = do
    textColor         <- view (config . colors . normal)
    textColorHl       <- view (config . colors . normalHl)
    lineNumberColor   <- view (config . colors . lineNumbers)
    lineNumberColorHl <- view (config . colors . lineNumbersHl)
    (width, height)   <- view region
    startPosition     <- use position
    startColumn       <- use (column . to fromIntegral)
    visibleLines      <- use (visible . to (take height))
    highlightedLines  <- use highlighted

    let renderLine (num, txt) =
            let (numColor, txtColor) = if num `S.member` highlightedLines
                    then (lineNumberColorHl, textColorHl)
                    else (lineNumberColor,   textColor)
                visibleCharacters = T.unpack (T.drop startColumn txt)
            in  ( string numColor (padWithSpace (show num))
                , string txtColor (padWithSpace visibleCharacters) )

        (renderedLineNumbers, renderedTextLines)
            = over both fold . unzip
            . map renderLine
            $ zip [startPosition+1..] visibleLines

    pure (resizeWidth width (renderedLineNumbers <|> renderedTextLines))

  where padWithSpace s = ' ' : s ++ " "
