{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Pager
    ( PagerState ()
    , PagerEvent (..)
    , PagerWidget
    , pagerWidget

    , moveToLine
    , scroll
    , scrollPage
    , replaceBufferContents
    ) where

import Control.Lens
import Control.Monad.State.Extended (StateT, put, modify)
import Data.Foldable
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Graphics.Vty.Image hiding (resize)
import Graphics.Vty.Prelude

import Vgrep.Environment
import Vgrep.Type
import Vgrep.Widget.Type


data PagerState = PagerState { _position :: Int
                             , _above    :: [Text]
                             , _visible  :: [Text] }

makeLensesFor [("_position", "position"), ("_visible", "visible")] ''PagerState

data PagerEvent
    = Scroll Int
    | ScrollPage Int
    | MoveToLine Int
    | ReplaceBufferContents [Text]

type PagerWidget = Widget PagerEvent PagerState

pagerWidget :: PagerWidget
pagerWidget =
    Widget { initialize = initPager
           , draw       = renderPager
           , handle     = handlePagerEvent }

initPager :: PagerState
initPager = PagerState { _position = 0
                       , _above    = []
                       , _visible  = [] }


handlePagerEvent :: Monad m => PagerEvent -> StateT PagerState (VgrepT m) ()
handlePagerEvent event = view region >>= \displayRegion -> case event of
    ReplaceBufferContents content -> replaceBufferContents content
    MoveToLine n                  -> moveToLine displayRegion n
    ScrollPage n                  -> scrollPage displayRegion n
    Scroll n                      -> scroll displayRegion n

replaceBufferContents :: Monad m => [Text] -> StateT PagerState (VgrepT m) ()
replaceBufferContents content = put (set visible content initPager)

moveToLine :: Monad m => DisplayRegion -> Int -> StateT PagerState (VgrepT m) ()
moveToLine displayRegion n = do
    let height = regionHeight displayRegion
    pos    <- use position
    scroll displayRegion (n - height `div` 2 - pos)

scroll :: Monad m => DisplayRegion -> Int -> StateT PagerState (VgrepT m) ()
scroll displayRegion n = do
    let height = regionHeight displayRegion
    linesVisible <- uses visible (length . take (height + 1))
    if | n > 0 && linesVisible > height
                   -> modify goDown >> scroll displayRegion (n - 1)
       | n < 0     -> modify goUp   >> scroll displayRegion (n + 1)
       | otherwise -> pure ()
  where
    goDown (PagerState l as     (b:bs)) = PagerState (l + 1) (b:as) bs
    goDown (PagerState l as     [])     = PagerState l       as     []
    goUp   (PagerState l (a:as) bs)     = PagerState (l - 1) as     (a:bs)
    goUp   (PagerState l []     bs)     = PagerState l       []     bs

scrollPage :: Monad m => DisplayRegion -> Int -> StateT PagerState (VgrepT m) ()
scrollPage displayRegion n =
    let height = regionHeight displayRegion
    in  scroll displayRegion (n * (height - 1))
                    -- gracefully leave one ^ line on the screen


renderPager :: Monad m => PagerState -> VgrepT m Image
renderPager state = do
    textColor       <- view (config . colors . normal)
    lineNumberColor <- view (config . colors . lineNumbers)
    (width, height) <- view region
    let visibleLines = take height (view visible state)
        image = renderLineNumbers lineNumberColor visibleLines
            <|> renderTextLines textColor visibleLines
    pure (resizeWidth width image)
  where
    renderTextLines attr =
        fold . fmap (string attr)
             . fmap padWithSpace
             . fmap T.unpack

    renderLineNumbers attr visibleLines =
        fold . fmap (string attr)
             . take (length visibleLines)
             . fmap (padWithSpace . show)
             $ [(view position state) ..]

    padWithSpace s = ' ' : s ++ " "
