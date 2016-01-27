{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Pager
    ( PagerState ()
    , PagerWidget
    , pagerWidget

    , moveToLine
    , scroll
    , scrollPage
    , replaceBufferContents
    ) where

import Control.Lens
import Control.Monad.State (State)
import Data.Foldable
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Widget.Type


type Buffer = (Int, [Text], [Text])

data PagerState = PagerState { _buffer :: Buffer
                             , _region :: DisplayRegion }

makeLenses ''PagerState

type PagerWidget = Widget PagerState

pagerWidget :: Text
            -> DisplayRegion
            -> PagerWidget
pagerWidget items initialRegion =
    Widget { _widgetState = initialPagerState items initialRegion
           , _dimensions  = initialRegion
           , _resize      = resizeToRegion
           , _draw        = renderPager }


initialPagerState :: Text -> DisplayRegion-> PagerState
initialPagerState initialContent initialRegion =
    PagerState { _buffer          = (1, [], T.lines initialContent)
               , _region          = initialRegion }

replaceBufferContents :: Text -> State PagerState ()
replaceBufferContents content = assign buffer (1, [], T.lines content)

moveToLine :: Int -> State PagerState ()
moveToLine n = do
    height <- use (region . to regionHeight)
    pos    <- use (buffer . _1)
    scroll (n - height `div` 2 - pos)

scroll :: Int -> State PagerState ()
scroll n = do
    height <- uses region regionHeight
    linesVisible <- uses (buffer . _3) (length . take (height + 1))
    if | n > 0 && linesVisible > height
                   -> modifying buffer goDown >> scroll (n - 1)
       | n < 0     -> modifying buffer goUp   >> scroll (n + 1)
       | otherwise -> pure ()
  where
    goDown (l, as, b:bs) = (l + 1, b:as, bs)
    goDown (l, as, [])   = (l,     as,   [])
    goUp   (l, a:as, bs) = (l - 1, as, a:bs)
    goUp   (l, [],   bs) = (l,     [], bs)

scrollPage :: Int -> State PagerState ()
scrollPage n = do height <- uses region regionHeight
                  scroll (n * (height - 1))
                -- gracefully leave one ^ line on the screen

renderPager :: PagerState -> Image
renderPager state =
    resizeWidth width (lineNumbers <|> textLines)
  where
    (width, height) = view region state
    (currentPosition, _, bufferLines) = view buffer state
    visibleLines = take height bufferLines

    textLines = fold . fmap (string defAttr)
                     . take height
                     . fmap padWithSpace
                     . fmap T.unpack
                     $ visibleLines

    lineNumbers = fold . fmap (string (defAttr `withForeColor` blue))
                       . take (length visibleLines)
                       . fmap (padWithSpace . show)
                       $ [currentPosition ..]

    padWithSpace s = ' ' : s ++ " "

resizeToRegion :: DisplayRegion -> State PagerState ()
resizeToRegion newRegion = assign region newRegion
