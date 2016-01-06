{-# LANGUAGE RecordWildCards, MultiWayIf #-}
module Vgrep.Widget.Pager where

import Data.Foldable
import Data.Monoid
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Event

data PagerState = PagerState { buffer          :: [String]
                             , scrollPos       :: Int
                             , region          :: DisplayRegion
                             , showLineNumbers :: Bool }


initialPagerState :: [String] -> DisplayRegion -> PagerState
initialPagerState items region =
    PagerState { buffer          = items
               , scrollPos       = 0
               , region          = region
               , showLineNumbers = True }

handlePagerEvents :: EventHandler Event PagerState
handlePagerEvents = handleKey KUp   [] (updateScrollPos . scrollUp)
                 <> handleKey KDown [] (updateScrollPos . scrollDown)

scrollUp :: PagerState -> PagerState
scrollUp state@PagerState{..} = state { scrollPos = scrollPos - 1 }

scrollDown :: PagerState -> PagerState
scrollDown state@PagerState{..} = state { scrollPos = scrollPos + 1 }

renderPager :: PagerState -> Picture
renderPager PagerState{..} =
    picForImage . resizeWidth (regionWidth region) $ (lineNumbers <|> textLines)
  where
    textLines = fold . fmap (string defAttr)
                     . take (regionHeight region)
                     . drop scrollPos . fmap (' ' :) $ buffer

    lineNumbers = fold . fmap (string (defAttr `withForeColor` brightBlack))
                       . take (min (regionHeight region) (length buffer - scrollPos))
                       . drop scrollPos . fmap show $ [1..]

resizeToRegion :: DisplayRegion -> PagerState -> PagerState
resizeToRegion newRegion state = updateScrollPos $ state { region = newRegion }

updateScrollPos :: PagerState -> PagerState
updateScrollPos state@PagerState{..} =
    if | scrollPos <= 0                  -> state { scrollPos = 0 }
       | scrollPos > linesCount - height -> state { scrollPos = linesCount - height }
       | otherwise                       -> state
  where height = regionHeight region
        linesCount = length buffer
