{-# LANGUAGE RecordWildCards, MultiWayIf #-}
module Vgrep.Widget.Pager ( PagerState()
                          , PagerWidget
                          , pagerWidget
                          ) where

import Data.Foldable
import Data.Monoid
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type

data PagerState = PagerState { buffer          :: [String]
                             , scrollPos       :: Int
                             , region          :: DisplayRegion
                             , showLineNumbers :: Bool }

type PagerWidget = Widget PagerState

pagerWidget :: [String]
            -> DisplayRegion
            -> PagerWidget
pagerWidget items region = Widget { state       = initialPagerState items region
                                  , dimensions  = region
                                  , resize      = resizeToRegion
                                  , draw        = renderPager
                                  , handleEvent = handlePagerEvents }


initialPagerState :: [String] -> DisplayRegion-> PagerState
initialPagerState items displayRegion =
    PagerState { buffer          = items
               , scrollPos       = 0
               , region          = displayRegion
               , showLineNumbers = True }

handlePagerEvents :: EventHandler PagerState
handlePagerEvents = handleKey KUp   [] (updateScrollPos . scrollUp)
                 <> handleKey KDown [] (updateScrollPos . scrollDown)

scrollUp :: PagerState -> PagerState
scrollUp state@PagerState{..} = state { scrollPos = scrollPos - 1 }

scrollDown :: PagerState -> PagerState
scrollDown state@PagerState{..} = state { scrollPos = scrollPos + 1 }

renderPager :: PagerState -> Image
renderPager PagerState{..} =
    resizeWidth width $ (lineNumbers <|> textLines)
  where
    width  = regionWidth  region
    height = regionHeight region

    textLines = fold . fmap (string defAttr)
                     . take height
                     . drop scrollPos . fmap padWithSpace $ buffer

    lineNumbers = fold . fmap (string (defAttr `withForeColor` brightBlack `withBackColor` black))
                       . take (min height (length buffer - scrollPos))
                       . drop scrollPos . fmap (padWithSpace . show) $ [1..]

    padWithSpace s = ' ' : s ++ " "

resizeToRegion :: DisplayRegion -> PagerState -> PagerState
resizeToRegion newRegion state = updateScrollPos $ state { region = newRegion }

updateScrollPos :: PagerState -> PagerState
updateScrollPos state@PagerState{..} =
    if | scrollPos < 0                   -> state { scrollPos = 0 }
       | linesCount < height             -> state { scrollPos = 0 }
       | scrollPos > linesCount - height -> state { scrollPos = linesCount - height }
       | otherwise                       -> state
  where height = regionHeight region
        linesCount = length buffer
