{-# LANGUAGE RecordWildCards, MultiWayIf, TemplateHaskell #-}
module Vgrep.Widget.Pager ( PagerState()
                          , PagerWidget
                          , pagerWidget
                          ) where

import Control.Lens
import Control.Lens.TH
import Data.Foldable
import Data.Monoid
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type

data PagerState = PagerState { _buffer          :: [String]
                             , _scrollPos       :: Int
                             , _region          :: DisplayRegion
                             , _showLineNumbers :: Bool }

makeLenses ''PagerState

type PagerWidget = Widget PagerState

pagerWidget :: [String]
            -> DisplayRegion
            -> PagerWidget
pagerWidget items region = Widget { _state       = initialPagerState items region
                                  , _dimensions  = region
                                  , _resize      = resizeToRegion
                                  , _draw        = renderPager
                                  , _handleEvent = handlePagerEvents }


initialPagerState :: [String] -> DisplayRegion-> PagerState
initialPagerState items displayRegion =
    PagerState { _buffer          = items
               , _scrollPos       = 0
               , _region          = displayRegion
               , _showLineNumbers = True }

handlePagerEvents :: EventHandler PagerState
handlePagerEvents = handleKey KUp   [] (updateScrollPos . scrollUp)
                 <> handleKey KDown [] (updateScrollPos . scrollDown)

scrollUp :: PagerState -> PagerState
scrollUp = over scrollPos (subtract 1)

scrollDown :: PagerState -> PagerState
scrollDown = over scrollPos (+ 1)

renderPager :: PagerState -> Image
renderPager state =
    resizeWidth width $ (lineNumbers <|> textLines)
  where
    width  = regionWidth  (view region state)
    height = regionHeight (view region state)
    bufferLength = length (view buffer state)
    currentPosition = view scrollPos state

    textLines = fold . fmap (string defAttr)
                     . take height
                     . drop currentPosition
                     . fmap padWithSpace
                     $ view buffer state

    lineNumbers = fold . fmap (string (defAttr `withForeColor` brightBlack
                                               `withBackColor` black))
                       . take (min height (bufferLength - currentPosition))
                       . drop currentPosition
                       . fmap (padWithSpace . show)
                       $ [1..]

    padWithSpace s = ' ' : s ++ " "

resizeToRegion :: DisplayRegion -> PagerState -> PagerState
resizeToRegion newRegion = updateScrollPos . set region newRegion

updateScrollPos :: PagerState -> PagerState
updateScrollPos state =
    if | pos < 0                   -> set scrollPos 0 state
       | linesCount < height       -> set scrollPos 0 state
       | pos > linesCount - height -> set scrollPos (linesCount - height) state
       | otherwise                 -> state
  where pos = view scrollPos state
        height = regionHeight (view region state)
        linesCount = length (view buffer state)
