{-# LANGUAGE MultiWayIf, TemplateHaskell #-}
module Vgrep.Widget.Pager ( PagerState()
                          , PagerWidget
                          , pagerWidget

                          , moveToLine
                          , scrollUp
                          , scrollDown
                          , replaceBufferContents
                          ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Data.Foldable
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type

data PagerState = PagerState { _buffer          :: Text
                             , _scrollPos       :: Int
                             , _region          :: DisplayRegion
                             , _showLineNumbers :: Bool }

makeLenses ''PagerState

type PagerWidget = Widget PagerState

pagerWidget :: Text
            -> DisplayRegion
            -> PagerWidget
pagerWidget items region = Widget { _widgetState = initialPagerState items region
                                  , _dimensions  = region
                                  , _resize      = resizeToRegion
                                  , _draw        = renderPager }


initialPagerState :: Text -> DisplayRegion-> PagerState
initialPagerState lines displayRegion =
    PagerState { _buffer          = lines
               , _scrollPos       = 0
               , _region          = displayRegion
               , _showLineNumbers = True }

replaceBufferContents :: Text -> State PagerState ()
replaceBufferContents lines = do assign buffer lines
                                 assign scrollPos 0

moveToLine :: Int -> State PagerState ()
moveToLine n = do
    height <- uses region regionHeight
    assign scrollPos (n - height `div` 2)
    updateScrollPos

scrollUp :: State PagerState ()
scrollUp = modifying scrollPos (subtract 1) >> updateScrollPos

scrollDown :: State PagerState ()
scrollDown = modifying scrollPos (+ 1) >> updateScrollPos

renderPager :: PagerState -> Image
renderPager state =
    resizeWidth width $ (lineNumbers <|> textLines)
  where
    width  = regionWidth  (view region state)
    height = regionHeight (view region state)
    linesCount = view bufferLength state
    currentPosition = view scrollPos state

    textLines = fold . fmap (string defAttr)
                     . take height
                     . drop currentPosition
                     . fmap padWithSpace
                     . fmap T.unpack
                     . T.lines
                     $ view buffer state

    lineNumbers = fold . fmap (string (defAttr `withForeColor` brightBlack
                                               `withBackColor` black))
                       . take (min height (linesCount - currentPosition))
                       . drop currentPosition
                       . fmap (padWithSpace . show)
                       $ [1..]

    padWithSpace s = ' ' : s ++ " "

resizeToRegion :: DisplayRegion -> State PagerState ()
resizeToRegion newRegion = assign region newRegion >> updateScrollPos

updateScrollPos :: State PagerState ()
updateScrollPos = do
    pos        <- use scrollPos
    height     <- use (region . to regionHeight)
    linesCount <- use (buffer . to T.lines . to length)
    if | pos < 0                   -> assign scrollPos 0
       | linesCount < height       -> assign scrollPos 0
       | pos > linesCount - height -> assign scrollPos (linesCount - height)
       | otherwise                 -> return ()

bufferLength :: Getter PagerState Int
bufferLength = buffer . to T.lines . to length
