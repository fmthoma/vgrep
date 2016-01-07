{-# LANGUAGE TemplateHaskell, DisambiguateRecordFields, MultiWayIf #-}
module Vgrep.Widget.Results where

import Control.Lens (over, set, view, _2, (&))
import Control.Lens.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Graphics.Vty (Image)
import Graphics.Vty.Prelude

import Vgrep.Widget.Type

data Buffer a = Buffer { _size :: !Int
                       , _pre  :: !(Seq a)
                       , _cur  :: !a
                       , _post :: !(Seq a) }

makeLenses ''Buffer


data ResultsState = State { _files     :: Buffer FileResults
                          , _scrollPos :: Int
                          , _region    :: DisplayRegion }

type FileResults = (String, Buffer Line)
type Line = (LineNumber, String)
type LineNumber = Int

makeLenses ''ResultsState


type ResultsWidget = Widget ResultsState

resultsWidget :: Map String [Line]
              -> DisplayRegion
              -> ResultsWidget
resultsWidget files dimensions =
    Widget { _state       = initState files dimensions
           , _dimensions  = dimensions
           , _resize      = undefined
           , _draw        = undefined
           , _handleEvent = undefined }

initBuffer :: [a] -> Buffer a
initBuffer as = Buffer { _size = length as
                       , _pre  = Seq.empty
                       , _cur  = head as -- FIXME partial function!
                       , _post = Seq.fromList (tail as) }

initState :: Map String [Line]
          -> DisplayRegion
          -> ResultsState
initState files dimensions =
    State { _files     = buffer
          , _scrollPos = 0
          , _region    = dimensions }
  where
    buffer = (initBuffer . map (over _2 initBuffer) . Map.toList) files

updateScrollPos :: ResultsState -> ResultsState
updateScrollPos state =
    if | current < firstVisible -> state & set scrollPos current
       | current > lastVisible  -> state & set scrollPos (current - height + 1)
       | otherwise              -> state
  where
    height       = regionHeight (view region state)
    current = computeCurrentItem (view files state)
    firstVisible = view scrollPos state
    lastVisible  = firstVisible + height - 1

computeCurrentItem :: Buffer FileResults -> Int
computeCurrentItem buffer = linesInFilesBeforeCurrent
                          + fileHeadersBeforeCurrent
                          + linesInCurrentFileBeforeCursor
  where
    fileHeadersBeforeCurrent = length (view pre buffer)
    linesInFilesBeforeCurrent =
        (sum . fmap length) (view (pre . traverse . _2 . pre) buffer)
    linesInCurrentFileBeforeCursor =
        (sum . fmap length) (view (cur . _2 . pre) buffer)
