{-# LANGUAGE TemplateHaskell, DisambiguateRecordFields #-}
module Vgrep.Widget.Results where

import Control.Lens
import Control.Lens.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence as Seq hiding (length)
import Graphics.Vty (DisplayRegion, Image)

import Vgrep.Widget.Type

data Buffer a = Buffer { _size :: !Int
                       , _pre  :: !(Seq a)
                       , _cur  :: !a
                       , _post :: !(Seq a) }

data ResultsState = State { _files      :: Buffer FileResults
                          , _cursorPos  :: Int
                          , _dimensions :: DisplayRegion }

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
    State { _files      = filesBuffer files
          , _cursorPos  = 0
          , _dimensions = dimensions }
  where
    filesBuffer = initBuffer . map (over _2 initBuffer) . Map.toList
