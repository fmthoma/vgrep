{-# LANGUAGE Rank2Types, TemplateHaskell, DisambiguateRecordFields, MultiWayIf #-}
module Vgrep.Widget.Results ( ResultsState()
                            , ResultsWidget
                            , resultsWidget
                            ) where

import Control.Lens ( Lens', Traversal'
                    , over, set, view, views, preview
                    , _1, _2, _Just
                    , (&) )
import Control.Lens.TH
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq, ViewL(..), ViewR(..), (><), (|>), (<|))
import qualified Data.Sequence as Seq
import Data.Sequence.Lens
import Graphics.Vty
import Graphics.Vty.Prelude
import Prelude hiding (lines)

import Vgrep.Widget.Type
import Vgrep.Event

data Buffer a = Buffer { _size :: !Int
                       , _pre  :: !(Seq a)
                       , _cur  :: !(Maybe a)
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

resultsWidget :: DisplayRegion
              -> [(String, [Line])]
              -> ResultsWidget
resultsWidget dimensions files =
    Widget { _state       = initState files dimensions
           , _dimensions  = dimensions
           , _resize      = resizeToRegion
           , _draw        = drawResultList
           , _handleEvent = handleResultListEvent }

initBuffer :: [a] -> Buffer a
initBuffer as = Buffer { _size = length as
                       , _pre  = Seq.empty
                       , _cur  = listToMaybe as
                       , _post = Seq.fromList (drop 1 as) }

initState :: [(String, [Line])]
          -> DisplayRegion
          -> ResultsState
initState files dimensions =
    State { _files     = buffer
          , _scrollPos = 0
          , _region    = dimensions }
  where
    buffer = (initBuffer . map (over _2 initBuffer)) files


handleResultListEvent :: EventHandler ResultsState
handleResultListEvent = handleKey KUp   [] previousLine
                     <> handleKey KDown [] nextLine

previousLine :: ResultsState -> ResultsState
previousLine state = case preview (currentFile' . linesAbove . viewR) state of
    Nothing        -> state
    Just EmptyR    -> previousFile state
    Just (ls :> l) -> state & set  (currentFile' . linesAbove) ls
                            & set  (currentFile' . currentLine) (Just l)
                            & over (currentFile' . linesBelow)
                                   (views (currentFile' . currentLine) asSeq state ><)
                            & updateScrollPos

previousFile :: ResultsState -> ResultsState
previousFile state = case preview (filesAbove . viewR) state of
    Nothing        -> state
    Just EmptyR    -> state
    Just (fs :> f) -> state & set  filesAbove fs
                            & set  currentFile (Just f)
                            & over filesBelow
                                   (viewAsSeq currentFile state ><)
                            & updateScrollPos

nextLine :: ResultsState -> ResultsState
nextLine state = case preview (currentFile' . linesBelow . viewL) state of
    Nothing        -> state
    Just EmptyL    -> nextFile state
    Just (l :< ls) -> state & over (currentFile' . linesAbove)
                                   (>< views (currentFile' . currentLine) asSeq state)
                            & set  (currentFile' . currentLine) (Just l)
                            & set  (currentFile' . linesBelow) ls
                            & updateScrollPos

nextFile :: ResultsState -> ResultsState
nextFile state = case preview (filesBelow . viewL) state of
    Nothing        -> state
    Just EmptyL    -> state
    Just (f :< fs) -> state & over filesAbove
                                   (>< viewAsSeq currentFile state)
                            & set  currentFile (Just f)
                            & set  filesBelow fs
                            & updateScrollPos


updateScrollPos :: ResultsState -> ResultsState
updateScrollPos state =
    if | current < firstVisible -> state & set scrollPos current
       | current > lastVisible  -> state & set scrollPos (current - height + 1)
       | otherwise              -> state
  where
    height       = regionHeight (view region state)
    current      = computeCurrentItem state
    firstVisible = view scrollPos state
    lastVisible  = firstVisible + height - 1

computeCurrentItem :: ResultsState -> Int
computeCurrentItem state = linesInFilesBeforeCurrent
                         + 2 * fileHeadersBeforeCurrent + 1
                         + linesInCurrentFileBeforeCursor
  where
    fileHeadersBeforeCurrent = length (view filesAbove state)
    linesInFilesBeforeCurrent =
        (sum . fmap length) (view (filesAbove . traverse . linesAbove) state)
    linesInCurrentFileBeforeCursor =
        (sum . fmap length) (view (currentFile' . linesAbove) state)


drawResultList :: ResultsState -> Image
drawResultList state =
    fold . Seq.take height
         . Seq.drop pos
         $  foldMap drawFileResults (view filesAbove  state)
         >< foldMap drawFileResults (viewAsSeq (files . cur) state)
         >< foldMap drawFileResults (view filesBelow state)
  where
    pos    = view scrollPos state
    width  = regionWidth  (view region state)
    height = regionHeight (view region state)

    fileHeader = defAttr `withBackColor` green
    lineNumber = defAttr `withForeColor` brightBlack
    resultLine = defAttr
    highlight  = defAttr `withStyle` standout

    drawFileResults :: FileResults -> Seq Image
    drawFileResults results = drawFileHeader results
                           <| drawFileItems results

    drawFileHeader :: FileResults -> Image
    drawFileHeader (fileName, _) = string fileHeader fileName

    drawFileItems :: FileResults -> Seq Image
    drawFileItems results =
        Seq.zipWith (<|>) (drawLineNumbers results)
                          (drawLinePreviews results)

    drawLineNumbers :: FileResults -> Seq Image
    drawLineNumbers  =
        drawItemsIn _1 (string lineNumber . padWithSpace . show)
                       (string lineNumber . padWithSpace . show)

    drawLinePreviews :: FileResults -> Seq Image
    drawLinePreviews = drawItemsIn _2 (string resultLine)
                                      (string (resultLine <> highlight))

    drawItemsIn :: Lens' Line a
                -> (a -> Image)
                -> (a -> Image)
                -> FileResults
                -> Seq Image
    drawItemsIn lens style highlightStyle (_, items) =
            fmap (style          . view lens) (view pre  items)
         >< fmap (highlightStyle . view lens) (viewAsSeq cur items)
         >< fmap (style          . view lens) (view post items)

    padWithSpace s = ' ' : s ++ " "

resizeToRegion :: DisplayRegion -> ResultsState -> ResultsState
resizeToRegion newRegion = updateScrollPos . set region newRegion

---------------------------------------------------------------------------
-- Lenses and Utilities

viewAsSeq :: Traversable t => Lens' s (t a) -> s -> Seq a
viewAsSeq traversal = views traversal asSeq

asSeq :: Traversable t => t a -> Seq a
asSeq = foldr (<|) empty

filesAbove :: Lens' ResultsState (Seq FileResults)
filesAbove = files . pre

filesBelow :: Lens' ResultsState (Seq FileResults)
filesBelow = files . post

currentFile :: Lens' ResultsState (Maybe FileResults)
currentFile = files . cur

currentFile' :: Traversal' ResultsState FileResults
currentFile' = files . cur . _Just

fileName :: Lens' FileResults String
fileName = _1

lines :: Lens' FileResults (Buffer Line)
lines = _2

linesAbove :: Lens' FileResults (Seq Line)
linesAbove = _2 . pre

linesBelow :: Lens' FileResults (Seq Line)
linesBelow = _2 . post

currentLine :: Lens' FileResults (Maybe Line)
currentLine = _2 . cur

currentLine' :: Traversal' FileResults Line
currentLine' = _2 . cur . _Just

lineNumber :: Lens' Line LineNumber
lineNumber = _1

text :: Lens' Line String
text = _2
