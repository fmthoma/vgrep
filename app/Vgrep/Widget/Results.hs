{-# LANGUAGE Rank2Types, TemplateHaskell, DisambiguateRecordFields, MultiWayIf #-}
module Vgrep.Widget.Results where

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

currentFile :: Traversal' ResultsState (Buffer Line)
currentFile = files . cur . _Just . _2

previousLine :: ResultsState -> ResultsState
previousLine state = case preview (currentFile . pre . viewR) state of
    Nothing        -> state
    Just EmptyR    -> previousFile state
    Just (ls :> l) -> state & set  (currentFile . pre) ls
                            & set  (currentFile . cur) (Just l)
                            & over (currentFile . post)
                                   (views (currentFile . cur) asSeq state ><)
                            & updateScrollPos

previousFile :: ResultsState -> ResultsState
previousFile state = case preview (files . pre . viewR) state of
    Nothing        -> state
    Just EmptyR    -> state
    Just (fs :> f) -> state & set  (files . pre) fs
                            & set  (files . cur) (Just f)
                            & over (files . post)
                                   (views (files . cur) asSeq state ><)
                            & updateScrollPos

nextLine :: ResultsState -> ResultsState
nextLine state = case preview (currentFile . post . viewL) state of
    Nothing        -> state
    Just EmptyL    -> nextFile state
    Just (l :< ls) -> state & over (currentFile . pre)
                                   (>< views (currentFile . cur) asSeq state)
                            & set  (currentFile . cur) (Just l)
                            & set  (currentFile . post) ls
                            & updateScrollPos

nextFile :: ResultsState -> ResultsState
nextFile state = case preview (files . post . viewL) state of
    Nothing        -> state
    Just EmptyL    -> state
    Just (f :< fs) -> state & over (files . pre)
                                   (>< views (files . cur) asSeq state)
                            & set  (files . cur) (Just f)
                            & set  (files . post) fs
                            & updateScrollPos


updateScrollPos :: ResultsState -> ResultsState
updateScrollPos state =
    if | current < firstVisible -> state & set scrollPos current
       | current > lastVisible  -> state & set scrollPos (current - height + 1)
       | otherwise              -> state
  where
    height       = regionHeight (view region state)
    current      = computeCurrentItem (view files state)
    firstVisible = view scrollPos state
    lastVisible  = firstVisible + height - 1

computeCurrentItem :: Buffer FileResults -> Int
computeCurrentItem buffer = linesInFilesBeforeCurrent
                          + 2 * fileHeadersBeforeCurrent + 1
                          + linesInCurrentFileBeforeCursor
  where
    fileHeadersBeforeCurrent = length (view pre buffer)
    linesInFilesBeforeCurrent =
        (sum . fmap length) (view (pre . traverse . _2 . pre) buffer)
    linesInCurrentFileBeforeCursor =
        (sum . fmap length) (view (cur . traverse . _2 . pre) buffer)


drawResultList :: ResultsState -> Image
drawResultList state =
    fold . Seq.take height
         . Seq.drop pos
         $  foldMap drawFileResults        (view (files . pre)  state)
         >< foldMap drawCurrentFileResults (views (files . cur) asSeq state)
         >< foldMap drawFileResults        (view (files . post) state)
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

    drawCurrentFileResults :: FileResults -> Seq Image
    drawCurrentFileResults results = drawFileHeader results
                                  <| drawCurrentFileItems results

    drawFileHeader :: FileResults -> Image
    drawFileHeader (fileName, _) = string fileHeader fileName

    drawFileItems :: FileResults -> Seq Image
    drawFileItems results =
        Seq.zipWith (<|>) (drawLineNumbers results)
                          (drawLinePreviews results)

    drawCurrentFileItems :: FileResults -> Seq Image
    drawCurrentFileItems results =
        Seq.zipWith (<|>) (drawLineNumbers results)
                          (drawCurrentFileLinePreviews results)

    drawLineNumbers :: FileResults -> Seq Image
    drawLineNumbers  =
        drawItemsIn _1 (string lineNumber . padWithSpace . show)
                       (string lineNumber . padWithSpace . show)

    drawLinePreviews :: FileResults -> Seq Image
    drawLinePreviews = drawItemsIn _2 (string resultLine)
                                      (string resultLine)

    drawCurrentFileLinePreviews :: FileResults -> Seq Image
    drawCurrentFileLinePreviews =
        drawItemsIn _2 (string resultLine)
                       (string (resultLine <> highlight))

    drawItemsIn :: Lens' Line a
                -> (a -> Image)
                -> (a -> Image)
                -> FileResults
                -> Seq Image
    drawItemsIn lens style highlightStyle (_, items) =
            fmap (style          . view lens) (view pre  items)
         >< fmap (highlightStyle . view lens) (views cur asSeq items)
         >< fmap (style          . view lens) (view post items)

    padWithSpace s = ' ' : s ++ " "

resizeToRegion :: DisplayRegion -> ResultsState -> ResultsState
resizeToRegion newRegion = updateScrollPos . set region newRegion

asSeq :: Traversable t => t a -> Seq a
asSeq = foldr (<|) empty
