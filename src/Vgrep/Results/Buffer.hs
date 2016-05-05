module Vgrep.Results.Buffer
    ( module Vgrep.Results
    , DisplayLine(..)
    , Buffer()
    , emptyBuffer
    , feed
    , showPrev, showNext
    , hidePrev, hideNext
    , moveUp, moveDown
    , resize
    , toLines
    , lineNumber
    , current
    , currentFile
    ) where

import           Control.Applicative
import           Data.Maybe
import           Data.Sequence ( Seq , (<|), (|>)
                               , ViewL(..), ViewR(..)
                               , viewl, viewr )
import qualified Data.Sequence as S
import           Data.Foldable
import           Data.Function
import           Data.List (groupBy)
import           Data.Monoid
import           Prelude hiding (reverse)

import Vgrep.Results


data Buffer
    = EmptyBuffer
    | Buffer
        (Seq FileLineReference) -- above screen (reversed)
        (Seq FileLineReference) -- top of screen (reversed)
        FileLineReference       -- currently selected
        (Seq FileLineReference) -- bottom of screen
        (Seq FileLineReference) -- below screen

emptyBuffer :: Buffer
emptyBuffer = EmptyBuffer

data DisplayLine = FileHeader   File
                 | Line         LineReference
                 | SelectedLine LineReference
                 deriving (Eq)


feed :: FileLineReference -> Buffer -> Buffer
feed l = \case
    EmptyBuffer          -> Buffer empty empty l empty empty
    Buffer as bs c ds es -> Buffer as bs c ds (es |> l)


reverse :: Buffer -> Buffer
reverse = \case
    Buffer as bs c ds es -> Buffer es ds c bs as
    EmptyBuffer          -> EmptyBuffer

showNext :: Buffer -> Maybe Buffer
showNext = \case
    Buffer as bs c ds es -> do e :< es' <- Just (viewl es)
                               Just (Buffer as bs c (ds |> e) es')
    EmptyBuffer          -> Nothing

showPrev :: Buffer -> Maybe Buffer
showPrev = fmap reverse . showNext . reverse

hideNext :: Buffer -> Maybe Buffer
hideNext = \case
    Buffer as bs c ds es -> do ds' :> d <- Just (viewr ds)
                               Just (Buffer as bs c ds' (d <| es))
    EmptyBuffer          -> Nothing

hidePrev :: Buffer -> Maybe Buffer
hidePrev = fmap reverse . hideNext . reverse

moveDown :: Buffer -> Maybe Buffer
moveDown = \case
    Buffer as bs c ds es -> do d :< ds' <- Just (viewl ds)
                               Just (Buffer as (c <| bs) d ds' es)
    EmptyBuffer          -> Nothing

moveUp :: Buffer -> Maybe Buffer
moveUp = fmap reverse . moveDown . reverse

resize :: Int -> Buffer -> Maybe Buffer
resize height buffer
    | visibleHeight buffer < height - 1 = Just (doResize buffer)
    | visibleHeight buffer > height     = Just (doResize buffer)
    | otherwise                         = Nothing
  where
    doResize buf
        -- FIXME we need some kind of bias
        -- to avoid running into an infinite
        -- loop, but this leaves some nasty
        -- artifacts when scrolling over the
        -- last line. -----------------v
        | visibleHeight buf < height - 1
        = maybe buf doResize (showNext buf <|> showPrev buf)

        | visibleHeight buf > height
        = maybe buf doResize (hidePrev buf <|> hideNext buf)

        | otherwise
        = buf

visibleHeight :: Buffer -> Int
visibleHeight = length . toLines

toLines :: Buffer -> [DisplayLine]
toLines EmptyBuffer          = []
toLines (Buffer _ bs c ds _) = linesBefore <> selected c <> linesAfter

  where
    linesBefore = case viewl bs of
        b :< _ | b `pointsToSameFile` c -> go (S.reverse bs)
        _otherwise                      -> go (S.reverse bs) <> header c

    linesAfter = case viewl ds of
        d :< _ | c `pointsToSameFile` d -> drop 1 (go ds)
        _otherwise                      -> go ds

    go refs = do
        fileResults <- groupBy pointsToSameFile (toList refs)
        header (head fileResults) <> fmap (Line . snd) fileResults

    header   = pure . FileHeader   . fst
    selected = pure . SelectedLine . snd
    pointsToSameFile = (==) `on` fst

lineNumber :: DisplayLine -> Maybe Int
lineNumber (FileHeader _)        = Nothing
lineNumber (Line         (n, _)) = n
lineNumber (SelectedLine (n, _)) = n


current :: Buffer -> Maybe FileLineReference
current = \case
    Buffer _ _ c _ _ -> Just c
    EmptyBuffer      -> Nothing

currentFile :: Buffer -> [LineReference]
currentFile = do
    let sameFileAs = (==) `on` fst
    inCurrentFile <- sameFileAs . fromJust . current
    map snd . filter inCurrentFile . bufferToList

bufferToList :: Buffer -> [FileLineReference]
bufferToList = \case
    EmptyBuffer                -> []
    Buffer as bs c ds es -> toList (as <> bs <> pure c <> ds <> es)
