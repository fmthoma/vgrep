module Vgrep.Results.Buffer
    ( module Vgrep.Results
    , DisplayLine(..)
    , Buffer
    , buffer
    , showPrev, showNext
    , hidePrev, hideNext
    , moveUp, moveDown
    , resize
    , toLines
    , current
    , lineNumber
    ) where

import           Control.Applicative
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


type Buffer = ( [FileLineReference]    -- above screen (reversed)
              , Seq FileLineReference  -- top of screen (reversed)
              , FileLineReference      -- currently selected
              , Seq FileLineReference  -- bottom of screen
              , [FileLineReference] )  -- below screen

data DisplayLine = FileHeader   File
                 | Line         LineReference
                 | SelectedLine LineReference
                 deriving (Eq)


buffer :: [FileLineReference] -> Maybe Buffer
buffer (ref : refs) = Just ([], empty, ref, empty, refs)
buffer []           = Nothing

reverse :: Buffer -> Buffer
reverse (as, bs, c, ds, es) = (es, ds, c, bs, as)

showNext :: Buffer -> Maybe Buffer
showNext (as, bs, c, ds, es) = do e:es' <- Just es
                                  Just (as, bs, c, ds |> e, es')
showPrev :: Buffer -> Maybe Buffer
showPrev = fmap reverse . showNext . reverse

hideNext :: Buffer -> Maybe Buffer
hideNext (as, bs, c, ds, es) = do ds' :> d <- Just (viewr ds)
                                  Just (as, bs, c, ds', d:es)

hidePrev :: Buffer -> Maybe Buffer
hidePrev = fmap reverse . hideNext . reverse

moveDown :: Buffer -> Maybe Buffer
moveDown (as, bs, c, ds, es) = do d :< ds' <- Just (viewl ds)
                                  Just (as, c <| bs, d, ds', es)

moveUp :: Buffer -> Maybe Buffer
moveUp = fmap reverse . moveDown . reverse

resize :: Int -> Buffer -> Buffer
resize height buf
    | visibleHeight buf < height - 1 -- FIXME we need some kind of bias
                                     -- to avoid running into an infinite
                                     -- loop, but this leaves some nasty
                                     -- artifacts when scrolling over the
                                     -- last line.
    = maybe buf (resize height) (showNext buf)

    | visibleHeight buf > height
    = maybe buf (resize height) (hidePrev buf <|> hideNext buf)

    | otherwise
    = buf

visibleHeight :: Buffer -> Int
visibleHeight = length . toLines

toLines :: Buffer -> [DisplayLine]
toLines (_, bs, c, ds, _) = linesBefore <> selected c <> linesAfter

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


current :: Buffer -> FileLineReference
current (_, _, c, _, _) = c

lineNumber :: DisplayLine -> Maybe Int
lineNumber (FileHeader _)        = Nothing
lineNumber (Line         (n, _)) = n
lineNumber (SelectedLine (n, _)) = n
