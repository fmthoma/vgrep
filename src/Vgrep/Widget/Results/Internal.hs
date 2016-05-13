module Vgrep.Widget.Results.Internal ( 
    -- * Results widget state
      Results (..)

    -- * Lenses
    , currentFileName
    , currentLineNumber
    , currentFileResultLineNumbers

    -- * Actions
    -- | In general, actions return @'Just' newResults@ if the buffer has
    -- changed, and @'Nothing'@ otherwise. This way it is easy to recognize
    -- whether or not a 'Vgrep.Event.Redraw' is necessary.
    , feed
    , showPrev, showNext
    , hidePrev, hideNext
    , moveUp, moveDown
    , resize

    -- * Utilities for displaying
    , DisplayLine(..)
    , toLines
    , lineNumber
    ) where

import           Control.Applicative
import           Control.Lens        (Getter, pre, to, _Just)
import           Data.Foldable
import           Data.Function
import           Data.List           (groupBy)
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence       (Seq, ViewL (..), ViewR (..), viewl, viewr,
                                      (<|), (|>))
import qualified Data.Sequence       as S
import           Data.Text.Lazy (Text)
import           Prelude             hiding (reverse)

import Vgrep.Results


-- | Results widget state
data Results
    = EmptyResults
    -- ^ The results list is empty

    | Results
        (Seq FileLineReference) -- above screen (reversed)
        (Seq FileLineReference) -- top of screen (reversed)
        FileLineReference       -- currently selected
        (Seq FileLineReference) -- bottom of screen
        (Seq FileLineReference) -- below screen
    -- ^ The structure of the Results buffer is a double Zipper:
    --
    -- * lines above the current screen
    -- * lines on screen above the current item
    -- * the current item
    -- * lines on screen below the current item
    -- * lines below the current screen


-- | Append a line to the 'Results'. The line is appended below the visible
-- screen, so use 'showNext' to make it visible.
feed :: FileLineReference -> Results -> Results
feed l = \case
    EmptyResults          -> Results empty empty l empty empty
    Results as bs c ds es -> Results as bs c ds (es |> l)


-- | Reverse the 'Results'
reverse :: Results -> Results
reverse = \case
    Results as bs c ds es -> Results es ds c bs as
    EmptyResults          -> EmptyResults

-- | Show one more item at the bottom of the screen if available.
showNext :: Results -> Maybe Results
showNext = \case
    Results as bs c ds es -> do e :< es' <- Just (viewl es)
                                Just (Results as bs c (ds |> e) es')
    EmptyResults          -> Nothing

-- | Show one more item at the top of the screen if available.
showPrev :: Results -> Maybe Results
showPrev = fmap reverse . showNext . reverse

-- | Remove the last item from the bottom of the screen and prepend it to
-- the invisible items below.
hideNext :: Results -> Maybe Results
hideNext = \case
    Results as bs c ds es -> do ds' :> d <- Just (viewr ds)
                                Just (Results as bs c ds' (d <| es))
    EmptyResults          -> Nothing

-- | Remove the first item from the top of the screen and append it to the
-- invisible items above.
hidePrev :: Results -> Maybe Results
hidePrev = fmap reverse . hideNext . reverse

-- | Move the cursor one item down.
moveDown :: Results -> Maybe Results
moveDown = \case
    Results as bs c ds es -> do d :< ds' <- Just (viewl ds)
                                Just (Results as (c <| bs) d ds' es)
    EmptyResults          -> Nothing

-- | Move the cursor one item up.
moveUp :: Results -> Maybe Results
moveUp = fmap reverse . moveDown . reverse

-- | Adjust the number of on-screen items to the given height:
--
-- * If the current list is too long for the new height, take items from
-- the top until the current item is topmost, then from the bottom.
-- * If the current list is too short for the new height, add items below
-- until the buffer is empty, then above.
resize
    :: Int           -- ^ the new height
    -> Results
    -> Maybe Results -- ^ @'Nothing'@ if the height has not changed,
                     -- @'Just' newResults@ otherwise
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

visibleHeight :: Results -> Int
visibleHeight = length . toLines


-- | Ad-hoc data structure to render the (visible) 'Results' as list of
-- lines.
data DisplayLine = FileHeader   File
                 | Line         LineReference
                 | SelectedLine LineReference
                 deriving (Eq)

-- | Converts the visible 'Results' to a list of 'DisplayLine's.  Each item
-- in the returned list corresponds to a line on the screen.
--
-- Each group of 'Line's that points to the same file is prepended with
-- a 'FileHeader'. The item below the cursor becomes a 'SelectedLine'.
toLines :: Results -> [DisplayLine]
toLines EmptyResults          = []
toLines (Results _ bs c ds _) = linesBefore <> selected c <> linesAfter

  where
    linesBefore = case viewl bs of
        b :< _ | b `pointsToSameFile` c -> go (S.reverse bs)
        _otherwise                      -> go (S.reverse bs) <> header c

    linesAfter = case viewl ds of
        d :< _ | c `pointsToSameFile` d -> drop 1 (go ds)
        _otherwise                      -> go ds

    go refs = do
        fileResults <- groupBy pointsToSameFile (toList refs)
        header (head fileResults) <> fmap (Line . getLineReference) fileResults

    header   = pure . FileHeader   . getFile
    selected = pure . SelectedLine . getLineReference
    pointsToSameFile = (==) `on` getFile

-- | The line number of a 'DisplayLine'. 'Nothing' for 'FileHeader's.
lineNumber :: DisplayLine -> Maybe Int
lineNumber = \case
    FileHeader _                     -> Nothing
    Line         (LineReference n _) -> n
    SelectedLine (LineReference n _) -> n


-- | The file name of the currently selected item
currentFileName :: Getter Results (Maybe Text)
currentFileName =
    pre (to current . _Just . to getFile . to getFileName)

-- | The line number of the currently selected item
currentLineNumber :: Getter Results (Maybe Int)
currentLineNumber =
    pre (to current . _Just . to getLineReference . to getLineNumber . _Just)

current :: Results -> Maybe FileLineReference
current = \case
    Results _ _ c _ _ -> Just c
    EmptyResults      -> Nothing

-- | The line numbers with matches in the file of the currentliy selected
-- item
currentFileResultLineNumbers :: Getter Results [Int]
currentFileResultLineNumbers =
    to (mapMaybe getLineNumber . currentFile)
  where
    currentFile = do
        let sameFileAs = (==) `on` getFile
        inCurrentFile <- sameFileAs . fromJust . current
        map getLineReference . filter inCurrentFile . bufferToList

bufferToList :: Results -> [FileLineReference]
bufferToList = \case
    EmptyResults          -> []
    Results as bs c ds es -> toList (as <> bs <> pure c <> ds <> es)
