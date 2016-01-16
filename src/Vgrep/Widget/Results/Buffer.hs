module Vgrep.Widget.Results.Buffer
    ( File(..)
    , LineReference
    , FileLineReference
    , Buffer
    , buffer
    , showPrev, showNext
    , hidePrev, hideNext
    , moveUp, moveDown
    , resize
    ) where

import           Control.Applicative
import           Data.Sequence ( Seq , (<|), (|>)
                               , ViewL(..), ViewR(..)
                               , viewl, viewr )
import           Data.Foldable
import           Data.List (nub)
import           Data.Monoid
import           Data.Text (Text)
import           Prelude hiding (reverse)


newtype File = File { getFileName :: Text } deriving (Eq)
type LineReference = (Maybe Int, Text)
type FileLineReference = (File, LineReference)

type Buffer = ( [FileLineReference]    -- above screen (reversed)
              , Seq FileLineReference  -- top of screen (reversed)
              , FileLineReference      -- currently selected
              , Seq FileLineReference  -- bottom of screen
              , [FileLineReference] )  -- below screen

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
    | visibleHeight buf < height
    = maybe buf (resize height) (showNext buf)

    | visibleHeight buf > height
    = maybe buf (resize height) (hidePrev buf <|> hideNext buf)

    | otherwise
    = buf

visibleHeight :: Buffer -> Int
visibleHeight buf = length . toLines

toLines :: Buffer -> [Either File LineReference]
toLines buffer = go . toList . visibleLineReferences
  where go refs = do
    fileResults <- groupBy ((==) `on` fst) refs
    Left (fst (head fileResults)) : fmap (Right . snd) fileResults

visibleLineReferences :: Buffer -> Seq LineReference
visibleLineReferences (_, bs, c, ds, _) = bs <> (c <| ds)
