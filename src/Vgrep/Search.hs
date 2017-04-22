{-# LANGUAGE OverloadedStrings #-}
module Vgrep.Search
    ( regex
    , matches

    , highlight
    , highlight'

    , Regex
    ) where

import Data.Array
import Data.Maybe
import Data.Text
import Data.Tuple
import Text.Regex.Base
import Text.Regex.TDFA.Text

import Vgrep.Ansi

regex :: Text -> Regex
regex = either error id . compile defaultCompOpt defaultExecOpt

matches :: Regex -> Text -> Bool
matches needle haystack = isJust (matchOnce needle haystack)


nextMatch needle = go
  where
    go = \case
        (ls, []) -> (ls, [])
        (ls, r:rs) -> if matches needle r
          then (ls, r:rs)
          else go (r:ls, rs)

prevMatch :: Regex -> ([Text], [Text]) -> ([Text], [Text])
prevMatch needle = swap . nextMatch needle . swap

highlight :: (Eq attr, Monoid attr) => attr -> Regex -> Text -> Formatted attr
highlight attr needle haystack = case matchOnceText needle haystack of
    Nothing -> bare haystack
    Just (pre, m, post) -> cat
        [ bare pre
        , format attr (bare (fst (m ! 0)))
        , highlight attr needle post ]

highlight' :: (Eq attr, Monoid attr) => attr -> Regex -> Formatted attr -> Formatted attr
highlight' attr needle haystack = case matchOnce needle (stripAnsi haystack) of
    Nothing -> haystack
    Just m -> let (matchOffset, matchLength) = m ! 0
                  pre  = takeFormatted matchOffset haystack
                  mt   = takeFormatted matchLength (dropFormatted matchOffset haystack)
                  post = highlight' attr needle (dropFormatted (matchOffset + matchLength) haystack)
              in  cat [pre, format attr mt, post]
