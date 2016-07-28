{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Vgrep.Search (
      Pattern
    , Searchable (..)
    , SearchableF (..)
    , Zipper (..)
    , Highlighted ()
    , render
    ) where

import           Data.Functor.Identity
import           Data.List
import           Data.Sequence         (Seq, ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Sequence         as Seq
import qualified Data.Text             as StrictText
import qualified Data.Text.Lazy        as LazyText

type Pattern = StrictText.Text -- FIXME for lack of a better definition

data Zipper a
    = ZEmpty
    | Zipper (Seq a) a (Seq a)

class Searchable a where
    match     :: Pattern -> a -> Bool
    highlight :: Pattern -> a -> Highlighted a

    next :: Pattern -> Zipper a -> Zipper a
    next pattern = \case
        ZEmpty -> ZEmpty
        Zipper as b cs -> case Seq.viewl cs of
            Seq.EmptyL -> Zipper as b cs
            c' :< cs' | match pattern c' -> Zipper (as |> b) c' cs'
                      | otherwise        -> next pattern (Zipper (as |> b) c' cs')

    prev :: Pattern -> Zipper a -> Zipper a
    prev pattern = \case
        ZEmpty -> ZEmpty
        Zipper as b cs -> case Seq.viewr as of
            Seq.EmptyR -> Zipper as b cs
            as' :> a' | match pattern a' -> Zipper as' a' (b <| cs)
                      | otherwise        -> prev pattern (Zipper as' a' (b <| cs))

class SearchableF f a where
    matchF     :: Pattern -> f a -> Bool
    highlightF :: Pattern -> f a -> f (Highlighted a)

    nextF      :: Pattern -> Zipper (f a) -> Zipper (f a)
    nextF pattern = \case
        ZEmpty -> ZEmpty
        Zipper as b cs -> case Seq.viewl cs of
            Seq.EmptyL -> Zipper as b cs
            c' :< cs' | matchF pattern c' -> Zipper (as |> b) c' cs'
                      | otherwise        -> nextF pattern (Zipper (as |> b) c' cs')

    prevF      :: Pattern -> Zipper (f a) -> Zipper (f a)
    prevF pattern = \case
        ZEmpty -> ZEmpty
        Zipper as b cs -> case Seq.viewr as of
            Seq.EmptyR -> Zipper as b cs
            as' :> a' | matchF pattern a' -> Zipper as' a' (b <| cs)
                      | otherwise        -> prevF pattern (Zipper as' a' (b <| cs))

instance Searchable a => SearchableF Identity a where
    matchF     pattern (Identity a) = match pattern a
    highlightF pattern (Identity a) = Identity (highlight pattern a)

instance Searchable LazyText.Text where
    match pattern text = (LazyText.fromStrict pattern) `LazyText.isInfixOf` text
    highlight pattern text
        | match pattern text = Highlighted $
            let pattern' = LazyText.fromStrict pattern
                chunks = LazyText.splitOn pattern' text
            in  intersperse (Highlight pattern') (fmap Normal chunks)
        | otherwise = Highlighted [Normal text]

instance Searchable StrictText.Text where
    match = StrictText.isInfixOf
    highlight pattern text
        | match pattern text = Highlighted $
            let chunks = StrictText.splitOn pattern text
            in  intersperse (Highlight pattern) (fmap Normal chunks)
        | otherwise = Highlighted [Normal text]


newtype Highlighted a = Highlighted [Highlight a]
    deriving (Show, Functor)

data Highlight a
    = Normal    a
    | Highlight a
    deriving (Show, Functor)

render
    :: Monoid b
    => (a -> b) -- ^ normal
    -> (a -> b) -- ^ highlighted
    -> (b -> b -> b) -- ^ mappend
    -> b             -- ^ mempty
    -> Highlighted a
    -> b
render renderNormal renderHighlighted append empty (Highlighted things) =
    foldr append empty (map go things)
  where
    go = \case
        Normal    thing -> renderNormal      thing
        Highlight thing -> renderHighlighted thing
