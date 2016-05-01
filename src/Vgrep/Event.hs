{-# LANGUAGE Rank2Types #-}
module Vgrep.Event
    ( Next (..)
    , Redraw (..)
    , Interrupt (..)

    , dispatch
    , dispatchMap

    , module Data.Map
    ) where

import Control.Monad.IO.Class
import Data.Map (Map, fromList)
import qualified Data.Map as M


data Redraw = Redraw | Unchanged

data Interrupt
    = Suspend (forall io. MonadIO io => io ())
    | Halt

data Next a
    = Skip
    | Continue a
    | Interrupt Interrupt


instance Monoid Redraw where
    mempty = Unchanged
    Unchanged `mappend` Unchanged = Unchanged
    _         `mappend` _         = Redraw

instance Monoid (Next a) where
    mempty = Skip
    Skip        `mappend` next       = next
    next        `mappend` _other     = next

instance Functor Next where
    fmap f = \case Skip        -> Skip
                   Continue a  -> Continue (f a)
                   Interrupt i -> Interrupt i

dispatch :: (e -> Maybe e') -> e -> Next e'
dispatch f = maybe Skip Continue . f

dispatchMap :: Ord e => Map e e' -> e -> Next e'
dispatchMap m = dispatch (`M.lookup` m)
