{-# LANGUAGE Rank2Types #-}
module Vgrep.Event
    ( Next (..)
    , Redraw (..)
    , Interrupt (..)

    , (==>)
    , dispatch
    ) where

import Control.Monad.IO.Class
import Data.Monoid (First (..))


data Redraw = Redraw | Unchanged

data Interrupt = Suspend (forall io. MonadIO io => io ())
               | Halt

data Next = Skip
          | Continue Redraw
          | Interrupt Interrupt


instance Monoid Redraw where
    mempty = Unchanged
    Unchanged `mappend` Unchanged = Unchanged
    Redraw    `mappend` _         = Redraw
    _         `mappend` Redraw    = Redraw

instance Monoid Next where
    mempty = Skip
    Continue l  `mappend` Continue r = Continue (l `mappend` r)
    Skip        `mappend` next       = next
    next        `mappend` _other     = next

(==>) :: Eq e => e -> e' -> e -> First e'
(==>) expected trigger actual | expected == actual = First (Just trigger)
                              | otherwise          = First Nothing

dispatch :: [e -> First e'] -> e -> Maybe e'
dispatch = fmap getFirst . mconcat
