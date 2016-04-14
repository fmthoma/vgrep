{-# LANGUAGE Rank2Types #-}
module Vgrep.Event
    ( Next (..)
    , Redraw (..)
    , Interrupt (..)
    ) where

import Control.Monad.IO.Class


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
