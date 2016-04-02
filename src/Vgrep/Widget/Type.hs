{-# LANGUAGE Rank2Types #-}
module Vgrep.Widget.Type
  ( Widget (..)
  , module Vgrep.Event
  ) where

import Control.Monad.State.Extended
import Graphics.Vty.Image (Image)

import Vgrep.Event (Redraw (..))
import Vgrep.Type

data Widget e s = Widget
    { initialize :: s
    , draw       :: forall m. Monad m => s -> VgrepT m Image
    , handle     :: forall m. Monad m => e -> StateT s (VgrepT m) Redraw }
