{-# LANGUAGE Rank2Types #-}
module Vgrep.Widget.Type
  ( Widget (..)

  , module Vgrep.Event
  ) where

import Graphics.Vty.Image (Image)
import Graphics.Vty.Input

import Vgrep.Event (Redraw (..), Next (..))
import Vgrep.Type

data Widget s = Widget
    { initialize :: s
    , draw       :: forall m. Monad m => VgrepT s m Image
    , handle     :: forall m. Monad m => Event -> s -> Next (VgrepT s m Redraw) }
