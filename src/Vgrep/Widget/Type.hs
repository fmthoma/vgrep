{-# LANGUAGE Rank2Types #-}
module Vgrep.Widget.Type
  ( Widget (..)

  , module Vgrep.Event
  ) where

import Control.Monad.State.Extended
import Graphics.Vty.Image (Image)
import Graphics.Vty.Input

import Vgrep.Event (Redraw (..))
import Vgrep.Type

data Widget s = Widget
    { initialize :: s
    , draw       :: forall m. Monad m =>          StateT s (VgrepT m) Image
    , handle     :: forall m. Monad m => Event -> StateT s (VgrepT m) Redraw }
