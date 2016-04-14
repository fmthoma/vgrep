{-# LANGUAGE Rank2Types #-}
module Vgrep.Widget.Type
  ( Widget (..)
  , delegate
  , module Vgrep.Event
  ) where

import Control.Monad.State.Extended
import Graphics.Vty.Image (Image)

import Vgrep.Event (Redraw (..))
import Vgrep.Type

data Widget e s = Widget
    { initialize :: s
    , draw       :: forall m. Monad   m => s -> VgrepT m Image
    , handle     :: forall m. MonadIO m => e -> StateT s (VgrepT m) Redraw }

delegate :: (e -> Maybe e')
         -> Widget e' s
         -> Widget e  s
delegate select widget = widget
    { handle = \e -> case select e of
        Just e' -> handle widget e'
        Nothing -> pure Unchanged }
