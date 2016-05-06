{-# LANGUAGE Rank2Types #-}
module Vgrep.Widget.Type
  ( Widget (..)

  -- ** Re-exports from "Vgrep.Event"
  , Redraw (..)
  , Next (..)
  ) where

import Graphics.Vty.Image (Image)
import Graphics.Vty.Input

import Vgrep.Event (Redraw (..), Next (..))
import Vgrep.Type

-- | A 'Widget' is a unit that is displayed on the screen. It is associated
-- with a mutable state @s@. It provides an event handler with default
-- keybindings and can generate a renderable 'Image'.
--
-- Widget modules should provide a 'Widget' instance and additionally a
-- collection of actions that can be invoked by external event handlers:
--
-- @
-- widgetAction :: 'VgrepT' s m 'Redraw'
-- @
data Widget s = Widget
    { initialize :: s
    -- ^ The initial state of the widget

    , draw       :: forall m. Monad m => VgrepT s m Image
    -- ^ Generate a renderable 'Image' from the widget state. The state can
    -- be modified (e. g. for resizing).

    , handle     :: forall m. Monad m => Event -> s -> Next (VgrepT s m Redraw)
    -- ^ The default event handler for this 'Widget'. May provide e.g.
    -- default keybindings.
    }
