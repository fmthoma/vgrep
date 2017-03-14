{-# LANGUAGE Rank2Types #-}
module Vgrep.Widget.Type
  ( Widget (..)
  , Cursor (..)

  -- ** Re-exports from "Vgrep.Event"
  , Redraw (..)
  , Next (..)

  -- ** Re-exports from "Graphics.Vty"
  , Image ()
  ) where

import Graphics.Vty.Image   (Image)

import Vgrep.Event (Next (..), Redraw (..))
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

    , cursor     :: forall m. Monad m => VgrepT s m Cursor
    -- ^ Get the current cursor position, or 'NoCursor' if not applicable.
    }

-- | Position for the cursor on the screen. Translates to
-- 'Graphics.Vty.Picture.Cursor'.
data Cursor
    = NoCursor
    -- ^ Don't show a cursor.
    | Cursor
        { col :: !Int
        , row :: !Int }
    -- ^ Show a cursor at a relative offset of ('col', 'row') from the origin
