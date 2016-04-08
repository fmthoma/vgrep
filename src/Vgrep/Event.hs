{-# LANGUAGE Rank2Types #-}
module Vgrep.Event
    ( Next (..)
    , Redraw (..)
    , Interrupt (..)

    , handle

    , keyEvent
    , keyCharEvent
    , resizeEvent
    ) where

import Control.Monad.State.Extended (StateT)
import Control.Monad.IO.Class
import qualified Graphics.Vty as Vty
import Graphics.Vty.Prelude

import Vgrep.Type


data Redraw = Redraw | Unchanged

instance Monoid Redraw where
    mempty = Unchanged
    Unchanged `mappend` Unchanged = Unchanged
    Redraw `mappend` _ = Redraw
    _ `mappend` Redraw = Redraw

data Interrupt = Suspend (forall io. MonadIO io => io ())
               | Halt

data Next s m = Skip
              | Continue (StateT s (VgrepT m) Redraw)
              | Interrupt Interrupt

instance Monoid (Next s m) where
    mempty = Skip
    Skip `mappend` next   = next
    next `mappend` _other = next
    

handle :: Monad m
       => (e -> Maybe e')
       -> (e' -> Next s m)
       -> (e  -> Next s m)
handle select action event = case select event of
    Just event' -> action event'
    Nothing     -> Skip


keyEvent :: Vty.Key -> [Vty.Modifier] -> Vty.Event -> Maybe ()
keyEvent k ms = \case
    Vty.EvKey k' ms' | (k', ms') == (k, ms) -> Just ()
    _otherwise                              -> Nothing

keyCharEvent :: Char -> [Vty.Modifier] -> Vty.Event -> Maybe ()
keyCharEvent c = keyEvent (Vty.KChar c)

resizeEvent :: Vty.Event -> Maybe DisplayRegion
resizeEvent = \case
    Vty.EvResize w h -> Just (w, h)
    _otherwise       -> Nothing
