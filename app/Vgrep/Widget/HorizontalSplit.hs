{-# LANGUAGE ExistentialQuantification, RecordWildCards, Rank2Types #-}
module Vgrep.Widget.HorizontalSplit where

import Control.Lens
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type

data HSplitState s = State { widgets :: (Widget s, Widget s)
                         , focused :: Lens' (Widget s, Widget s) (Widget s)
                         , ratio   :: Rational
                         , region  :: DisplayRegion }

type HSplitWidget s = Widget (HSplitState s)

hSplitWidget :: Widget s
             -> Widget s
             -> Rational
             -> DisplayRegion
             -> HSplitWidget s
hSplitWidget left right ratio region =
    Widget { state       = State (left, right) _1 ratio region
           , dimensions  = region
           , resize      = undefined
           , draw        = undefined
           , handleEvent = passEventToFocusedWidget }


passEventToFocusedWidget :: EventHandler (HSplitState s)
passEventToFocusedWidget = EventHandler $ \state@State{..} event -> do
    let widget = view focused widgets
    next <- handle passEventsToWidget widget event
    return $ fmap (updateFocusedWidget state) next

updateFocusedWidget :: HSplitState s -> Widget s -> HSplitState s
updateFocusedWidget state@State{..} updatedWidget =
    state { widgets = set focused updatedWidget widgets }
