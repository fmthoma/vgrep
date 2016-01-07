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

focusedWidget :: Lens' (HSplitState s) (Widget s)
focusedWidget = lens widgets (\state widgets' -> state { widgets = widgets' }) . _1

focusedWidget' :: Lens (HSplitState s) (IO (Next (HSplitState s))) (Widget s) (IO (Next (Widget s)))
focusedWidget' = lens (view focusedWidget) (\state -> fmap (fmap (\w -> set focusedWidget w state)))

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
passEventToFocusedWidget = EventHandler $ \event ->
    over focusedWidget' (handle passEventsToWidget event)
