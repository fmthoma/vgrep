{-# LANGUAGE RecordWildCards, Rank2Types #-}
module Vgrep.Widget.HorizontalSplit where

import Control.Lens
import Graphics.Vty hiding (resize)
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type

data HSplitState s t = State { widgets :: (Widget s, Widget t)
                             , focused :: Focus
                             , ratio   :: Rational
                             , region  :: DisplayRegion }

data Focus = FocusLeft | FocusRight

_widgets :: Lens' (HSplitState s t) (Widget s, Widget t)
_widgets = lens widgets
                (\state widgets' -> state { widgets = widgets' })

leftWidget :: Lens' (HSplitState s t) (Widget s)
leftWidget = _widgets . _1

rightWidget :: Lens' (HSplitState s t) (Widget t)
rightWidget = _widgets . _2

type HSplitWidget = forall s t. Widget (HSplitState s t)

hSplitWidget :: Widget s
             -> Widget t
             -> Rational
             -> DisplayRegion
             -> Widget (HSplitState s t)
hSplitWidget left right ratio region =
    Widget { state       = State (left, right) FocusLeft ratio region
           , dimensions  = region
           , resize      = resizeWidgets
           , draw        = drawWidgets
           , handleEvent = passEventToFocusedWidget }


passEventToFocusedWidget :: EventHandler (HSplitState s t)
passEventToFocusedWidget = EventHandler $ \event state@State{..} ->
    case focused of
        FocusLeft  -> passEventTo leftWidget  event state
        FocusRight -> passEventTo rightWidget event state

passEventTo :: Lens' (HSplitState s t) (Widget u)
            -> Event
            -> HSplitState s t
            -> IO (Next (HSplitState s t))
passEventTo selector event =
    over (liftNext selector) (handle passEventsToWidget event)

liftNext :: Lens' s a
         -> Lens s (IO (Next s)) a (IO (Next a))
liftNext l = lens (view l)
                  (\s -> fmap (fmap (\a -> set l a s)))

resizeWidgets :: DisplayRegion -> HSplitState s t -> HSplitState s t
resizeWidgets newRegion@(w, h) state@State{..} =
    let leftRegion  = (ceiling (ratio * fromIntegral w), h)
        rightRegion = (floor   (ratio * fromIntegral w), h)
    in  state { widgets = ( resizeWidget (fst widgets) leftRegion
                          , resizeWidget (snd widgets) rightRegion )
              , region = newRegion }

drawWidgets :: HSplitState s t -> Image
drawWidgets state@State{..} = drawWidget (view leftWidget  state)
                          <-> drawWidget (view rightWidget state)
