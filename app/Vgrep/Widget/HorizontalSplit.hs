{-# LANGUAGE RecordWildCards, Rank2Types #-}
module Vgrep.Widget.HorizontalSplit where

import Control.Lens
import Data.Monoid
import Graphics.Vty hiding (resize)
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type

data HSplitState s t = State { widgets :: (s, t)
                             , focused :: Focus
                             , ratio   :: Rational
                             , region  :: DisplayRegion }

data Focus = FocusLeft | FocusRight

_widgets :: Lens' (HSplitState s t) (s, t)
_widgets = lens widgets
                (\state widgets' -> state { widgets = widgets' })

leftWidget :: Lens' (HSplitState s t) s
leftWidget = _widgets . _1

rightWidget :: Lens' (HSplitState s t) t
rightWidget = _widgets . _2

type HSplitWidget s t = Widget (HSplitState s t)

hSplitWidget :: Widget s
             -> Widget t
             -> Rational
             -> DisplayRegion
             -> HSplitWidget (Widget s) (Widget t)
hSplitWidget left right ratio region =
    Widget { state       = initState left right ratio region
           , dimensions  = region
           , resize      = resizeWidgets
           , draw        = drawWidgets
           , handleEvent = switchFocusOn (KChar '\t')
                        <> passEventToFocusedWidget }

initState :: Widget s
          -> Widget t
          -> Rational
          -> DisplayRegion
          -> HSplitState (Widget s) (Widget t)
initState left right ratio region = resizeWidgets region $
    State { widgets = (left, right)
          , focused = FocusLeft
          , ratio   = ratio
          , region  = region }


switchFocusOn :: Key -> EventHandler (HSplitState s t)
switchFocusOn key = handleKey key [] $
    \state@State{..} -> state { focused = switch focused }
  where
    switch FocusLeft  = FocusRight
    switch FocusRight = FocusLeft

passEventToFocusedWidget :: EventHandler (HSplitState (Widget s) (Widget t))
passEventToFocusedWidget = EventHandler $ \event state@State{..} ->
    case focused of
        FocusLeft  -> passEventTo leftWidget  event state
        FocusRight -> passEventTo rightWidget event state

passEventTo :: Lens' (HSplitState (Widget s) (Widget t)) (Widget u)
            -> Event
            -> HSplitState (Widget s) (Widget t)
            -> Next (HSplitState (Widget s) (Widget t))
passEventTo selector event =
    over (liftNext selector) (handle passEventsToWidget event)

liftNext :: Lens' s a
         -> Lens s (Next s) a (Next a)
liftNext l = lens (view l)
                  (\s -> fmap (\a -> set l a s))

resizeWidgets :: DisplayRegion
              -> HSplitState (Widget s) (Widget t)
              -> HSplitState (Widget s) (Widget t)
resizeWidgets newRegion@(w, h) state@State{..} =
    let leftRegion  = (ceiling (ratio * fromIntegral w), h)
        rightRegion = (floor ((1 - ratio) * fromIntegral w), h)
    in  state { widgets = ( resizeWidget (fst widgets) leftRegion
                          , resizeWidget (snd widgets) rightRegion )
              , region = newRegion }

drawWidgets :: HSplitState (Widget s) (Widget t) -> Image
drawWidgets state@State{..} = drawWidget (view leftWidget  state)
                          <|> drawWidget (view rightWidget state)
