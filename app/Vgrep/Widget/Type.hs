module Vgrep.Widget.Type where

import Graphics.Vty

import Vgrep.Event

data Widget e s = Widget { state       :: s
                         , dimensions  :: DisplayRegion
                         , resize      :: DisplayRegion -> s -> s
                         , draw        :: s -> Image
                         , handleEvent :: EventHandler e s }

passEventsToWidget :: EventHandler e (Widget e s)
passEventsToWidget = EventHandler $ \widget event -> do
    next <- handle (handleEvent widget) (state widget) event
    return (fmap (updateState widget) next)
  where
    updateState widget newState = widget { state = newState }

drawWidget :: Widget e s -> Image
drawWidget widget = draw widget (state widget)
