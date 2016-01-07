module Vgrep.Widget.Type where

import Graphics.Vty

import Vgrep.Event

data Widget s = Widget { state       :: s
                       , dimensions  :: DisplayRegion
                       , resize      :: DisplayRegion -> s -> s
                       , draw        :: s -> Image
                       , handleEvent :: EventHandler s }

passEventsToWidget :: EventHandler (Widget s)
passEventsToWidget = EventHandler $ \event widget -> do
    next <- handle (handleEvent widget) event (state widget)
    return (fmap (updateState widget) next)
  where
    updateState widget newState = widget { state = newState }

drawWidget :: Widget s -> Image
drawWidget widget = draw widget (state widget)
