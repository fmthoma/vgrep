module Vgrep.Widget.Type where

import Graphics.Vty hiding (resize)

import Vgrep.Event

data Widget s = Widget { state       :: s
                       , dimensions  :: DisplayRegion
                       , resize      :: DisplayRegion -> s -> s
                       , draw        :: s -> Image
                       , handleEvent :: EventHandler s }

passEventsToWidget :: EventHandler (Widget s)
passEventsToWidget = EventHandler $ \event widget ->
    fmap (updateState widget)
         (handle (handleEvent widget) event (state widget))
  where
    updateState widget newState = widget { state = newState }

drawWidget :: Widget s -> Image
drawWidget widget = draw widget (state widget)

resizeWidget :: Widget s -> DisplayRegion -> Widget s
resizeWidget widget newRegion =
    widget { state = (resize widget) newRegion (state widget) }
