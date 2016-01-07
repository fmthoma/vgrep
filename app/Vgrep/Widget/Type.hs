{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Type where

import Control.Lens
import Control.Lens.TH
import Graphics.Vty hiding (resize)

import Vgrep.Event

data Widget s = Widget { _state       :: s
                       , _dimensions  :: DisplayRegion
                       , _resize      :: DisplayRegion -> s -> s
                       , _draw        :: s -> Image
                       , _handleEvent :: EventHandler s }

makeLenses ''Widget

passEventsToWidget :: EventHandler (Widget s)
passEventsToWidget = EventHandler $ \event widget ->
    let eventHandler = view handleEvent widget
        currentState = view state widget
        next = handle eventHandler event currentState
    in  fmap (\newState -> set state newState widget) next
  where

drawWidget :: Widget s -> Image
drawWidget widget = (view draw widget) (view state widget)

resizeWidget :: Widget s -> DisplayRegion -> Widget s
resizeWidget widget newRegion =
    let resizeTo = view resize widget
    in  over state (resizeTo newRegion) widget
