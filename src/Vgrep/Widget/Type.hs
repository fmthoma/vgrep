{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Type where

import Control.Lens
import Control.Monad.State.Extended
import Graphics.Vty hiding (resize)

import Vgrep.Event
import Vgrep.Type

data Widget s = Widget { _widgetState :: s
                       , _dimensions  :: DisplayRegion
                       , _resize      :: DisplayRegion -> State s ()
                       , _draw        :: s -> Vgrep Image }

makeLenses ''Widget

drawWidget :: Widget s -> Vgrep Image
drawWidget widget = (view draw widget) (view widgetState widget)

resizeWidget :: DisplayRegion -> State (Widget s) ()
resizeWidget newRegion = do
    resizeTo <- use resize
    zoom widgetState (resizeTo newRegion)

handleResizeEvent :: EventHandler (Widget s)
handleResizeEvent = mkEventHandler $ \event widget -> case event of
    VtyEvent (EvResize w h) -> Continue (execState (resizeWidget (w, h)) widget)
    _otherwise              -> Unchanged
