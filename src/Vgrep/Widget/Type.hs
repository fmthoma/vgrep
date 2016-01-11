{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Type where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Graphics.Vty hiding (resize)

import Vgrep.Event

data Widget s = Widget { _widgetState :: s
                       , _dimensions  :: DisplayRegion
                       , _resize      :: DisplayRegion -> State s ()
                       , _draw        :: s -> Image }

makeLenses ''Widget

drawWidget :: Widget s -> Image
drawWidget widget = (view draw widget) (view widgetState widget)

resizeWidget :: DisplayRegion -> State (Widget s) ()
resizeWidget newRegion = do
    resizeTo <- use resize
    zoom widgetState (resizeTo newRegion)
