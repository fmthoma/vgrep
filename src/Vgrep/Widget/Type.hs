{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Type where

import Control.Lens
import Control.Monad.State.Extended
import Graphics.Vty hiding (resize)

import Vgrep.Type

data Widget s = Widget { _resize :: DisplayRegion -> State s ()
                       , _draw   :: s -> Vgrep Image }

makeLenses ''Widget
