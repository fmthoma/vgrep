module Vgrep.Widget.Type where

import Control.Monad.State.Extended
import Graphics.Vty.Image (Image)
import Graphics.Vty.Prelude

import Vgrep.Type

data Widget s = Widget { initialize :: DisplayRegion -> s
                       , resize     :: DisplayRegion -> State s ()
                       , draw       :: s -> Vgrep Image }
