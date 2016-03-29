module Vgrep.Widget.Type where

import Control.Monad.State.Extended
import Graphics.Vty.Image (Image)

import Vgrep.Type

data Widget e s = Widget
    { initialize :: s
    , draw       :: s -> Vgrep Image
    , handle     :: e -> StateT s Vgrep () }
