{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment
    ( Environment (..)
    , config
    , region

    , module Vgrep.Environment.Config
    , module Graphics.Vty.Prelude
    ) where

import Control.Lens
import Graphics.Vty.Prelude

import Vgrep.Environment.Config

data Environment = Env { _config :: Config
                       , _region :: DisplayRegion }

makeLenses ''Environment
