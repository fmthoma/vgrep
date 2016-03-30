{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment
    ( Environment (..)
    , config

    , module Vgrep.Environment.Config
    ) where

import Control.Lens

import Vgrep.Environment.Config

data Environment = Env { _config :: Config }

makeLenses ''Environment
