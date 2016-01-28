{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment
    ( Environment (..)
    , input
    , config

    , module Vgrep.Environment.Config
    ) where

import Control.Lens
import Data.Text.Lazy (Text)

import Vgrep.Environment.Config

data Environment = Env { _input  :: Text
                       , _config :: Config }

makeLenses ''Environment
