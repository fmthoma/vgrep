{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment where

import Control.Lens
import Data.Text.Lazy (Text)

data Environment = Env { _input :: Text }

makeLenses ''Environment
