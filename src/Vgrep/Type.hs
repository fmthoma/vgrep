module Vgrep.Type where

import Control.Monad.Reader

import Vgrep.Environment

type Vgrep a = ReaderT Environment IO a
