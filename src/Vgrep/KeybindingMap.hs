{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vgrep.KeybindingMap where

import           Data.Map.Strict (Map)
import           Vgrep.Command
import qualified Vgrep.Key       as Key


newtype KeybindingMap = KeybindingMap { unKeybindingMap :: Map Key.Chord Command }
  deriving (Show, Eq, Monoid)
