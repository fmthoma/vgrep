{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vgrep.KeybindingMap where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Vgrep.Command
import qualified Vgrep.Key       as Key


newtype KeybindingMap = KeybindingMap { unKeybindingMap :: Map Key.Chord Command }
  deriving (Show, Eq, Monoid)

lookup :: Key.Chord -> KeybindingMap -> Maybe Command
lookup chord (KeybindingMap m) = M.lookup chord m

fromList :: [(Key.Chord, Command)] -> KeybindingMap
fromList = KeybindingMap . M.fromList
