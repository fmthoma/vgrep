{-# LANGUAGE DeriveGeneric #-}
module Vgrep.Environment.Config.Monoid
  ( ConfigMonoid (..)
  , ColorsMonoid (..)
  , KeybindingsMonoid (..)
  ) where

import Data.Monoid
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import GHC.Generics
import Graphics.Vty.Attributes  (Attr)

import Vgrep.KeybindingMap

-- $setup
-- >>> import Data.Map.Strict
-- >>> import Vgrep.Command
-- >>> import qualified Vgrep.Key as Key

-- | A 'Monoid' for reading partial configs. The 'ConfigMonoid' can be converted
-- to an actual 'Vgrep.Environment.Config.Config' using
-- 'Vgrep.Environment.Config.fromConfigMonoid'.
--
-- The Monoid consists mostly of 'First a' values, so the most important config
-- (the one that overrides all the others) should be read first.
data ConfigMonoid = ConfigMonoid
    { _mcolors      :: ColorsMonoid
    , _mtabstop     :: First Int
    , _meditor      :: First String
    , _mkeybindings :: KeybindingsMonoid
    } deriving (Eq, Show, Generic)

instance Monoid ConfigMonoid where
    mempty  = memptydefault
    mappend = mappenddefault


-- | A 'Monoid' for reading partial 'Vgrep.Environment.Config.Colors'
-- configurations.
--
-- Note that the attributes are not merged, but overridden:
--
-- >>> import Graphics.Vty.Attributes
-- >>> let leftStyle  = defAttr `withStyle` standout
-- >>> let rightStyle = defAttr `withForeColor` black
-- >>> let l = mempty { _mnormal = First (Just leftStyle)}
-- >>> let r = mempty { _mnormal = First (Just rightStyle)}
-- >>> _mnormal (l <> r) == First (Just (leftStyle <> rightStyle))
-- False
-- >>> _mnormal (l <> r) == First (Just leftStyle)
-- True
data ColorsMonoid = ColorsMonoid
    { _mlineNumbers   :: First Attr
    , _mlineNumbersHl :: First Attr
    , _mnormal        :: First Attr
    , _mnormalHl      :: First Attr
    , _mfileHeaders   :: First Attr
    , _mselected      :: First Attr
    } deriving (Eq, Show, Generic)

instance Monoid ColorsMonoid where
    mempty = memptydefault
    mappend = mappenddefault


-- | A 'Monoid' for reading a partial 'Vgrep.Environment.Config.Keybindings'
-- configuration.
--
-- Mappings are combined using left-biased 'Data.Map.Strict.union':
--
-- >>> let l = Just (KeybindingMap (fromList [(Key.Chord mempty Key.Down, ResultsDown), (Key.Chord mempty Key.Up, ResultsUp)]))
-- >>> let r = Just (KeybindingMap (fromList [(Key.Chord mempty Key.Down, PagerDown)]))
-- >>> l <> r
-- Just (KeybindingMap {unKeybindingMap = fromList [(Chord (fromList []) Up,ResultsUp),(Chord (fromList []) Down,ResultsDown)]})
-- >>> r <> l
-- Just (KeybindingMap {unKeybindingMap = fromList [(Chord (fromList []) Up,ResultsUp),(Chord (fromList []) Down,PagerDown)]})
--
-- In particular, @'Just' ('Data.Map.Strict.fromList' [])@ (declaring an empty
-- list of mappings) and @'Nothing'@ (not declaring anything) are equivalent,
-- given that there are already default mappings:
--
-- >>> l <> Just (KeybindingMap (fromList [])) == l <> Nothing
-- True
--
-- This means that new keybindings override the previous ones if they collide,
-- otherwise they are simply added. To remove a keybinding, it has to be mapped
-- to 'Unset' explicitly.
data KeybindingsMonoid = KeybindingsMonoid
    { _mresultsKeybindings :: Maybe KeybindingMap
    , _mpagerKeybindings   :: Maybe KeybindingMap
    , _mglobalKeybindings  :: Maybe KeybindingMap
    } deriving (Eq, Show, Generic)

instance Monoid KeybindingsMonoid where
    mempty = memptydefault
    mappend = mappenddefault
