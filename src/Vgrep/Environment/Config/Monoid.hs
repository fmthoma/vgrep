{-# LANGUAGE DeriveGeneric #-}
module Vgrep.Environment.Config.Monoid
  ( ConfigMonoid (..)
  , ColorsMonoid (..)
  , KeybindingsMonoid (..)
  ) where

import Data.Map.Strict          (Map)
import Data.Monoid
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import GHC.Generics
import Graphics.Vty.Attributes  (Attr)

import           Vgrep.Command
import qualified Vgrep.Key     as Key


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


data KeybindingsMonoid = KeybindingsMonoid
    { _mresultsKeybindings :: Map Key.Chord Command
    , _mpagerKeybindings   :: Map Key.Chord Command
    , _mglobalKeybindings  :: Map Key.Chord Command
    } deriving (Eq, Show, Generic)

instance Monoid KeybindingsMonoid where
    mempty = memptydefault
    mappend = mappenddefault
