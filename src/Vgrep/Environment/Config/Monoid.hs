{-# LANGUAGE DeriveGeneric #-}
module Vgrep.Environment.Config.Monoid
  ( ConfigMonoid (..)
  , ColorsMonoid (..)
  , Attr
  ) where

import Data.Monoid
import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import GHC.Generics
import Graphics.Vty.Attributes  (Attr)


data ConfigMonoid = ConfigMonoid
    { _mcolors  :: ColorsMonoid
    , _mtabstop :: First Int
    , _meditor  :: First String
    } deriving (Eq, Show, Generic)

instance Monoid ConfigMonoid where
    mempty  = memptydefault
    mappend = mappenddefault


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
