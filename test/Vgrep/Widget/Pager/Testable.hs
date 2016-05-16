{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vgrep.Widget.Pager.Testable
    ( module Vgrep.Widget.Pager
    , module Vgrep.Widget.Pager.Internal
    ) where

import Data.Text.Lazy.Testable ()
import Test.QuickCheck

import Vgrep.Widget.Pager
import Vgrep.Widget.Pager.Internal

instance Arbitrary Pager where
    arbitrary = do
        linesOfText <- arbitrary
        pos <- case linesOfText of
            [] -> pure 0
            _  -> choose (1, length linesOfText)
        pure Pager
            { _position    = pos
            , _column      = 0
            , _highlighted = mempty
            , _above       = take pos linesOfText
            , _visible     = drop pos linesOfText }
