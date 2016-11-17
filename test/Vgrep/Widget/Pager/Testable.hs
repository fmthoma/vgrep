{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vgrep.Widget.Pager.Testable
    ( module Vgrep.Widget.Pager
    , module Vgrep.Widget.Pager.Internal
    ) where

import Data.Sequence           as Seq (fromList)
import Data.Text.Testable ()
import Test.QuickCheck

import Vgrep.Widget.Pager
import Vgrep.Widget.Pager.Internal

instance Arbitrary Pager where
    arbitrary = do
        linesOfText <- arbitrary
        pos <- case linesOfText of
            [] -> pure 0
            _  -> choose (0, length linesOfText - 1)
        pure Pager
            { _column      = 0
            , _highlighted = mempty
            , _above       = Seq.fromList (take pos linesOfText)
            , _visible     = Seq.fromList (drop pos linesOfText) }
