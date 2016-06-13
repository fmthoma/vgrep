{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Text.Lazy.Testable ( module Data.Text.Lazy ) where

import Data.Text.Lazy
import Test.Tasty.QuickCheck

instance Arbitrary Text where
    arbitrary = fmap pack arbitrary
    shrink = fmap pack . shrink . unpack
