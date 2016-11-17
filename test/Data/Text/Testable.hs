{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Text.Testable ( module Data.Text ) where

import Data.Text
import Test.Tasty.QuickCheck

instance Arbitrary Text where
    arbitrary = fmap pack arbitrary
    shrink = fmap pack . shrink . unpack
