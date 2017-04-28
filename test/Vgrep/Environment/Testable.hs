{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vgrep.Environment.Testable
    ( module Vgrep.Environment
    ) where

import Test.QuickCheck

import Vgrep.Environment


instance Arbitrary Environment where
    arbitrary = do
        width  <- arbitrary `suchThat` (> 0) -- FIXME tweak numbers
        height <- arbitrary `suchThat` (> 0) -- FIXME tweak numbers
        pure Env
            { _viewport = Viewport width height
            , _config = defaultConfig
            , _searchRegex = Nothing }
