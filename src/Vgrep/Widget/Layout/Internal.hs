{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Layout.Internal (
    -- * Layout widget state
      Layout (..)
    , Ratio (..)
    , Orientation (..)
    , Focus (..)

    -- ** Auto-generated lenses
    , splitRatio
    , orientation
    , primary
    , secondary
    , focus

    -- ** Additional lenses
    , focusedWidget
    ) where

import Control.Lens.Compat

-- $setup
-- >>> :set -fno-warn-missing-fields

-- | The internal state of the 'Layout' widget. Tracks the state of both the
-- child widgets and the current layout.
data Layout s t = Layout
    { _splitRatio  :: Ratio
    , _orientation :: Orientation
    , _primary     :: s
    , _secondary   :: t
    , _focus       :: Focus }

data Ratio
    = Dynamic Rational
    | FixedPrimary Int
    | FixedSecondary Int
    deriving (Eq)

data Orientation = Horizontal | Vertical deriving (Eq)

data Focus = PrimaryOnly | FocusPrimary | FocusSecondary | SecondaryOnly
    deriving (Eq)

makeLenses ''Layout


-- | The currently focused child widget
--
-- >>> view focusedWidget $ Layout { _primary = "foo", _focus = PrimaryOnly }
-- Left "foo"
--
-- >>> view focusedWidget $ Layout { _primary = "foo", _focus = FocusPrimary }
-- Left "foo"
--
-- >>> view focusedWidget $ Layout { _secondary = "bar", _focus = FocusSecondary }
-- Right "bar"
focusedWidget :: Getter (Layout s t) (Either s t)
focusedWidget = to getFocusedWidget
  where
    getFocusedWidget state = case view focus state of
        PrimaryOnly    -> Left  (view primary state)
        FocusPrimary   -> Left  (view primary state)
        FocusSecondary -> Right (view secondary state)
        SecondaryOnly  -> Right (view secondary state)
