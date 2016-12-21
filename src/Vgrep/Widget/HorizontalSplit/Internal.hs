{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.HorizontalSplit.Internal (
    -- * Split-view widget state
      HSplit (..)
    , Layout (..)
    , Focus (..)

    -- ** Auto-generated lenses
    , leftWidget
    , rightWidget
    , layout

    -- ** Additional lenses
    , currentWidget
    , leftWidgetFocused
    , rightWidgetFocused

    -- ** Re-exports
    , (%)
    ) where

import Control.Lens.Compat
import Data.Ratio          ((%))


-- $setup
-- >>> :set -fno-warn-missing-fields

-- | The internal state of the split-view widget. Tracks the state of both
-- child widgets and the current layout.
data HSplit s t = HSplit
    { _leftWidget  :: s
    -- ^ State of the left widget

    , _rightWidget :: t
    -- ^ State of the right widget

    , _layout      :: Layout
    -- ^ Current layout
    }

data Focus = FocusLeft | FocusRight deriving (Eq)
data Layout = LeftOnly | RightOnly | Split Focus Rational deriving (Eq)

makeLenses ''HSplit


-- | The currently focused child widget
--
-- >>> view currentWidget $ HSplit { _leftWidget = "foo", _layout = LeftOnly }
-- Left "foo"
currentWidget :: Lens' (HSplit s t) (Either s t)
currentWidget = lens getCurrentWidget setCurrentWidget
  where
    getCurrentWidget state = case view layout state of
        LeftOnly           -> Left  (view leftWidget  state)
        Split FocusLeft _  -> Left  (view leftWidget  state)
        RightOnly          -> Right (view rightWidget state)
        Split FocusRight _ -> Right (view rightWidget state)

    setCurrentWidget state newWidget = case (view layout state, newWidget) of
        (RightOnly,          Left  widgetL) -> set leftWidget  widgetL state
        (Split FocusLeft _,  Left  widgetL) -> set leftWidget  widgetL state
        (LeftOnly,           Right widgetR) -> set rightWidget widgetR state
        (Split FocusRight _, Right widgetR) -> set rightWidget widgetR state
        (_,                  _            ) -> state

-- | Traverses the left widget if focused
--
-- >>> has leftWidgetFocused $ HSplit { _layout = LeftOnly }
-- True
--
-- >>> has leftWidgetFocused $ HSplit { _layout = RightOnly }
-- False
--
-- >>> has leftWidgetFocused $ HSplit { _layout = Split FocusLeft (1 % 2) }
-- True
leftWidgetFocused :: Traversal' (HSplit s t) s
leftWidgetFocused = currentWidget . _Left

-- | Traverses the right widget if focused
--
-- >>> has rightWidgetFocused $ HSplit { _layout = RightOnly }
-- True
--
-- >>> has rightWidgetFocused $ HSplit { _layout = LeftOnly }
-- False
--
-- >>> has rightWidgetFocused $ HSplit { _layout = Split FocusRight (1 % 2) }
-- True
rightWidgetFocused :: Traversal' (HSplit s t) t
rightWidgetFocused = currentWidget . _Right
