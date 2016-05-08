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
    ) where

import Control.Lens


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
-- >>> view currentWidget $ State { _leftWidget = foo, _layout = LeftOnly }
-- Left foo
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
-- >>> has leftWidgetFocused $ State { _layout = LeftOnly }
-- True
--
-- >>> has leftWidgetFocused $ State { _layout = RightOnly }
-- False
--
-- >>> has leftWidgetFocused $ State { _layout = Split FocusLeft 1%2 }
-- False
leftWidgetFocused :: Traversal' (HSplit s t) s
leftWidgetFocused = currentWidget . _Left

-- | Traverses the right widget if focused
-- 
-- >>> has rightWidgetFocused $ State { _layout = RightOnly }
-- True
--
-- >>> has rightWidgetFocused $ State { _layout = LeftOnly }
-- False
--
-- >>> has rightWidgetFocused $ State { _layout = Split FocusRight 1%2 }
-- True
rightWidgetFocused :: Traversal' (HSplit s t) t
rightWidgetFocused = currentWidget . _Right
