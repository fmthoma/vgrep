-- | A split-view widget that displays two widgets side-by-side.
module Vgrep.Widget.HorizontalSplit (
    -- * Horizontal split view widget
      hSplitWidget
    , HSplitWidget

    -- ** Widget state
    , HSplit ()
    , Focus (..)

    -- ** Widget actions
    , leftOnly
    , rightOnly
    , splitView
    , switchFocus

    -- ** Lenses
    , leftWidget
    , rightWidget
    , currentWidget
    , leftWidgetFocused
    , rightWidgetFocused
    ) where

import Control.Applicative (liftA2)
import Control.Lens.Compat
import Graphics.Vty.Image

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type
import Vgrep.Widget.HorizontalSplit.Internal
import Vgrep.Widget.Type


type HSplitWidget s t = Widget (HSplit s t)

-- | Compose two 'Widget's side-by-side
--
-- * __Initial state__
--
--     Initially, the left widget is rendered full-screen.
--
-- * __Drawing the Widgets__
--
--     Drawing is delegated to the child widgets in a local environment
--     reduced to thir respective 'Viewport'.
hSplitWidget
    :: Widget s
    -> Widget t
    -> HSplitWidget s t
hSplitWidget left right = Widget
    { initialize = initHSplit   left right
    , draw       = drawWidgets  left right }

initHSplit :: Widget s -> Widget t -> HSplit s t
initHSplit left right = HSplit
    { _leftWidget  = initialize left
    , _rightWidget = initialize right
    , _layout      = LeftOnly }


-- | Display the left widget full-screen
leftOnly :: Monad m => VgrepT (HSplit s t) m Redraw
leftOnly = use layout >>= \case
    LeftOnly -> pure Unchanged
    _other   -> assign layout LeftOnly >> pure Redraw

-- | Display the right widget full-screen
rightOnly :: Monad m => VgrepT (HSplit s t) m Redraw
rightOnly = use layout >>= \case
    RightOnly -> pure Unchanged
    _other    -> assign layout RightOnly >> pure Redraw

-- | Display both widgets in a split view.
splitView
    :: Monad m
    => Focus -- ^ Focus left or right area
    -> Rational -- ^ Left area width as fraction of overall width
    -> VgrepT (HSplit s t) m Redraw
splitView focus ratio = assign layout (Split focus ratio) >> pure Redraw

-- | Switch focus from left to right child widget and vice versa (only if
-- the '_layout' is 'Split')
switchFocus :: Monad m => VgrepT (HSplit s t) m Redraw
switchFocus = use layout >>= \case
    Split focus ratio -> assign layout (switch focus ratio) >> pure Redraw
    _otherwise        -> pure Unchanged
  where
    switch FocusLeft  ratio = Split FocusRight (1 - ratio)
    switch FocusRight ratio = Split FocusLeft  (1 - ratio)

drawWidgets
    :: Monad m
    => Widget s
    -> Widget t
    -> VgrepT (HSplit s t) m Image
drawWidgets left right = use layout >>= \case
    LeftOnly      -> zoom leftWidget  (draw left)
    RightOnly     -> zoom rightWidget (draw right)
    Split _ ratio -> liftA2 (<|>)
        (runInLeftWidget  ratio (draw left))
        (runInRightWidget ratio (draw right))

runInLeftWidget
    :: Monad m
    => Rational
    -> VgrepT s m Image
    -> VgrepT (HSplit s t) m Image
runInLeftWidget ratio action =
    let leftRegion = over viewportWidth $ \w ->
            ceiling (ratio * fromIntegral w)
    in  zoom leftWidget (local leftRegion action)


runInRightWidget
    :: Monad m
    => Rational
    -> VgrepT t m Image
    -> VgrepT (HSplit s t) m Image
runInRightWidget ratio action =
    let rightRegion = over viewportWidth $ \w ->
            floor ((1-ratio) * fromIntegral w)
    in  zoom rightWidget (local rightRegion action)
