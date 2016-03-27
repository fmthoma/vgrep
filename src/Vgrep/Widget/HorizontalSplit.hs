{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Vgrep.Widget.HorizontalSplit
    ( HSplitState ()
    , initHSplit
    , HSplitWidget
    , hSplitWidget

    , leftWidget
    , rightWidget
    , currentWidget
    , leftWidgetFocused
    , rightWidgetFocused
    , leftOnly
    , rightOnly
    , splitFocusLeft
    , splitFocusRight
    , switchFocus
    ) where

import Control.Lens
import Control.Monad.State.Extended (State)
import Graphics.Vty.Image hiding (resize)
import Graphics.Vty.Prelude

import Vgrep.Type
import Vgrep.Widget.Type


data HSplitState s t = State { _widgets :: (s, t)
                             , _split   :: Split
                             , _region  :: DisplayRegion }

data Focus = FocusLeft | FocusRight deriving (Eq)
data Split = LeftOnly | RightOnly | Split (Focus, Rational) deriving (Eq)

makeLenses ''HSplitState

leftWidget :: Lens' (HSplitState s t) s
leftWidget = widgets . _1

rightWidget :: Lens' (HSplitState s t) t
rightWidget = widgets . _2

currentWidget :: Lens' (HSplitState s t) (Either s t)
currentWidget = lens getCurrentWidget setCurrentWidget
  where
    getCurrentWidget state = case view split state of
        LeftOnly              -> Left  (view leftWidget  state)
        Split (FocusLeft, _)  -> Left  (view leftWidget  state)
        RightOnly             -> Right (view rightWidget state)
        Split (FocusRight, _) -> Right (view rightWidget state)

    setCurrentWidget state newWidget = case (view split state, newWidget) of
        (RightOnly,             Left  widgetL) -> set leftWidget  widgetL state
        (Split (FocusLeft, _),  Left  widgetL) -> set leftWidget  widgetL state
        (LeftOnly,              Right widgetR) -> set rightWidget widgetR state
        (Split (FocusRight, _), Right widgetR) -> set rightWidget widgetR state
        (_,                     _            ) -> state

leftWidgetFocused :: Traversal' (HSplitState s t) s
leftWidgetFocused = currentWidget . _Left

rightWidgetFocused :: Traversal' (HSplitState s t) t
rightWidgetFocused = currentWidget . _Right


type HSplitWidget s t = Widget (HSplitState s t)

hSplitWidget :: Widget s -> Widget t -> HSplitWidget s t
hSplitWidget left right =
    Widget { initialize = initHSplit    left right
           , resize     = resizeWidgets left right
           , draw       = drawWidgets   left right }

initHSplit :: Widget s -> Widget t -> DisplayRegion -> HSplitState s t
initHSplit left right initialRegion =
    State  { _widgets = ( initialize left  initialRegion
                        , initialize right initialRegion )
           , _split   = LeftOnly
           , _region  = initialRegion }


leftOnly :: HSplitWidget s t -> State (HSplitState s t) ()
leftOnly widget = do assign split LeftOnly
                     use region >>= resize widget

rightOnly :: HSplitWidget s t -> State (HSplitState s t) ()
rightOnly widget = do assign split RightOnly
                      use region >>= resize widget

splitFocusLeft :: Rational -> HSplitWidget s t -> State (HSplitState s t) ()
splitFocusLeft ratio widget = do assign split (Split (FocusLeft, ratio))
                                 use region >>= resize widget

splitFocusRight :: Rational -> HSplitWidget s t -> State (HSplitState s t) ()
splitFocusRight ratio widget = do assign split (Split (FocusRight, ratio))
                                  use region >>= resize widget

switchFocus :: HSplitWidget s t -> State (HSplitState s t) ()
switchFocus widget = use split >>= \case
    Split focus  -> do assign split (Split (switch focus))
                       use region >>= resize widget
    _otherwise   -> pure ()
  where
    switch (FocusLeft,  ratio) = (FocusRight, 1 - ratio)
    switch (FocusRight, ratio) = (FocusLeft,  1 - ratio)

resizeWidgets :: Widget s
              -> Widget t
              -> DisplayRegion
              -> State (HSplitState s t) ()
resizeWidgets left right newRegion@(w, h) = do
    assign region newRegion
    use split >>= \case
        LeftOnly  -> zoom (widgets . _1) (resize left  newRegion)
        RightOnly -> zoom (widgets . _2) (resize right newRegion)
        Split (_, ratio) -> do
            let leftRegion  = (ceiling (ratio * fromIntegral w), h)
                rightRegion = (floor ((1 - ratio) * fromIntegral w), h)
            zoom (widgets . _1) (resize left  leftRegion)
            zoom (widgets . _2) (resize right rightRegion)

drawWidgets :: Widget s -> Widget t -> HSplitState s t -> Vgrep Image
drawWidgets left right state = case view split state of
    LeftOnly  -> draw left  (view leftWidget  state)
    RightOnly -> draw right (view rightWidget state)
    Split _   -> liftA2 (<|>) (draw left  (view leftWidget  state))
                              (draw right (view rightWidget state))
