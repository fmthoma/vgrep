{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Vgrep.Widget.HorizontalSplit
    ( HSplitState ()
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
import Control.Monad.State (State, execState)
import Graphics.Vty (Image, DisplayRegion, (<|>))

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

currentWidget :: Lens' (HSplitWidget s t) (Either s t)
currentWidget = widgetState . lens getCurrentWidget setCurrentWidget
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

leftWidgetFocused :: Traversal' (HSplitWidget s t) s
leftWidgetFocused = currentWidget . _Left

rightWidgetFocused :: Traversal' (HSplitWidget s t) t
rightWidgetFocused = currentWidget . _Right


type HSplitWidget s t = Widget (HSplitState s t)

hSplitWidget :: Widget s
             -> Widget t
             -> DisplayRegion
             -> HSplitWidget (Widget s) (Widget t)
hSplitWidget left right initialRegion =
    Widget { _widgetState = initState left right initialRegion
           , _dimensions  = initialRegion
           , _resize      = resizeWidgets
           , _draw        = drawWidgets }

initState :: Widget s
          -> Widget t
          -> DisplayRegion
          -> HSplitState (Widget s) (Widget t)
initState left right initialRegion =
    execState (resizeWidgets initialRegion) $
        State { _widgets = (left, right)
              , _split   = LeftOnly
              , _region  = initialRegion }

leftOnly :: State (HSplitState (Widget s) (Widget t)) ()
leftOnly = do assign split LeftOnly
              use region >>= resizeWidgets

rightOnly :: State (HSplitState (Widget s) (Widget t)) ()
rightOnly = do assign split RightOnly
               use region >>= resizeWidgets

splitFocusLeft :: Rational -> State (HSplitState (Widget s) (Widget t)) ()
splitFocusLeft ratio = do assign split (Split (FocusLeft, ratio))
                          use region >>= resizeWidgets

splitFocusRight :: Rational -> State (HSplitState (Widget s) (Widget t)) ()
splitFocusRight ratio = do assign split (Split (FocusRight, ratio))
                           use region >>= resizeWidgets

switchFocus :: State (HSplitState (Widget s) (Widget t)) ()
switchFocus = use split >>= \case
    Split focus  -> do assign split (Split (switch focus))
                       use region >>= resizeWidgets
    _otherwise   -> pure ()
  where
    switch (FocusLeft,  ratio) = (FocusRight, 1 - ratio)
    switch (FocusRight, ratio) = (FocusLeft,  1 - ratio)

resizeWidgets :: DisplayRegion
              -> State (HSplitState (Widget s) (Widget t)) ()
resizeWidgets newRegion@(w, h) = do
    assign region newRegion
    use split >>= \case
        LeftOnly  -> zoom (widgets . _1) (resizeWidget newRegion)
        RightOnly -> zoom (widgets . _2) (resizeWidget newRegion)
        Split (_, ratio) -> do
            let leftRegion  = (ceiling (ratio * fromIntegral w), h)
                rightRegion = (floor ((1 - ratio) * fromIntegral w), h)
            zoom (widgets . _1) (resizeWidget leftRegion)
            zoom (widgets . _2) (resizeWidget rightRegion)

drawWidgets :: HSplitState (Widget s) (Widget t) -> Image
drawWidgets state = case view split state of
    LeftOnly  -> drawWidget (view leftWidget  state)
    RightOnly -> drawWidget (view rightWidget state)
    Split _   -> drawWidget (view leftWidget  state)
             <|> drawWidget (view rightWidget state)
