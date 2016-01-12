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
    , focusLeft
    , focusRight
    , switchFocus
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.State (State, execState)
import Graphics.Vty (Image, DisplayRegion, (<|>))

import Vgrep.Widget.Type


data HSplitState s t = State { _widgets :: (s, t)
                             , _focused :: Focus
                             , _ratio   :: Rational
                             , _region  :: DisplayRegion }

data Focus = FocusLeft | FocusRight deriving (Eq)

makeLenses ''HSplitState

leftWidget :: Lens' (HSplitState s t) s
leftWidget = widgets . _1

rightWidget :: Lens' (HSplitState s t) t
rightWidget = widgets . _2

currentWidget :: Lens' (HSplitWidget s t) (Either s t)
currentWidget = widgetState . lens getCurrentWidget setCurrentWidget
  where
    getCurrentWidget state = case view focused state of
        FocusLeft  -> Left  (view leftWidget  state)
        FocusRight -> Right (view rightWidget state)

    setCurrentWidget state newWidget = case (view focused state, newWidget) of
        (FocusLeft,  Left  widgetL) -> set leftWidget  widgetL state
        (FocusRight, Right widgetR) -> set rightWidget widgetR state
        (_         , _      )       -> state

leftWidgetFocused :: Traversal' (HSplitWidget s t) s
leftWidgetFocused = currentWidget . _Left

rightWidgetFocused :: Traversal' (HSplitWidget s t) t
rightWidgetFocused = currentWidget . _Right


type HSplitWidget s t = Widget (HSplitState s t)

hSplitWidget :: Widget s
             -> Widget t
             -> Rational
             -> DisplayRegion
             -> HSplitWidget (Widget s) (Widget t)
hSplitWidget left right initialRatio initialRegion =
    Widget { _widgetState = initState left right initialRatio initialRegion
           , _dimensions  = initialRegion
           , _resize      = resizeWidgets
           , _draw        = drawWidgets }

initState :: Widget s
          -> Widget t
          -> Rational
          -> DisplayRegion
          -> HSplitState (Widget s) (Widget t)
initState left right initialRatio initialRegion =
    execState (resizeWidgets initialRegion) $
        State { _widgets = (left, right)
              , _focused = FocusLeft
              , _ratio   = initialRatio
              , _region  = initialRegion }


focusLeft :: State (HSplitState (Widget s) (Widget t)) ()
focusLeft = do f <- use focused
               when (f == FocusRight) switchFocus

focusRight :: State (HSplitState (Widget s) (Widget t)) ()
focusRight = do f <- use focused
                when (f == FocusLeft) switchFocus

switchFocus :: State (HSplitState (Widget s) (Widget t)) ()
switchFocus = do
    modifying focused switch
    modifying ratio (\r -> 1 - r)
    use region >>= resizeWidgets
  where
    switch FocusLeft  = FocusRight
    switch FocusRight = FocusLeft

resizeWidgets :: DisplayRegion
              -> State (HSplitState (Widget s) (Widget t)) ()
resizeWidgets newRegion@(w, h) = do
    splitRatio  <- use ratio
    let leftRegion  = (ceiling (splitRatio * fromIntegral w), h)
        rightRegion = (floor ((1 - splitRatio) * fromIntegral w), h)
    zoom (widgets . _1) (resizeWidget leftRegion)
    zoom (widgets . _2) (resizeWidget rightRegion)
    assign region newRegion

drawWidgets :: HSplitState (Widget s) (Widget t) -> Image
drawWidgets state = drawWidget (view leftWidget  state)
                <|> drawWidget (view rightWidget state)
