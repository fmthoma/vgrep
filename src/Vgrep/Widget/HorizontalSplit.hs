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
    ) where

import Control.Lens
import Control.Monad.State.Extended (StateT)
import Control.Monad.Reader (local)
import Graphics.Vty.Image hiding (resize)
import Graphics.Vty.Input

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type
import Vgrep.Widget.Type


data HSplitState s t = State { _leftWidget  :: s
                             , _rightWidget :: t
                             , _split       :: Split }

data Focus = FocusLeft | FocusRight deriving (Eq)
data Split = LeftOnly | RightOnly | Split Focus Rational deriving (Eq)

makeLenses ''HSplitState


currentWidget :: Lens' (HSplitState s t) (Either s t)
currentWidget = lens getCurrentWidget setCurrentWidget
  where
    getCurrentWidget state = case view split state of
        LeftOnly           -> Left  (view leftWidget  state)
        Split FocusLeft _  -> Left  (view leftWidget  state)
        RightOnly          -> Right (view rightWidget state)
        Split FocusRight _ -> Right (view rightWidget state)

    setCurrentWidget state newWidget = case (view split state, newWidget) of
        (RightOnly,          Left  widgetL) -> set leftWidget  widgetL state
        (Split FocusLeft _,  Left  widgetL) -> set leftWidget  widgetL state
        (LeftOnly,           Right widgetR) -> set rightWidget widgetR state
        (Split FocusRight _, Right widgetR) -> set rightWidget widgetR state
        (_,                  _            ) -> state

leftWidgetFocused :: Traversal' (HSplitState s t) s
leftWidgetFocused = currentWidget . _Left

rightWidgetFocused :: Traversal' (HSplitState s t) t
rightWidgetFocused = currentWidget . _Right


type HSplitWidget s t = Widget (HSplitState s t)

hSplitWidget :: Widget s
             -> Widget t
             -> HSplitWidget s t
hSplitWidget left right =
    Widget { initialize = initHSplit   left right
           , draw       = drawWidgets  left right
           , handle     = handleEvents left right }

initHSplit :: Widget s -> Widget t -> HSplitState s t
initHSplit left right =
    State  { _leftWidget  = initialize left
           , _rightWidget = initialize right
           , _split       = LeftOnly }


leftOnly :: Monad m => StateT (HSplitState s t) m Redraw
leftOnly = use split >>= \case
    LeftOnly -> pure Unchanged
    _other   -> assign split LeftOnly >> pure Redraw

rightOnly :: Monad m => StateT (HSplitState s t) m Redraw
rightOnly = use split >>= \case
    RightOnly -> pure Unchanged
    _other    -> assign split RightOnly >> pure Redraw

splitView :: Monad m => Focus -> Rational -> StateT (HSplitState s t) m Redraw
splitView focus ratio = assign split (Split focus ratio) >> pure Redraw

switchFocus :: Monad m => StateT (HSplitState s t) m Redraw
switchFocus = use split >>= \case
    Split focus ratio -> assign split (switch focus ratio) >> pure Redraw
    _otherwise        -> pure Unchanged
  where
    switch FocusLeft  ratio = Split FocusRight (1 - ratio)
    switch FocusRight ratio = Split FocusLeft  (1 - ratio)

drawWidgets :: Monad m
            => Widget s
            -> Widget t
            -> StateT (HSplitState s t) (VgrepT m) Image
drawWidgets left right = use split >>= \case
    LeftOnly      -> zoom leftWidget  (draw left)
    RightOnly     -> zoom rightWidget (draw right)
    Split _ ratio -> liftA2 (<|>)
        (runInLeftWidget  ratio (draw left))
        (runInRightWidget ratio (draw right))

runInLeftWidget
    :: Monad m
    => Rational
    -> StateT s (VgrepT m) Image
    -> StateT (HSplitState s t) (VgrepT m) Image
runInLeftWidget ratio action =
    let leftRegion = over (region . _1) $ \w ->
            ceiling (ratio * fromIntegral w)
    in  zoom leftWidget (local leftRegion action)


runInRightWidget
    :: Monad m
    => Rational
    -> StateT t (VgrepT m) Image
    -> StateT (HSplitState s t) (VgrepT m) Image
runInRightWidget ratio action =
    let rightRegion = over (region . _1) $ \w ->
            floor ((1-ratio) * fromIntegral w)
    in  zoom rightWidget (local rightRegion action)

-- ------------------------------------------------------------------------
-- Events & Keybindings
-- ------------------------------------------------------------------------

-- FIXME: local region!
handleEvents :: Monad m
             => Widget s
             -> Widget t
             -> Event
             -> StateT (HSplitState s t) (VgrepT m) Redraw
handleEvents left right e =
    use currentWidget >>= \case
        Left _ -> case hSplitKeyBindings_left e of
            Just leftHSplitEvent -> leftHSplitEvent
            Nothing              -> zoom leftWidget (handle left e)
        Right _ -> case hSplitKeyBindings_right e of
            Just rightHSplitEvent -> rightHSplitEvent
            Nothing               -> zoom rightWidget (handle right e)

hSplitKeyBindings_left :: Monad m
                       => Event
                       -> Maybe (StateT (HSplitState s t) (VgrepT m) Redraw)
hSplitKeyBindings_left = dispatch
    [ EvKey (KChar '\t') [] ==> switchFocus
    , EvKey (KChar 'f')  [] ==> leftOnly ]

hSplitKeyBindings_right :: Monad m
                        => Event
                        -> Maybe (StateT (HSplitState s t) (VgrepT m) Redraw)
hSplitKeyBindings_right = dispatch
    [ EvKey (KChar '\t') [] ==> switchFocus
    , EvKey (KChar 'q')  [] ==> leftOnly
    , EvKey (KChar 'f')  [] ==> rightOnly ]

