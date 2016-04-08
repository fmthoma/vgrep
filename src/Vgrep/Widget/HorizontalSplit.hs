{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Vgrep.Widget.HorizontalSplit
    ( HSplitState ()
    , HSplitEvent (..)
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
import Control.Monad.State.Extended (StateT, get)
import Control.Monad.Reader (local)
import Graphics.Vty.Image hiding (resize)

import Vgrep.Environment
import Vgrep.Type
import Vgrep.Widget.Type


data HSplitState s t = State { _widgets :: (s, t)
                             , _split   :: Split }

data Focus = FocusLeft | FocusRight deriving (Eq)
data Split = LeftOnly | RightOnly | Split Focus Rational deriving (Eq)

data HSplitEvent u v
    = LeftEvent u
    | RightEvent v
    | BothEvents u v
    | FocusedWidgetEvent u v
    | LeftWidget
    | RightWidget
    | SplitView Focus Rational
    | SwitchFocus

makeLenses ''HSplitState

leftWidget :: Lens' (HSplitState s t) s
leftWidget = widgets . _1

rightWidget :: Lens' (HSplitState s t) t
rightWidget = widgets . _2

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


type HSplitWidget u v s t = Widget (HSplitEvent u v) (HSplitState s t)

hSplitWidget :: Widget u s -> Widget v t -> HSplitWidget u v s t
hSplitWidget left right =
    Widget { initialize = initHSplit    left right
           , draw       = drawWidgets   left right
           , handle     = handleEvents  left right }

initHSplit :: Widget u s -> Widget v t -> HSplitState s t
initHSplit left right =
    State  { _widgets = (initialize left , initialize right)
           , _split   = LeftOnly }


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
            => Widget u s
            -> Widget v t
            -> HSplitState s t
            -> VgrepT m Image
drawWidgets left right state = case view split state of
    LeftOnly  -> draw left  (view leftWidget  state)
    RightOnly -> draw right (view rightWidget state)
    Split _ _ -> liftA2 (<|>)
        (local (leftRegion state)  (draw left  (view leftWidget  state)))
        (local (rightRegion state) (draw right (view rightWidget state)))

handleEvents :: Monad m
             => Widget u s
             -> Widget v t
             -> HSplitEvent u v
             -> StateT (HSplitState s t) (VgrepT m) Redraw
handleEvents left right = \case
    LeftEvent l    -> handleLeft l
    RightEvent r   -> handleRight r
    BothEvents l r -> liftA2 mappend (handleLeft l) (handleRight r)
    FocusedWidgetEvent l r -> use currentWidget >>= \case
        Left _  -> handleLeft l
        Right _ -> handleRight r
    LeftWidget     -> leftOnly
    RightWidget    -> rightOnly
    SplitView f r  -> splitView f r
    SwitchFocus    -> switchFocus
  where handleLeft event = get >>= \state ->
            local (leftRegion state) (zoom leftWidget (handle left event))
        handleRight event = get >>= \state ->
            local (rightRegion state) (zoom rightWidget (handle right event))

leftRegion, rightRegion :: HSplitState s t -> Environment -> Environment
leftRegion state = case view split state of
    LeftOnly      -> id
    RightOnly     -> id
    Split _ ratio -> over region $ \(w, h) ->
                            (ceiling (ratio * fromIntegral w), h)
rightRegion state = case view split state of
    LeftOnly      -> id
    RightOnly     -> id
    Split _ ratio -> over region $ \(w, h) ->
                            (floor ((1 - ratio) * fromIntegral w), h)
