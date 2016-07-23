{-# LANGUAGE Rank2Types #-}
module Vgrep.Widget.Layout (
      hSplitWidget
    , LayoutWidget

    , Layout ()
    , Focus (..)
    , Ratio (..)


    , primaryOnly
    , secondaryOnly
    , splitView
    , switchFocus

    , primary
    , secondary
    , currentWidget
    , splitRatio
    , focus
    ) where

import Control.Lens
import Data.Monoid
import Data.Ratio         ((%))
import Graphics.Vty       ((<|>))
import Graphics.Vty.Input

import Vgrep.Event
import Vgrep.Type
import Vgrep.Widget.Layout.Internal
import Vgrep.Widget.Type


type LayoutWidget s t = Widget (Layout s t)


hSplitWidget :: Widget s -> Widget t -> LayoutWidget s t
hSplitWidget primaryWidget secondaryWidget =
    layoutWidget primaryWidget secondaryWidget Layout
        { _primary     = initialize primaryWidget
        , _secondary   = initialize secondaryWidget
        , _orientation = Horizontal
        , _splitRatio  = Dynamic (2 % 3)
        , _focus       = PrimaryOnly }

layoutWidget :: Widget s -> Widget t -> Layout s t -> LayoutWidget s t
layoutWidget primaryWidget secondaryWidget initialLayout = Widget
    { initialize = initialLayout
    , draw       = drawLayout   primaryWidget secondaryWidget
    , cursor     = getCursor    primaryWidget secondaryWidget
    , handle     = handleEvents primaryWidget secondaryWidget }


drawLayout :: Monad m => Widget s -> Widget t -> VgrepT (Layout s t) m Image
drawLayout primaryWidget secondaryWidget = use focus >>= \case
    PrimaryOnly    -> zoom primary   (draw primaryWidget)
    SecondaryOnly  -> zoom secondary (draw secondaryWidget)
    _splitView     -> do
        primaryImage   <- runInPrimaryWidget   (draw primaryWidget)
        secondaryImage <- runInSecondaryWidget (draw secondaryWidget)
        pure (primaryImage <|> secondaryImage)

getCursor :: Widget s -> Widget t -> Layout s t -> Cursor
getCursor primaryWidget secondaryWidget = view focus >>= \case
    PrimaryOnly    -> magnify primary   (cursor primaryWidget)
    FocusPrimary   -> magnify primary   (cursor primaryWidget)   -- FIXME
    FocusSecondary -> magnify secondary (cursor secondaryWidget) -- FIXME
    SecondaryOnly  -> magnify secondary (cursor secondaryWidget)

runInPrimaryWidget
    :: Monad m
    => VgrepT s m Image
    -> VgrepT (Layout s t) m Image
runInPrimaryWidget action = do
    dimension <- use (orientation . to regionDimension)
    scale <- use splitRatio <&> \case
        FixedPrimary   pdim -> const pdim
        FixedSecondary sdim -> \dim -> sdim - dim
        Dynamic r           -> \dim -> floor ((1-r) * fromIntegral dim)
    zoom primary (local (over (region . dimension) scale) action)

runInSecondaryWidget
    :: Monad m
    => VgrepT t m Image
    -> VgrepT (Layout s t) m Image
runInSecondaryWidget action = do
    dimension <- use (orientation . to regionDimension)
    scale <- use splitRatio <&> \case
        FixedPrimary   pdim -> \dim -> pdim - dim
        FixedSecondary sdim -> const sdim
        Dynamic r           -> \dim -> ceiling (r * fromIntegral dim)
    zoom secondary (local (over (region . dimension) scale) action)

regionDimension :: Orientation -> Lens' DisplayRegion Int
regionDimension = \case
    Horizontal -> _1
    Vertical   -> _2


-- ------------------------------------------------------------------------
-- Events & Keybindings
-- ------------------------------------------------------------------------

-- FIXME: local region!
handleEvents
    :: Monad m
    => Widget s
    -> Widget t
    -> Event
    -> Layout s t
    -> Next (VgrepT (Layout s t) m Redraw)
handleEvents primaryWidget secondaryWidget e s = case view currentWidget s of
    Left  ls -> hSplitKeyBindingsLeft e
             <> fmap (zoom primary) (handle primaryWidget   e ls)
    Right rs -> hSplitKeyBindingsRight e
             <> fmap (zoom secondary) (handle secondaryWidget e rs)

hSplitKeyBindingsLeft
    :: Monad m
    => Event
    -> Next (VgrepT (Layout s t) m Redraw)
hSplitKeyBindingsLeft = dispatchMap $ fromList
    [ (EvKey (KChar '\t') [], switchFocus)
    , (EvKey (KChar 'f')  [], primaryOnly) ]

hSplitKeyBindingsRight
    :: Monad m
    => Event
    -> Next (VgrepT (Layout s t) m Redraw)
hSplitKeyBindingsRight = dispatchMap $ fromList
    [ (EvKey (KChar '\t') [], switchFocus)
    , (EvKey (KChar 'q')  [], primaryOnly)
    , (EvKey (KChar 'f')  [], secondaryOnly) ]

primaryOnly :: Monad m => VgrepT (Layout s t) m Redraw
primaryOnly = use focus >>= \case
    PrimaryOnly -> pure Unchanged
    _other      -> assign focus PrimaryOnly >> pure Redraw

secondaryOnly :: Monad m => VgrepT (Layout s t) m Redraw
secondaryOnly = use focus >>= \case
    SecondaryOnly -> pure Unchanged
    _other        -> assign focus SecondaryOnly >> pure Redraw

splitView :: Monad m => VgrepT (Layout s t) m Redraw
splitView = use focus >>= \case
    PrimaryOnly   -> assign focus FocusPrimary   >> pure Redraw
    SecondaryOnly -> assign focus FocusSecondary >> pure Redraw
    _otherwise    -> pure Unchanged

switchFocus :: Monad m => VgrepT (Layout s t) m Redraw
switchFocus = use focus >>= \case
    FocusPrimary   -> do
        assign focus FocusSecondary
        flipDynamicRatio
        pure Redraw
    FocusSecondary -> do
        assign focus FocusPrimary
        flipDynamicRatio
        pure Redraw
    _otherwise -> pure Unchanged
  where
    flipDynamicRatio = modifying splitRatio $ \case
        Dynamic r  -> Dynamic (1-r)
        fixedRatio -> fixedRatio
