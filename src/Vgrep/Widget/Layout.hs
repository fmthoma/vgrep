{-# LANGUAGE Rank2Types #-}
module Vgrep.Widget.Layout (
    -- * Layout widget
      layoutWidget
    , LayoutWidget
    , hSplitWidget

    -- ** Widget state
    , Layout ()
    , Focus (..)
    , Ratio (..)
    , Orientation (..)

    -- ** Widget actions
    , primaryOnly
    , secondaryOnly
    , splitView
    , switchFocus

    -- ** Lenses
    , primary
    , secondary
    , focusedWidget
    , splitRatio
    , focus

    -- ** Re-exports
    , (%)
    ) where

import Control.Lens
import Data.Monoid
import Data.Ratio         ((%))
import Graphics.Vty       ((<|>), (<->))
import Graphics.Vty.Input

import Vgrep.Event
import Vgrep.Type
import Vgrep.Widget.Layout.Internal
import Vgrep.Widget.Type


type LayoutWidget s t = Widget (Layout s t)

-- | Compose two 'Widget's side-by-side
--
-- * __Initial state__
--
--     Initially, the left widget is rendered full-screen.
--
-- * __Default keybindings__
--
--     Events are routed to the focused widget. Additionally, the
--     following keybindings are defined:
--
--     @
--     Tab   'switchFocus'
--     f     full screen ('leftOnly' / 'rightOnly')
--     q     close right widget ('leftOnly' if right widget is focused)
--     @
hSplitWidget :: Widget s -> Widget t -> LayoutWidget s t
hSplitWidget primaryWidget secondaryWidget =
    ( layoutWidget
        primaryWidget
        secondaryWidget
        Horizontal
        (Dynamic (2%3))
        PrimaryOnly )
    { handle = hSplitKeyBindings <> delegateEvents primaryWidget secondaryWidget }


-- | Compose two 'Widget's with the given layout
--
-- * __Drawing the Widgets__
--
--     Drawing is delegated to the child widgets in a local environment
--     reduced to thir respective 'DisplayRegion'.
--
-- * __Default keybindings__
--
--     None, although specialized versions like 'hSplitWidget' provide
--     keybindings.
layoutWidget
    :: Widget s
    -> Widget t
    -> Orientation
    -> Ratio
    -> Focus
    -> LayoutWidget s t
layoutWidget primaryWidget secondaryWidget orientation' ratio' focus' = Widget
    { initialize = Layout
        { _primary = initialize primaryWidget
        , _secondary = initialize secondaryWidget
        , _orientation = orientation'
        , _splitRatio = ratio'
        , _focus = focus' }
    , draw       = drawLayout     primaryWidget secondaryWidget
    , cursor     = getCursor      primaryWidget secondaryWidget
    , handle     = delegateEvents primaryWidget secondaryWidget }


drawLayout :: Monad m => Widget s -> Widget t -> VgrepT (Layout s t) m Image
drawLayout primaryWidget secondaryWidget = use focus >>= \case
    PrimaryOnly    -> zoom primary   (draw primaryWidget)
    SecondaryOnly  -> zoom secondary (draw secondaryWidget)
    _splitView     -> do
        primaryImage   <- runInPrimaryWidget   (draw primaryWidget)
        secondaryImage <- runInSecondaryWidget (draw secondaryWidget)
        use orientation >>= \case
            Horizontal -> pure (primaryImage <|> secondaryImage)
            Vertical   -> pure (primaryImage <-> secondaryImage)

getCursor :: Monad m => Widget s -> Widget t -> VgrepT (Layout s t) m Cursor
getCursor primaryWidget secondaryWidget = use focus >>= \case
    PrimaryOnly    -> zoom primary   (cursor primaryWidget)
    FocusPrimary   -> zoom primary   (cursor primaryWidget)   -- FIXME
    FocusSecondary -> zoom secondary (cursor secondaryWidget) -- FIXME
    SecondaryOnly  -> zoom secondary (cursor secondaryWidget)

runInPrimaryWidget
    :: Monad m
    => VgrepT s m Image
    -> VgrepT (Layout s t) m Image
runInPrimaryWidget action = do
    dimension <- use (orientation . to regionDimension)
    scale <- use splitRatio <&> \case
        FixedPrimary   pdim -> const pdim
        FixedSecondary sdim -> \dim -> dim - sdim
        Dynamic r           -> \dim -> floor ((1-r) * fromIntegral dim)
    zoom primary (local (over (region . dimension) scale) action)

runInSecondaryWidget
    :: Monad m
    => VgrepT t m Image
    -> VgrepT (Layout s t) m Image
runInSecondaryWidget action = do
    dimension <- use (orientation . to regionDimension)
    scale <- use splitRatio <&> \case
        FixedPrimary   pdim -> \dim -> dim - pdim
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

delegateEvents
    :: Monad m
    => Widget s
    -> Widget t
    -> Event
    -> Layout s t
    -> Next (VgrepT (Layout s t) m Redraw)
delegateEvents primaryWidget secondaryWidget e s = case view focusedWidget s of
    Left  ls -> fmap (zoom primary) (handle primaryWidget   e ls)
    Right rs -> fmap (zoom secondary) (handle secondaryWidget e rs)

hSplitKeyBindings
    :: Monad m
    => Event
    -> Layout s t
    -> Next (VgrepT (Layout s t) m Redraw)
hSplitKeyBindings e s = case view focusedWidget s of
    Left  _ -> hSplitKeyBindingsLeft e
    Right _ -> hSplitKeyBindingsRight e

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

-- | Display the primary wdiget full-screen
primaryOnly :: Monad m => VgrepT (Layout s t) m Redraw
primaryOnly = use focus >>= \case
    PrimaryOnly -> pure Unchanged
    _other      -> assign focus PrimaryOnly >> pure Redraw

-- | Display the secondary widget full-screen
secondaryOnly :: Monad m => VgrepT (Layout s t) m Redraw
secondaryOnly = use focus >>= \case
    SecondaryOnly -> pure Unchanged
    _other        -> assign focus SecondaryOnly >> pure Redraw

-- | Display both widgets in a split view
splitView :: Monad m => VgrepT (Layout s t) m Redraw
splitView = use focus >>= \case
    PrimaryOnly   -> assign focus FocusPrimary   >> pure Redraw
    SecondaryOnly -> assign focus FocusSecondary >> pure Redraw
    _otherwise    -> pure Unchanged

-- | Switch focus from left to right child widget and vice versa (only if
-- the '_layout' is 'Split')
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
