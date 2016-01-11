{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Vgrep.Widget.HorizontalSplit ( HSplitState()
                                    , HSplitWidget
                                    , hSplitWidget
                                    ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Data.Monoid
import Graphics.Vty hiding (resize)
import Graphics.Vty.Prelude

import Vgrep.Event
import Vgrep.Widget.Type


data HSplitState s t = State { _widgets :: (s, t)
                             , _focused :: Focus
                             , _ratio   :: Rational
                             , _region  :: DisplayRegion }

data Focus = FocusLeft | FocusRight

makeLenses ''HSplitState

leftWidget :: Lens' (HSplitState s t) s
leftWidget = widgets . _1

rightWidget :: Lens' (HSplitState s t) t
rightWidget = widgets . _2

type HSplitWidget s t = Widget (HSplitState s t)

hSplitWidget :: Widget s
             -> Widget t
             -> Rational
             -> DisplayRegion
             -> HSplitWidget (Widget s) (Widget t)
hSplitWidget left right ratio region =
    Widget { _widgetState = initState left right ratio region
           , _dimensions  = region
           , _resize      = resizeWidgets
           , _draw        = drawWidgets
           , _handleEvent = switchFocusOn (KChar '\t')
                         <> passEventToFocusedWidget }

initState :: Widget s
          -> Widget t
          -> Rational
          -> DisplayRegion
          -> HSplitState (Widget s) (Widget t)
initState left right ratio region = execState (resizeWidgets region) $
    State { _widgets = (left, right)
          , _focused = FocusLeft
          , _ratio   = ratio
          , _region  = region }


switchFocusOn :: Key -> EventHandler (HSplitState (Widget s) (Widget t))
switchFocusOn key = handleKey key [] $ do
    modifying focused switch
    modifying ratio (\r -> 1 - r)
    use region >>= resizeWidgets
  where
    switch FocusLeft  = FocusRight
    switch FocusRight = FocusLeft

passEventToFocusedWidget :: EventHandler (HSplitState (Widget s) (Widget t))
passEventToFocusedWidget = EventHandler $ \event state ->
    case view focused state of
        FocusLeft  -> passEventTo leftWidget  event state
        FocusRight -> passEventTo rightWidget event state

passEventTo :: Lens' (HSplitState (Widget s) (Widget t)) (Widget u)
            -> Event
            -> HSplitState (Widget s) (Widget t)
            -> Next (HSplitState (Widget s) (Widget t))
passEventTo selector event =
    over (liftNext selector) (handle passEventsToWidget event)

liftNext :: Lens' s a -> Lens s (Next s) a (Next a)
liftNext l = lens (view l) (\s -> fmap (\a -> set l a s))

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
