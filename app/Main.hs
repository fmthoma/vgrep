{-# LANGUAGE DisambiguateRecordFields #-}
module Main where

import Data.Monoid
import Data.Ratio
import Graphics.Vty hiding (resize)

import Vgrep.App
import Vgrep.Event
import Vgrep.Widget as Widget
import Vgrep.Widget.HorizontalSplit
import Vgrep.Widget.Pager

main :: IO ()
main = do
    _finalState <- runApp app
    return ()


type MainWidget = HSplitWidget PagerWidget PagerWidget

app :: App MainWidget
app = App { initialize  = initSplitView
          , handleEvent = eventHandler
          , render      = picForImage . drawWidget }

initSplitView :: Vty -> IO MainWidget
initSplitView vty = do
    ls <- fmap lines (getContents)
    displayRegion <- displayBounds (outputIface vty)
    let leftPager  = pagerWidget ls displayRegion
        rightPager = pagerWidget (reverse ls) displayRegion
    displayRegion <- displayBounds (outputIface vty)
    return (hSplitWidget leftPager rightPager (1 % 3) displayRegion)

eventHandler :: EventHandler MainWidget
eventHandler = exitOn (KChar 'q') []
            <> handleResizeEvent
            <> passEventsToWidget

handleResizeEvent :: EventHandler MainWidget
handleResizeEvent = EventHandler $ \event widget -> case event of
    EvResize w h -> return . Continue $ resizeWidget widget (w, h)
    _            -> return Unchanged
