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
import Vgrep.Widget.Results

main :: IO ()
main = do
    _finalState <- runApp app
    return ()


type MainWidget = HSplitWidget PagerWidget ResultsWidget

app :: App MainWidget
app = App { _initialize  = initSplitView
          , _handleEvent = eventHandler
          , _render      = picForImage . drawWidget }

initSplitView :: Vty -> IO MainWidget
initSplitView vty = do
    ls <- fmap lines (getContents)
    displayRegion <- displayBounds (outputIface vty)
    let leftPager  = pagerWidget ls displayRegion
        rightPager = resultsWidget displayRegion
                     $ [ ("foo.hs", [ (123, "foobar")
                                    , (32,  "hello world")
                                    , (456, "bar") ] )
                       , ("ba1.hs", [ (1,   "bar")
                                    , (32,  "hello world")
                                    , (123, "baz") ] )
                       , ("ba2.hs", [ (1,   "bar")
                                    , (32,  "hello")
                                    , (320, "hello world")
                                    , (100000000,   "bar")
                                    , (32,  "hello world")
                                    , (123, "baz") ] )
                       , ("ba3.hs", [ (1,   "bar")
                                    , (32,  "hello world")
                                    , (123, "baz") ] )
                       , ("ba4.hs", [ (1,   "bar")
                                    , (32,  "hello")
                                    , (320, "hello world")
                                    , (123, "baz") ] )
                       , ("ba5.hs", [ (1,   "bar")
                                    , (32,  "hello world")
                                    , (123, "baz") ] )
                       , ("ba6.hs", [ (1,   "bar")
                                    , (32,  "hello world")
                                    , (123, "baz") ] )
                       , ("ba7.hs", [ (1,   "bar")
                                    , (32,  "hello")
                                    , (320, "hello world")
                                    , (123, "baz") ] )
                       , ("ba8.hs", [ (1,   "bar")
                                    , (32,  "hello world")
                                    , (123, "baz") ] )
                       , ("ba9.hs", [ (1,   "bar")
                                    , (123, "baz") ] ) ]
    displayRegion <- displayBounds (outputIface vty)
    return (hSplitWidget leftPager rightPager (1 % 3) displayRegion)

eventHandler :: EventHandler MainWidget
eventHandler = exitOn (KChar 'q') []
            <> handleResizeEvent
            <> passEventsToWidget

handleResizeEvent :: EventHandler MainWidget
handleResizeEvent = EventHandler $ \event widget -> case event of
    EvResize w h -> Continue (resizeWidget widget (w, h))
    _            -> Unchanged
