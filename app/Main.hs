{-# LANGUAGE DisambiguateRecordFields #-}
module Main where

import Data.Monoid
import Graphics.Vty hiding (resize)

import Vgrep.App
import Vgrep.Event
import Vgrep.Widget as Widget
import Vgrep.Widget.Pager

main :: IO ()
main = do
    _finalState <- runApp app
    return ()


app :: App PagerWidget
app = App { initialize  = initPager
          , handleEvent = eventHandler
          , render      = picForImage . drawWidget }

initPager :: Vty -> IO PagerWidget
initPager vty = do
    ls <- fmap lines (getContents)
    displayRegion <- displayBounds (outputIface vty)
    return (pagerWidget ls displayRegion)

eventHandler :: EventHandler PagerWidget
eventHandler = exitOn (KChar 'q') []
            <> handleResizeEvent
            <> passEventsToWidget

handleResizeEvent :: EventHandler PagerWidget
handleResizeEvent = EventHandler $ \event widget -> case event of
    EvResize w h -> return . Continue $ widget
                        { state = resize widget (w, h) (state widget) }
    _            -> return Unchanged
