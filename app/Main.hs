module Main where

import Data.Monoid
import Graphics.Vty

import Vgrep.App
import Vgrep.Event
import Vgrep.Widget.Pager

main :: IO ()
main = do
    _finalState <- runApp app
    return ()


app :: App Event PagerState
app = App { initialize  = initPager
          , liftEvent   = id
          , handleEvent = eventHandler
          , render      = renderPager }

initPager :: Vty -> IO PagerState
initPager vty = do
    ls <- fmap lines (getContents)
    displayRegion <- displayBounds (outputIface vty)
    return (initialPagerState ls displayRegion)

eventHandler :: EventHandler Event PagerState
eventHandler = exitOn (KChar 'q') []
            <> handleResize resizeToRegion
            <> handlePagerEvents

