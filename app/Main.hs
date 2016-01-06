module Main where

import Data.Monoid
import Graphics.Vty

import Vgrep.App
import Vgrep.Event
import Vgrep.Widget.List

main :: IO ()
main = do
    _finalState <- runApp app
    return ()


app :: App Event ListState
app = App { initialize  = initList
          , liftEvent   = id
          , handleEvent = eventHandler
          , render      = renderList }

initList :: Vty -> IO ListState
initList vty = do
    ls <- fmap lines (getContents)
    displayRegion <- displayBounds (outputIface vty)
    return (initialListState ls displayRegion)

eventHandler :: EventHandler Event ListState
eventHandler = exitOn    (KChar 'q') []
            <> handleKey (KChar 'd') [] (updateScrollPos . deleteLine)
            <> handleKey (KChar 'D') [] (updateScrollPos . deleteLine . previousLine)
            <> handleResize             resizeToRegion
            <> handleListEvents

