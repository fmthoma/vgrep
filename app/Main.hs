{-# LANGUAGE RecordWildCards, LambdaCase, MultiWayIf #-}
module Main where

import Data.Foldable
import Data.Monoid
import Data.Sequence hiding (update, take, empty)
import qualified Data.Sequence as Seq
import Graphics.Vty
import Graphics.Vty.Prelude

import Vgrep.App
import Vgrep.Event

main :: IO ()
main = do
    _finalState <- runApp app
    return ()

data PagerState = PagerState { bufferPre   :: Seq String
                             , currentLine :: String
                             , bufferPost  :: Seq String
                             , scrollPos   :: Int
                             , region      :: DisplayRegion }

app :: App Event PagerState
app = App { initialize  = initPager
          , liftEvent   = id
          , handleEvent = eventHandler
          , render      = renderer }

initPager :: Vty -> IO PagerState
initPager vty = do
    ls <- fmap lines (getContents)
    displayRegion <- displayBounds (outputIface vty)
    return $ PagerState { bufferPre   = empty
                        , currentLine = head ls
                        , bufferPost  = fromList (tail ls)
                        , scrollPos   = 0
                        , region      = displayRegion }

renderer :: Renderer PagerState
renderer PagerState{..} =
    let dpls = (fmap fore bufferPre |> back currentLine) >< fmap fore bufferPost
    in  (picForImage . fold . Seq.drop scrollPos) dpls
  where
    fore = string (defAttr `withForeColor` green) . truncate
    back = string (defAttr `withBackColor` blue) . truncate
    truncate s = let width = (regionWidth region `div` 2) - 1
                 in  take width (' ' : s ++ repeat ' ') ++ " "

eventHandler :: EventHandler Event PagerState
eventHandler = exitOn    (KChar 'q') []
            <> handleKey KUp         [] (updateScrollPos . previousLine)
            <> handleKey KDown       [] (updateScrollPos . nextLine)
            <> handleKey (KChar 'd') [] (updateScrollPos . deleteLine)
            <> handleKey (KChar 'D') [] (updateScrollPos . deleteLine . previousLine)
            <> handleResize             resizeToRegion

nextLine :: PagerState -> PagerState
nextLine state@PagerState{..} = case viewl bufferPost of
    EmptyL  -> state
    l :< ls -> state { bufferPre   = bufferPre |> currentLine
                     , currentLine = l
                     , bufferPost  = ls }

previousLine :: PagerState -> PagerState
previousLine state@PagerState{..} = case viewr bufferPre of
    EmptyR  -> state
    ls :> l -> state { bufferPre   = ls
                     , currentLine = l
                     , bufferPost  = currentLine <| bufferPost }

deleteLine :: PagerState -> PagerState
deleteLine state@PagerState{..} = case viewl bufferPost of
    EmptyL  -> state
    l :< ls -> state { currentLine = l
                     , bufferPost  = ls }

resizeToRegion :: DisplayRegion -> PagerState -> PagerState
resizeToRegion newRegion state = updateScrollPos $ state { region = newRegion }

updateScrollPos :: PagerState -> PagerState
updateScrollPos state@PagerState{..} =
    if | current < firstVisible -> state { scrollPos = current }
       | current > lastVisible  -> state { scrollPos = current - height + 1 }
       | otherwise              -> state
  where
    height       = regionHeight region
    current      = Seq.length bufferPre
    firstVisible = scrollPos
    lastVisible  = scrollPos + height - 1
