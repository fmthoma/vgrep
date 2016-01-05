{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Main where

import Data.Foldable
import Data.Monoid
import Data.Sequence hiding (update)
import Graphics.Vty

import Vgrep.App
import Vgrep.Event

main :: IO ()
main = do
    _finalState <- runApp app
    return ()

data PagerState = PagerState { bufferPre   :: Seq String
                             , currentLine :: String
                             , bufferPost  :: Seq String
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
                        , region      = displayRegion }

renderer :: Renderer PagerState
renderer PagerState{..} =
    let dpls = (fmap fore bufferPre |> back currentLine) >< fmap fore bufferPost
    in  picForImage (fold dpls)
  where
    fore = string (defAttr `withForeColor` green) . (' ':)
    back = string (defAttr `withBackColor` blue) . (' ':)

eventHandler :: EventHandler Event PagerState
eventHandler = exitOn    (KChar 'q') []
            <> handleKey KUp         [] previousLine
            <> handleKey KDown       [] nextLine
            <> handleKey (KChar 'd') [] deleteLine
            <> handleKey (KChar 'D') [] (deleteLine . previousLine)
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
resizeToRegion newRegion state = state { region = newRegion }
