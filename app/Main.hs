{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Main where

import Data.Foldable
import Data.Sequence hiding (update)
import Graphics.Vty
import System.Posix

main :: IO ()
main = do
    ls <- fmap lines (getContents)
    cfg <- standardIOConfig
    tty <- openFd "/dev/tty" ReadOnly Nothing defaultFileFlags
    vty <- mkVty (cfg { inputFd = Just tty })
    let initialState = PagerState { bufferPre   = empty
                           , currentLine = head ls
                           , bufferPost  = fromList (tail ls) }
    _finalState <- eventLoop vty handleEvent render initialState
    shutdown vty

data PagerState = PagerState { bufferPre   :: Seq String
                             , currentLine :: String
                             , bufferPost  :: Seq String }

render :: Vty -> PagerState -> IO ()
render display state@PagerState{..} =
    let dpls = (fmap fore bufferPre |> back currentLine) >< fmap fore bufferPost
        img = fold dpls
        pic = picForImage img
    in  update display pic
  where
    fore = string (defAttr `withForeColor` green) . (' ':)
    back = string (defAttr `withBackColor` blue) . (' ':)

handleEvent :: EventHandler Event PagerState
handleEvent state = \case
    EvKey KUp         [] -> return (Continue (previousLine state))
    EvKey KDown       [] -> return (Continue (nextLine state))
    EvKey (KChar 'q') [] -> return (Halt state)
    EvKey (KChar 'd') [] -> return (Continue (deleteLine state))
    EvKey (KChar 'D') [] -> return (Continue ((deleteLine . previousLine) state))
    _                    -> return (Continue state)

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



type EventHandler e s = s -> e -> IO (Next s)
type Renderer s = Vty -> s -> IO ()

data Next s = Continue s
            | Halt s

eventLoop :: Vty -> EventHandler Event s -> Renderer s -> s -> IO s
eventLoop vty handler renderer state = do
    renderer vty state
    event <- nextEvent vty
    next <- handler state event
    case next of Continue newState -> eventLoop vty handler renderer newState
                 Halt     newState -> return newState
