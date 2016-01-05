{-# LANGUAGE RecordWildCards #-}
module Vgrep.App where

import Graphics.Vty as Vty
import System.Posix

data App e s = App { liftEvent   :: Vty.Event -> e
                   , handleEvent :: EventHandler e s
                   , render      :: Renderer s }

type EventHandler e s = s -> e -> IO (Next s)

type Renderer s = Vty -> s -> IO ()

data Next s = Continue s
            | Halt s

runApp :: App e s -> s -> IO s
runApp app initialState = do
    vty <- initVty
    finalState <- eventLoop vty app initialState
    shutdown vty
    return finalState

initVty :: IO Vty
initVty = do
    cfg <- standardIOConfig
    tty <- openFd "/dev/tty" ReadOnly Nothing defaultFileFlags
    mkVty (cfg { inputFd = Just tty })

eventLoop :: Vty -> App e s -> s -> IO s
eventLoop vty app@App{..} currentState = do
    render vty currentState
    event <- nextEvent vty
    next <- handleEvent currentState (liftEvent event)
    case next of
        Continue newState -> eventLoop vty app newState
        Halt     newState -> return newState
