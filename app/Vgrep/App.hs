{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Vgrep.App where

import Graphics.Vty as Vty
import System.Posix

import Vgrep.Event

data App s = App { initialize  :: Vty -> IO s
                 , handleEvent :: EventHandler s
                 , render      :: s -> Vty.Picture }


runApp :: App s -> IO s
runApp app = do
    vty <- initVty
    initialState <- initialize app vty
    finalState <- eventLoop vty app initialState
    shutdown vty
    return finalState

initVty :: IO Vty
initVty = do
    cfg <- standardIOConfig
    ttyIn  <- openFd "/dev/tty" ReadOnly  Nothing defaultFileFlags
    ttyOut <- openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
    mkVty (cfg { inputFd = Just ttyIn , outputFd = Just ttyOut })

eventLoop :: Vty -> App s -> s -> IO s
eventLoop vty app@App{..} initialState = do
    refresh initialState
    runLoop initialState
  where
    runLoop currentState = do
        event <- nextEvent vty
        let next = handle handleEvent event currentState
        case next of
            Unchanged         -> runLoop currentState
            Halt     newState -> return newState
            Continue newState -> refresh newState >> runLoop newState
    refresh currentState = update vty (render currentState)
