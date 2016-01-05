{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Vgrep.App where

import Graphics.Vty as Vty
import System.Posix

import Vgrep.Event

data App e s = App { liftEvent   :: Vty.Event -> e
                   , handleEvent :: EventHandler e s
                   , render      :: Renderer s }

type Renderer s = s -> Vty.Picture


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
eventLoop vty app@App{..} initialState = do
    refresh initialState
    runLoop initialState
  where
    runLoop currentState = do
        event <- nextEvent vty
        next <- handle handleEvent currentState (liftEvent event)
        case next of
            Unchanged         -> runLoop currentState
            Halt     newState -> return newState
            Continue newState -> refresh newState >> runLoop newState
    refresh currentState = update vty (render currentState)
