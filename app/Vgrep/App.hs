{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Vgrep.App where

import Graphics.Vty as Vty
import System.Posix

data App e s = App { liftEvent   :: Vty.Event -> e
                   , handleEvent :: EventHandler e s
                   , render      :: Renderer s }

newtype EventHandler e s = EventHandler { handle :: s -> e -> IO (Next s) }

instance Monoid (EventHandler e s) where
    mempty = EventHandler $ \_ _ -> return Unchanged
    h1 `mappend` h2 = EventHandler $ \state event -> do
        res1 <- handle h1 state event
        case res1 of
            Unchanged -> handle h2 state event
            next      -> return res1

type Renderer s = s -> Vty.Picture

data Next s = Continue s
            | Halt s
            | Unchanged

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
    update vty (render currentState)
    event <- nextEvent vty
    next <- handle handleEvent currentState (liftEvent event)
    case next of
        Unchanged         -> eventLoop vty app currentState
        Continue newState -> eventLoop vty app newState
        Halt     newState -> return newState
