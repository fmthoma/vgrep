{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Vgrep.App where

import Control.Applicative
import Graphics.Vty as Vty
import System.Posix

data App e s = App { liftEvent   :: Vty.Event -> e
                   , handleEvent :: EventHandler e s
                   , render      :: Renderer s }

newtype EventHandler e s = EventHandler { handle :: s -> e -> IO (Next s) }

instance Monoid (EventHandler e s) where
    mempty = EventHandler $ \_ _ -> return Unchanged
    h1 `mappend` h2 = EventHandler $ \state event ->
        liftA2 mappend (handle h1 state event)
                       (handle h2 state event)

type Renderer s = s -> Vty.Picture

data Next s = Continue s
            | Halt s
            | Unchanged

instance Monoid (Next s) where
    mempty = Unchanged
    Unchanged `mappend` next = next
    next      `mappend` _    = next


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
