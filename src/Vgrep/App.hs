{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Vgrep.App where

import Control.Exception (bracket)
import Control.Lens
import Graphics.Vty (Vty, Config(..))
import qualified Graphics.Vty as Vty
import System.Posix

import Vgrep.Event

data App s = App { _initialize  :: Vty -> IO s
                 , _handleEvent :: EventHandler s
                 , _render      :: s -> Vty.Picture }

makeLenses ''App

runApp :: App s -> IO s
runApp app = do
    next <- bracket initVty Vty.shutdown $ \vty -> do
        initialState <- (view initialize app) vty
        eventLoop vty app initialState
    suspendAndResumeLoop next
  where
    suspendAndResumeLoop = \case
        Halt finalState     -> pure finalState
        Resume continuation -> do
            newState <- continuation
            next <- bracket initVty Vty.shutdown $ \vty ->
                eventLoop vty app newState
            suspendAndResumeLoop next
        _ -> error "Internal error: Unhandled Continuation"
             -- All others must be handled in eventLoop

initVty :: IO Vty
initVty = do
    cfg <- Vty.standardIOConfig
    ttyIn  <- openFd "/dev/tty" ReadOnly  Nothing defaultFileFlags
    ttyOut <- openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
    Vty.mkVty (cfg { inputFd = Just ttyIn , outputFd = Just ttyOut })

eventLoop :: Vty -> App s -> s -> IO (Next s)
eventLoop vty app initialState = do
    refresh initialState
    loop initialState
  where
    loop currentState = do
        event <- Vty.nextEvent vty
        next <- runEventHandler handleAppEvent event currentState
        case next of
            Unchanged         -> loop currentState
            Continue newState -> do refresh newState
                                    loop newState
            other             -> pure other
    refresh currentState = Vty.update vty (renderApp currentState)
    renderApp = view render app
    handleAppEvent = view handleEvent app
