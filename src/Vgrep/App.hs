{-# LANGUAGE TemplateHaskell #-}
module Vgrep.App where

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
    vty <- initVty
    initialState <- (view initialize app) vty
    finalState <- eventLoop vty app initialState
    Vty.shutdown vty
    return finalState

initVty :: IO Vty
initVty = do
    cfg <- Vty.standardIOConfig
    ttyIn  <- openFd "/dev/tty" ReadOnly  Nothing defaultFileFlags
    ttyOut <- openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
    Vty.mkVty (cfg { inputFd = Just ttyIn , outputFd = Just ttyOut })

eventLoop :: Vty -> App s -> s -> IO s
eventLoop vty app initialState = do
    refresh initialState
    runLoop initialState
  where
    runLoop currentState = do
        event <- Vty.nextEvent vty
        let next = handle handleAppEvent event currentState
        case next of
            Unchanged         -> runLoop currentState
            Halt     newState -> newState
            Continue newState -> do refresh =<< newState
                                    runLoop =<< newState
    refresh currentState = Vty.update vty (renderApp currentState)
    renderApp = view render app
    handleAppEvent = view handleEvent app
