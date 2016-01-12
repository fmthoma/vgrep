{-# LANGUAGE TemplateHaskell #-}
module Vgrep.App where

import Control.Lens
import Control.Lens.TH
import Graphics.Vty as Vty
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
    shutdown vty -- CR/quchen: Is the shutdown necessary? If yes, consider
                 --            bracket and friends
    return finalState -- CR/quchen: Just my opinion, but I don't write
                      --            return ever anymore, now that we have
                      --            pure. return is misleading to non-
                      --            Haskellers, and pure is a much better name
                      --            anyway: `pure x` has exactly the semantics
                      --            of `x` and nothing more in the lifted
                      --            context.

initVty :: IO Vty
initVty = do
    cfg <- standardIOConfig
    ttyIn  <- openFd "/dev/tty" ReadOnly  Nothing defaultFileFlags -- CR/quchen: What's the Nothing for?
    ttyOut <- openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
    mkVty (cfg { inputFd = Just ttyIn , outputFd = Just ttyOut })

eventLoop :: Vty -> App s -> s -> IO s
eventLoop vty app initialState = do
    refresh initialState
    runLoop initialState
  where
    -- CR/quchen: mostly simply called `loop`
    runLoop currentState = do
        event <- nextEvent vty
        let next = handle (view handleEvent app) event currentState
        case next of
            Unchanged         -> runLoop currentState
            Halt     newState -> newState
            Continue newState -> do refresh =<< newState
                                    runLoop =<< newState
    refresh currentState = update vty (renderApp currentState)
    renderApp = view render app
    handleAppEvent = view handleEvent app
