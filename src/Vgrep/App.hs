{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Vgrep.App
    ( App(..)
    , runApp
    , ttyIn
    , ttyOut
    ) where

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
runApp app = startEventLoop >>= suspendAndResume
  where
    startEventLoop = withVty $ \vty -> do
        initialState <- (view initialize app) vty
        refresh vty initialState
        eventLoop vty initialState

    continueEventLoop currentState = withVty $ \vty -> do
        refresh vty currentState
        eventLoop vty currentState

    eventLoop vty currentState = do
        event <- Vty.nextEvent vty
        next <- runEventHandler handleAppEvent event currentState
        case next of
            Unchanged         -> eventLoop vty currentState
            Continue newState -> refresh vty newState >> eventLoop vty newState
            other             -> pure other

    suspendAndResume = \case
        Halt finalState      -> pure finalState
        Resume outsideAction -> outsideAction >>= continueEventLoop >>= suspendAndResume
        _other               -> cannotHappen_othersAreHandledInEventLoop

    refresh vty = Vty.update vty . renderApp
    renderApp = view render app
    handleAppEvent = view handleEvent app

    cannotHappen_othersAreHandledInEventLoop =
        error "Internal error: Unhandled Continuation"


withVty :: (Vty -> IO s) -> IO s
withVty = bracket initVty Vty.shutdown

initVty :: IO Vty
initVty = do
    cfg <- Vty.standardIOConfig
    fdIn  <- ttyIn
    fdOut <- ttyOut
    Vty.mkVty (cfg { inputFd = Just fdIn , outputFd = Just fdOut })

ttyIn, ttyOut :: IO Fd
ttyIn  = openFd "/dev/tty" ReadOnly  Nothing defaultFileFlags
ttyOut = openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
