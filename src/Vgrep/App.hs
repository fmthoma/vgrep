{-# LANGUAGE TemplateHaskell #-}
module Vgrep.App
    ( App(..)
    , runApp, runApp_
    , ttyIn
    , ttyOut
    ) where

import Control.Lens
import Control.Monad.Reader
import Graphics.Vty (Vty, Config(..))
import qualified Graphics.Vty as Vty
import System.Posix

import Vgrep.Event
import Vgrep.Type

data App s = App { _initialize  :: Vty -> VgrepT IO s
                 , _handleEvent :: EventHandler s
                 , _render      :: s -> Vgrep Vty.Picture }

makeLenses ''App


runApp_ :: App s -> VgrepT IO ()
runApp_ app = runApp app >> pure ()

runApp :: App s -> VgrepT IO s
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
        event <- liftIO (Vty.nextEvent vty)
        next <- runEventHandler handleAppEvent event currentState
        case next of
            Unchanged         -> eventLoop vty currentState
            Continue newState -> refresh vty newState >> eventLoop vty newState
            other             -> pure other

    suspendAndResume = \case
        Halt finalState      -> pure finalState
        Resume outsideAction -> outsideAction >>= continueEventLoop >>= suspendAndResume
        _other               -> cannotHappen_othersAreHandledInEventLoop

    refresh vty = liftVgrep . fmap (Vty.update vty) . renderApp
    renderApp = view render app
    handleAppEvent = view handleEvent app

    cannotHappen_othersAreHandledInEventLoop =
        error "Internal error: Unhandled Continuation"


withVty :: (Vty -> VgrepT IO s) -> VgrepT IO s
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
