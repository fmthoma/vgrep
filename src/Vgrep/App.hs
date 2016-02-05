{-# LANGUAGE TemplateHaskell #-}
module Vgrep.App
    ( App(..)
    , runApp, runApp_
    , ttyIn
    , ttyOut
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Text.Lazy
import Graphics.Vty (Vty, Config(..))
import qualified Graphics.Vty as Vty
import Pipes (Consumer)
import System.Posix

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type

data App s = App { initialize   :: Vty -> VgrepT IO s
                 , receiveInput :: Consumer Text (StateT s Vgrep) ()
                 , handleEvent  :: EventHandler s
                 , render       :: s -> Vgrep Vty.Picture }


runApp_ :: App s -> Environment -> Consumer Text IO ()
runApp_ app env = runApp app env >> pure ()

runApp :: App s -> Environment -> Consumer Text IO s
runApp app env = lift (runVgrepT env (startEventLoop >>= suspendAndResume))
  where
    startEventLoop = withVty $ \vty -> do
        initialState <- initialize app vty
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
    renderApp = render app
    handleAppEvent = handleEvent app

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
