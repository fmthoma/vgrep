{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types      #-}
module Vgrep.App
    ( App(..)
    , runApp, runApp_
    , ttyIn
    , ttyOut
    ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import Graphics.Vty (Vty)
import qualified Graphics.Vty as Vty
import Pipes hiding (next)
import Pipes.Concurrent
import Pipes.Prelude as P
import System.Posix

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type

data App e s = App
    { initialize   :: forall m. MonadIO m => Vty -> VgrepT m s
    , liftEvent    :: Vty.Event -> e
    , handleEvent  :: EventHandler e s
    , render       :: forall m. Monad m => s -> VgrepT m Vty.Picture }


runApp_ :: App e s -> Config -> Producer e IO () -> IO ()
runApp_ app conf externalEvents = void (runApp app conf externalEvents)

runApp :: App e s -> Config -> Producer e IO () -> IO s
runApp app conf externalEvents = withSpawn unbounded $ \(evSink, evSource) -> do
    displayRegion <- displayRegionHack
    externalEventThread <- (async . runEffect) (externalEvents >-> toOutput evSink)
    (finalState, _) <- runVgrepT (appEventLoop app evSource evSink)
                                 (Env conf displayRegion)
    cancel externalEventThread
    pure finalState

displayRegionHack :: IO DisplayRegion
displayRegionHack = bracket initVty Vty.shutdown $ \vty ->
        Vty.displayBounds (Vty.outputIface vty)

appEventLoop :: App e s -> Input e -> Output e -> VgrepT IO s
appEventLoop app evSource evSink = do
    startEventLoop >>= suspendAndResume

  where
    startEventLoop = withVty vtyEventSink $ \vty -> do
        initialState <- initialize app vty
        refresh vty initialState
        runEffect $ (fromInput evSource >> pure Unchanged) >-> eventLoop vty initialState

    continueEventLoop currentState = withVty vtyEventSink $ \vty -> do
        refresh vty currentState
        runEffect $ (fromInput evSource >> pure Unchanged) >-> eventLoop vty currentState

    eventLoop vty currentState = do
        event <- await
        next <- lift (runEventHandler handleAppEvent event currentState)
        case next of
            Unchanged         -> eventLoop vty currentState
            Continue newState -> lift (refresh vty newState) >> eventLoop vty newState
            other             -> pure other

    suspendAndResume = \case
        Halt finalState      -> pure finalState
        Resume outsideAction -> outsideAction >>= continueEventLoop >>= suspendAndResume
        _other               -> cannotHappen_othersAreHandledInEventLoop

    refresh vty s = renderApp s >>= lift . Vty.update vty
    renderApp = render app
    vtyEventSink = P.map (liftEvent app) >-> toOutput evSink
    handleAppEvent = handleEvent app

    cannotHappen_othersAreHandledInEventLoop =
        error "Internal error: Unhandled Continuation"


withVty :: Consumer Vty.Event IO () -> (Vty -> VgrepT IO s) -> VgrepT IO s
withVty sink action = vgrepBracket before after (\(vty, _) -> action vty)
  where
    before = do
        vty <- initVty
        evThread <- (async . runEffect) $
            lift (Vty.nextEvent vty) >~ sink
        pure (vty, evThread)
    after (vty, evThread) = do
        cancel evThread
        Vty.shutdown vty


initVty :: IO Vty
initVty = do
    cfg <- Vty.standardIOConfig
    fdIn  <- ttyIn
    fdOut <- ttyOut
    Vty.mkVty (cfg { Vty.inputFd = Just fdIn , Vty.outputFd = Just fdOut })

ttyIn, ttyOut :: IO Fd
ttyIn  = openFd "/dev/tty" ReadOnly  Nothing defaultFileFlags
ttyOut = openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
