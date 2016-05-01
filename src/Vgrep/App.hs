{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Vgrep.App
    ( App(..)
    , runApp, runApp_
    , ttyIn
    , ttyOut
    ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State (get)
import Graphics.Vty (Vty)
import qualified Graphics.Vty as Vty
import Pipes hiding (next)
import Pipes.Concurrent
import Pipes.Prelude as P
import System.Posix.IO
import System.Posix.Types (Fd)

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type

data App e s = App
    { initialize   :: forall m. MonadIO m => m s
    , liftEvent    :: Vty.Event -> e
    , handleEvent  :: forall m. MonadIO m => e -> s -> Next (VgrepT s m Redraw)
    , render       :: forall m. Monad m => VgrepT s m Vty.Picture }


runApp_ :: App e s -> Config -> Producer e IO () -> IO ()
runApp_ app conf externalEvents = void (runApp app conf externalEvents)

runApp :: App e s -> Config -> Producer e IO () -> IO s
runApp app conf externalEvents = withSpawn unbounded $ \(evSink, evSource) -> do
    displayRegion <- displayRegionHack
    externalEventThread <- (async . runEffect) (externalEvents >-> toOutput evSink)
    initialState <- initialize app
    (_, finalState) <- runVgrepT (appEventLoop app evSource evSink)
                                 initialState
                                 (Env conf displayRegion)
    cancel externalEventThread
    pure finalState

displayRegionHack :: IO DisplayRegion
displayRegionHack = bracket initVty Vty.shutdown $ \vty ->
        Vty.displayBounds (Vty.outputIface vty)

appEventLoop :: forall e s. App e s -> Input e -> Output e -> VgrepT s IO ()
appEventLoop app evSource evSink = startEventLoop >>= suspendAndResume

  where
    startEventLoop :: VgrepT s IO Interrupt
    startEventLoop = withVty vtyEventSink $ \vty -> do
        refresh vty
        runEffect ((fromInput evSource >> pure Halt) >-> eventLoop vty)

    continueEventLoop :: VgrepT s IO Interrupt
    continueEventLoop = withVty vtyEventSink $ \vty -> do
        refresh vty
        runEffect ((fromInput evSource >> pure Halt) >-> eventLoop vty)

    eventLoop :: Vty -> Consumer e (VgrepT s IO) Interrupt
    eventLoop vty = do
        event <- await
        currentState <- get
        case handleAppEvent event currentState of
            Skip            -> eventLoop vty
            Continue action -> lift action >>= \case
                Unchanged   -> eventLoop vty
                Redraw      -> lift (refresh vty) >> eventLoop vty
            Interrupt int   -> pure int

    suspendAndResume :: Interrupt -> VgrepT s IO ()
    suspendAndResume = \case
        Halt                  -> pure ()
        Suspend outsideAction -> do env <- ask
                                    outsideAction env
                                    continueEventLoop >>= suspendAndResume

    refresh :: Vty -> VgrepT s IO ()
    refresh vty = render app >>= lift . Vty.update vty
    vtyEventSink = P.map (liftEvent app) >-> toOutput evSink
    handleAppEvent = handleEvent app


withVty :: Consumer Vty.Event IO () -> (Vty -> VgrepT s IO a) -> VgrepT s IO a
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
