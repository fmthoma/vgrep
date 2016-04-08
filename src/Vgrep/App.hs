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
import Control.Monad.State (StateT (..), execStateT, get)
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
    { initialize   :: forall m. MonadIO m => VgrepT m s
    , liftEvent    :: Vty.Event -> e
    , handleEvent  :: forall m. MonadIO m => EventHandler e s m
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

appEventLoop :: forall e s. App e s -> Input e -> Output e -> VgrepT IO s
appEventLoop app evSource evSink =
    initialize app >>= execStateT (startEventLoop >>= suspendAndResume)

  where
    startEventLoop :: StateT s (VgrepT IO) Interrupt
    startEventLoop = smurf vtyEventSink $ \vty -> do
        refresh vty
        runEffect ((fromInput evSource >> pure Halt) >-> eventLoop vty)

    continueEventLoop :: StateT s (VgrepT IO) Interrupt
    continueEventLoop = smurf vtyEventSink $ \vty -> do
        refresh vty
        runEffect ((fromInput evSource >> pure Halt) >-> eventLoop vty)

    eventLoop :: Vty -> Consumer e (StateT s (VgrepT IO)) Interrupt
    eventLoop vty = do
        event <- await
        case runEventHandler handleAppEvent event of
            Skip                -> eventLoop vty
            Continue action     -> lift (action >> refresh vty) >> eventLoop vty
            Interrupt interrupt -> pure interrupt

    suspendAndResume :: Interrupt -> StateT s (VgrepT IO) ()
    suspendAndResume = \case
        Halt                  -> pure ()
        Suspend outsideAction -> do liftIO outsideAction
                                    continueEventLoop >>= suspendAndResume

    refresh :: Vty -> StateT s (VgrepT IO) ()
    refresh vty = get >>= lift . render app >>= lift . lift . Vty.update vty
    vtyEventSink = P.map (liftEvent app) >-> toOutput evSink
    handleAppEvent = handleEvent app


smurf :: Consumer Vty.Event IO ()
      -> (Vty -> StateT s (VgrepT IO) a)
      -> StateT s (VgrepT IO) a

smurf sink action = StateT $ \s ->
    withVty sink $ \vty -> runStateT (action vty) s

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
