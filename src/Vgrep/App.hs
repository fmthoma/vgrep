{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Vgrep.App
    ( App(..)
    , runApp, runApp_

    -- * Auxiliary definitions
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


-- | The 'App' type is parameterized over the type 'e' of events it handles
-- and the type 's' of its state.
data App e s = App
    { initialize  :: forall m. MonadIO m => m s
    -- ^ Creates the initial state for the app.

    , liftEvent   :: Vty.Event -> e
    -- ^ How to convert an external 'Vty.Event' to the App's event

    , handleEvent :: forall m. MonadIO m => e -> s -> Next (VgrepT s m Redraw)
    -- ^ Handles an event, possibly modifying the App's state.
    --
    -- @
    -- handleEvent e s = case e of
    --     'Vty.EvKey' 'Vty.KEnter' [] -> 'Continue' ('pure' 'Unchanged')
    --     -- Handles the @Enter@ key, but does nothing.
    --
    --     'Vty.EvKey' 'Vty.KUp' [] -> 'Continue' ('pure' 'Redraw')
    --     -- Handles the @Up@ key and triggers a redraw.
    --
    --     _otherwise          -> 'Skip'
    --     -- Does not handle the event, so other handlers may be invoked.
    -- @

    , render      :: forall m. Monad m => VgrepT s m Vty.Picture
    -- ^ Creates a 'Vty.Picture' to be displayed. May modify the App's
    -- state (e. g. for resizing).
    }


-- | Like 'runApp', but does not return the final state.
runApp_ :: App e s -> Config -> Producer e IO () -> IO ()
runApp_ app conf externalEvents = void (runApp app conf externalEvents)

-- | Runs runs the event loop until an @'Interrupt' 'Halt'@ is encountered.
-- Events handled with @'Interrupt' ('Suspend' action)@ will shut down
-- 'Vty.Vty', run the action (e. g. invoking an external editor), and start
-- 'Vty.Vty' again.
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

-- | We need the display region in order to initialize the app, which in
-- turn will start 'Vty.Vty'. To resolve this circular dependency, we start
-- once 'Vty.Vty' in order to determine the display region, and shut it
-- down again immediately.
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
-- | Opens @/dev/tty@ read-only. Should be connected to the @stdin@ of
-- a GUI process (e. g. 'Vty.Vty').
ttyIn  = openFd "/dev/tty" ReadOnly  Nothing defaultFileFlags
-- | Opens @/dev/tty@ write-only. Should be connected to the @stdout@ of
-- a GUI process (e. g. 'Vty.Vty').
ttyOut = openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
