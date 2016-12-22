{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Vgrep.App
    ( App(..)
    , runApp, runApp_
    ) where

import           Control.Concurrent.Async
import           Graphics.Vty             (Vty)
import qualified Graphics.Vty             as Vty
import           Pipes                    hiding (next)
import           Pipes.Concurrent.PQueue
import           Pipes.Prelude            as P

import Vgrep.App.Internal
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
runApp app conf externalEvents = withSpawn $ \(evSink, evSource) -> do
    initialViewport <- viewportHack
    let userEventSink   = contramap (User,)   evSink
        systemEventSink = contramap (System,) evSink
    externalEventThread <- (async . runEffect) (externalEvents >-> toOutput systemEventSink)
    initialState <- initialize app
    (_, finalState) <- runVgrepT (appEventLoop app evSource userEventSink)
                                 initialState
                                 (Env conf initialViewport)
    cancel externalEventThread
    pure finalState

-- | Monomorphic version of 'Data.Functor.Contravariant.contramap', to
-- avoid having to update pipes-concurrency.
contramap :: (b -> a) -> Output a -> Output b
contramap f (Output a) = Output (a . f)


appEventLoop :: forall e s. App e s -> Input e -> Output e -> VgrepT s IO ()
appEventLoop app evSource evSink = eventLoop

  where
    eventLoop :: VgrepT s IO ()
    eventLoop = startEventLoop >>= suspendAndResume

    startEventLoop :: VgrepT s IO Interrupt
    startEventLoop = withVgrepVty $ \vty -> withEvThread vtyEventSink vty $ do
        refresh vty
        runEffect ((fromInput evSource >> pure Halt) >-> eventHandler vty)

    suspendAndResume :: Interrupt -> VgrepT s IO ()
    suspendAndResume = \case
        Halt                  -> pure ()
        Suspend outsideAction -> do env <- ask
                                    outsideAction env
                                    eventLoop

    eventHandler :: Vty -> Consumer e (VgrepT s IO) Interrupt
    eventHandler vty = go
      where
        go = do
            event <- await
            currentState <- get
            case handleAppEvent event currentState of
                Skip            -> go
                Continue action -> lift action >>= \case
                    Unchanged   -> go
                    Redraw      -> lift (refresh vty) >> go
                Interrupt int   -> pure int

    refresh :: Vty -> VgrepT s IO ()
    refresh vty = render app >>= lift . Vty.update vty
    vtyEventSink = P.map (liftEvent app) >-> toOutput evSink
    handleAppEvent = handleEvent app
