{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Vgrep.App
    ( App(..)
    , runApp, runApp_
    ) where

import           Control.Concurrent.Async
import           Control.Exception
import           Graphics.Vty             (Vty)
import qualified Graphics.Vty             as Vty
import           Pipes                    hiding (next)
import           Pipes.Concurrent.PQueue
import           Pipes.Prelude            as P
import           System.Posix.IO
import           System.Posix.Types       (Fd)

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
    displayRegion <- displayRegionHack
    let userEventSink   = contramap (User,)   evSink
        systemEventSink = contramap (System,) evSink
    externalEventThread <- (async . runEffect) (externalEvents >-> toOutput systemEventSink)
    initialState <- initialize app
    (_, finalState) <- runVgrepT (appEventLoop app evSource userEventSink)
                                 initialState
                                 (Env conf displayRegion)
    cancel externalEventThread
    pure finalState

-- | Monomorphic version of 'Data.Functor.Contravariant.contramap', to
-- avoid having to update pipes-concurrency.
contramap :: (b -> a) -> Output a -> Output b
contramap f (Output a) = Output (a . f)

-- | We need the display region in order to initialize the app, which in
-- turn will start 'Vty.Vty'. To resolve this circular dependency, we start
-- once 'Vty.Vty' in order to determine the display region, and shut it
-- down again immediately.
displayRegionHack :: IO DisplayRegion
displayRegionHack = withVty (Vty.displayBounds . Vty.outputIface)

appEventLoop :: forall e s. App e s -> Input e -> Output e -> VgrepT s IO ()
appEventLoop app evSource evSink = startEventLoop >>= suspendAndResume

  where
    startEventLoop :: VgrepT s IO Interrupt
    startEventLoop = withVgrepVty $ \vty -> withEvThread vtyEventSink vty $ do
        refresh vty
        runEffect ((fromInput evSource >> pure Halt) >-> eventLoop vty)

    eventLoop :: Vty -> Consumer e (VgrepT s IO) Interrupt
    eventLoop vty = go
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

    suspendAndResume :: Interrupt -> VgrepT s IO ()
    suspendAndResume = \case
        Halt                  -> pure ()
        Suspend outsideAction -> do env <- ask
                                    outsideAction env
                                    startEventLoop >>= suspendAndResume

    refresh :: Vty -> VgrepT s IO ()
    refresh vty = render app >>= lift . Vty.update vty
    vtyEventSink = P.map (liftEvent app) >-> toOutput evSink
    handleAppEvent = handleEvent app


-- | 'User' events do have higher priority than 'System' events, so that
-- the application stays responsive even in case of event queue congestion.
data EventPriority = User | System deriving (Eq, Ord, Enum)


-- | Spawns a thread parallel to the action that listens to 'Vty' events and
-- redirects them to the 'Consumer'.
withEvThread :: Consumer Vty.Event IO () -> Vty -> VgrepT s IO a -> VgrepT s IO a
withEvThread sink vty =
    vgrepBracket createEvThread cancel . const
  where
    createEvThread = (async . runEffect) $ lift (Vty.nextEvent vty) >~ sink


-- | Passes a 'Vty' instance to the action and shuts it down properly after the
-- action finishes. The 'Vty.inputFd' and 'Vty.outputFd' handles are connected
-- to @/dev/tty@ (see 'tty').
withVty :: (Vty -> IO a) -> IO a
-- | Like 'withVty', but lifted to @'VgrepT' s 'IO'@.
withVgrepVty :: (Vty -> VgrepT s IO a) -> VgrepT s IO a
(withVty, withVgrepVty) =
    let initVty fd = do
            cfg <- Vty.standardIOConfig
            Vty.mkVty cfg { Vty.inputFd  = Just fd
                          , Vty.outputFd = Just fd }
    in  ( \action -> withTty      $ \fd -> bracket      (initVty fd) Vty.shutdown action
        , \action -> withVgrepTty $ \fd -> vgrepBracket (initVty fd) Vty.shutdown action)


-- | Passes two file descriptors for read and write access to @/dev/tty@ to the
-- action. After the action has finished, the file descriptors will be closed
-- again.
withTty :: (Fd -> IO a) -> IO a
-- | Like 'withTty', but lifted to @'VgrepT' s 'IO'@.
withVgrepTty :: (Fd -> VgrepT s IO a) -> VgrepT s IO a
(withTty, withVgrepTty) = (bracket before after, vgrepBracket before after)
  where
    before = tty
    after fd = closeFd fd `catch` ignoreIOException
    ignoreIOException :: IOException -> IO ()
    ignoreIOException _ = pure ()

-- | Opens @/dev/tty@ in Read/Write mode. Should be connected to the @stdin@ and
-- @stdout@ of a GUI process (e. g. 'Vty.Vty').
tty :: IO Fd
tty = openFd "/dev/tty" ReadWrite Nothing defaultFileFlags
