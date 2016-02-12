{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types      #-}
module Vgrep.App
    ( App(..)
    , runApp, runApp_
    , ttyIn
    , ttyOut
    ) where

import Control.Concurrent.Async
import Control.Monad.Reader
import Data.Text.Lazy
import Graphics.Vty (Vty, Config(..))
import qualified Graphics.Vty as Vty
import Pipes hiding (next)
import Pipes.Concurrent
import Pipes.Prelude as P
import System.Posix

import Vgrep.Environment
import Vgrep.Event
import Vgrep.Type

data App s = App { initialize   :: Vty -> VgrepT IO s
                 , handleEvent  :: EventHandler s
                 , render       :: s -> Vgrep Vty.Picture }


runApp_ :: App s -> Environment -> Producer Text IO () -> IO ()
runApp_ app env input = void (runApp app env input)

runApp :: App s -> Environment -> Producer Text IO () -> IO s
runApp app env input = withSpawn unbounded $ \(evSink, evSource) -> do
    inputThread <- (async . runEffect) (input >-> P.map ReceiveInput >-> toOutput evSink)
    finalState <- runVgrepT env (appEventLoop app evSource evSink)
    cancel inputThread
    pure finalState

appEventLoop :: App s -> Input Event -> Output Event -> VgrepT IO s
appEventLoop app evSource evSink = do
    startEventLoop >>= suspendAndResume

  where
    startEventLoop = withVty evSink $ \vty -> do
        initialState <- initialize app vty
        refresh vty initialState
        runEffect $ (fromInput evSource >> pure Unchanged) >-> eventLoop vty initialState

    continueEventLoop currentState = withVty evSink $ \vty -> do
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

    refresh vty = liftVgrep . fmap (Vty.update vty) . renderApp
    renderApp = render app
    handleAppEvent = handleEvent app

    cannotHappen_othersAreHandledInEventLoop =
        error "Internal error: Unhandled Continuation"


withVty :: Output Event -> (Vty -> VgrepT IO s) -> VgrepT IO s
withVty sink action = bracket before after (\(vty, _) -> action vty)
  where
    before = do
        vty <- initVty
        evThread <- (async . runEffect) $
            lift (Vty.nextEvent vty) >~ P.map VtyEvent >-> toOutput sink
        pure (vty, evThread)
    after (vty, evThread) = do
        cancel evThread
        Vty.shutdown vty


initVty :: IO Vty
initVty = do
    cfg <- Vty.standardIOConfig
    fdIn  <- ttyIn
    fdOut <- ttyOut
    Vty.mkVty (cfg { inputFd = Just fdIn , outputFd = Just fdOut })

ttyIn, ttyOut :: IO Fd
ttyIn  = openFd "/dev/tty" ReadOnly  Nothing defaultFileFlags
ttyOut = openFd "/dev/tty" WriteOnly Nothing defaultFileFlags
