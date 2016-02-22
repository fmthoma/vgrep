{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Event
    ( EventHandler ()
    , runEventHandler
    , mkEventHandler
    , mkEventHandlerIO
    , liftEventHandler

    , Next (..)

    , handle
    , handleResize

    , keyEvent
    , keyCharEvent
    , continueIO
    , continue
    , suspend
    , halt
    ) where

import Control.Applicative
import Control.Monad.State.Extended ( State, StateT
                                    , execState, execStateT
                                    , liftState )
import qualified Graphics.Vty as Vty
import Graphics.Vty.Prelude

import Vgrep.Type


newtype EventHandler e s = EventHandler
    { runEventHandler :: e -> s -> VgrepT IO (Next s) }

mkEventHandler :: (e -> s -> Next s) -> EventHandler e s
mkEventHandler f = EventHandler $ \e s -> pure (f e s)

mkEventHandlerIO :: (e -> s -> VgrepT IO (Next s)) -> EventHandler e s
mkEventHandlerIO = EventHandler

liftEventHandler :: (e -> Maybe e') -> EventHandler e' s -> EventHandler e s
liftEventHandler f (EventHandler h) = EventHandler $ \e -> case f e of
    Just e' -> h e'
    Nothing -> const (pure Unchanged)


instance Monoid (EventHandler e s) where
    mempty = EventHandler $ \_ _ -> pure Unchanged
    h1 `mappend` h2 = EventHandler $ \ev s ->
        liftA2 mappend (runEventHandler h1 ev s) (runEventHandler h2 ev s)

data Next s = Continue s
            | Resume (VgrepT IO s)
            | Halt s
            | Unchanged
            deriving (Functor)

instance Monoid (Next s) where
    mempty = Unchanged
    Unchanged `mappend` next = next
    next      `mappend` _    = next


handle :: (e -> Bool) -> (s -> VgrepT IO (Next s)) -> EventHandler e s
handle doesMatch action = mkEventHandlerIO $ \event state ->
    if doesMatch event then action state else pure Unchanged

handleResize :: (DisplayRegion -> State s ()) -> EventHandler Vty.Event s
handleResize action = mkEventHandler $ \event state -> case event of
    Vty.EvResize w h -> Continue (execState (action (w, h)) state)
    _otherwise       -> Unchanged


keyEvent :: Vty.Key -> [Vty.Modifier] -> Vty.Event -> Bool
keyEvent k ms = \case
    Vty.EvKey k' ms' | (k', ms') == (k, ms) -> True
    _otherwise                              -> False

keyCharEvent :: Char -> [Vty.Modifier] -> Vty.Event -> Bool
keyCharEvent c = keyEvent (Vty.KChar c)


continueIO :: StateT s (VgrepT IO) () -> s -> VgrepT IO (Next s)
continueIO action state = (fmap Continue . execStateT action) state

continue :: State s () -> s -> VgrepT IO (Next s)
continue action = continueIO (liftState action)

suspend :: StateT s (VgrepT IO) () -> s -> VgrepT IO (Next s)
suspend action = pure . Resume . execStateT action

halt :: s -> VgrepT IO (Next s)
halt state = pure (Halt state)
