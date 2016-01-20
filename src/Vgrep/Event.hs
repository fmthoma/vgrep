{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Event
    ( EventHandler ()
    , mkEventHandler
    , mkEventHandlerIO

    , Next(..)

    , handle
    , handleKey
    , handleKeyIO
    , handleKeySuspend
    , handleResize
    , exitOn
    ) where

import Control.Applicative
import Control.Monad.State.Extended ( State, StateT
                                    , execState, execStateT
                                    , liftState )
import Graphics.Vty as Vty

newtype EventHandler s = EventHandler
                         { handle :: Event -> s -> IO (Next s) }

mkEventHandler :: (Event -> s -> Next s) -> EventHandler s
mkEventHandler f = EventHandler $ \e s -> pure (f e s)

mkEventHandlerIO :: (Event -> s -> IO (Next s)) -> EventHandler s
mkEventHandlerIO = EventHandler

instance Monoid (EventHandler s) where
    mempty = EventHandler $ \_ _ -> pure Unchanged
    h1 `mappend` h2 = EventHandler $ \ev s ->
        liftA2 mappend (handle h1 ev s) (handle h2 ev s)

data Next s = Continue s
            | Resume (IO s)
            | Halt s
            | Unchanged
            deriving (Functor)

instance Monoid (Next s) where
    mempty = Unchanged
    Unchanged `mappend` next = next
    next      `mappend` _    = next


handleKeyIO :: Key -> [Modifier] -> StateT s IO () -> EventHandler s
handleKeyIO key modifiers action =
    mkEventHandlerIO (_handleKey key modifiers action)

handleKey :: Key -> [Modifier] -> State s () -> EventHandler s
handleKey key modifiers action =
    mkEventHandlerIO (_handleKey key modifiers (liftState action))

_handleKey :: Key -> [Modifier] -> StateT s IO () -> Event -> s -> IO (Next s)
_handleKey key modifiers action event state = case event of
    EvKey k ms | (k, ms) == (key, modifiers)
                -> (fmap Continue . execStateT action) state
    _otherwise  -> pure Unchanged

handleKeySuspend :: Key -> [Modifier] -> StateT s IO () -> EventHandler s
handleKeySuspend key modifiers action =
    mkEventHandlerIO (_handleKeySuspend key modifiers action)

_handleKeySuspend :: Key -> [Modifier] -> StateT s IO () -> Event -> s -> IO (Next s)
_handleKeySuspend key modifiers action event state = case event of
    EvKey k ms | (k, ms) == (key, modifiers)
                -> (pure . Resume . execStateT action) state
    _otherwise  -> pure Unchanged

handleResize :: (DisplayRegion -> State s ()) -> EventHandler s
handleResize action = mkEventHandler $ \event state -> case event of
    EvResize w h -> Continue (execState (action (w, h)) state)
    _otherwise   -> Unchanged

exitOn :: Key -> [Modifier] -> EventHandler s
exitOn key modifiers = mkEventHandler $ \event state -> case event of
    EvKey k ms | k == key && ms == modifiers -> Halt state
    _otherwise                               -> Unchanged
