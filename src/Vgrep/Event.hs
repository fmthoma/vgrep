{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Event
    ( EventHandler ()
    , mkEventHandler
    , mkEventHandlerIO

    , Next(..)

    , handle
    , handleKey
    , handleKeyIO
    , handleResize
    , exitOn
    ) where

import Control.Monad.State.Extended ( State, StateT
                                    , execState, execStateT
                                    , liftState )
import Data.Monoid
import Graphics.Vty as Vty

newtype EventHandler s = EventHandler
                         { handle :: Event -> s -> Next (IO s) }

mkEventHandler :: (Event -> s -> Next s) -> EventHandler s
mkEventHandler f = EventHandler $ \e s -> fmap pure (f e s)

mkEventHandlerIO :: (Event -> s -> Next (IO s)) -> EventHandler s
mkEventHandlerIO = EventHandler

instance Monoid (EventHandler s) where
    mempty = EventHandler $ \_ _ -> Unchanged
    h1 `mappend` h2 = EventHandler (handle h1 <> handle h2)

data Next s = Continue s
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

_handleKey :: Key -> [Modifier] -> StateT s IO () -> Event -> s -> Next (IO s)
_handleKey key modifiers action event state = case event of
    EvKey k ms | (k, ms) == (key, modifiers)
                -> (Continue . execStateT action) state
    _otherwise  -> Unchanged

handleResize :: (DisplayRegion -> State s ()) -> EventHandler s
handleResize action = mkEventHandler $ \event state -> case event of
    EvResize w h -> Continue (execState (action (w, h)) state)
    _otherwise   -> Unchanged

exitOn :: Key -> [Modifier] -> EventHandler s
exitOn key modifiers = mkEventHandler $ \event state -> case event of
    EvKey k ms | k == key && ms == modifiers -> Halt state
    _otherwise                               -> Unchanged
