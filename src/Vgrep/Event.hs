{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Event ( EventHandler ()
                   , mkEventHandler
                   , mkEventHandlerIO

                   , Next(..)

                   , handle
                   , handleKey
                   , handleKeyIO
                   , handleResize
                   , exitOn
                   ) where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Graphics.Vty as Vty

newtype EventHandler s = EventHandler
                         { handle :: Event -> s -> Next (IO s) }

mkEventHandler :: (Event -> s -> Next s) -> EventHandler s
mkEventHandler = EventHandler . fmap (fmap (fmap pure))

mkEventHandlerIO :: (Event -> s -> Next (IO s)) -> EventHandler s
mkEventHandlerIO = EventHandler

instance Monoid (EventHandler s) where
    mempty = EventHandler $ \_ _ -> Unchanged
    h1 `mappend` h2 = EventHandler $ \state event ->
        handle h1 state event <> handle h2 state event

data Next s = Continue s
            | Halt s
            | Unchanged
            deriving (Functor)

instance Monoid (Next s) where
    mempty = Unchanged
    Unchanged `mappend` next = next
    next      `mappend` _    = next


handleKeyIO :: Key -> [Modifier] -> State s (IO ()) -> EventHandler s
handleKeyIO key modifiers action =
    mkEventHandlerIO (_handleKey key modifiers action)

handleKey :: Key -> [Modifier] -> State s () -> EventHandler s
handleKey key modifiers action =
    mkEventHandlerIO (_handleKey key modifiers (fmap pure action))

_handleKey :: Key -> [Modifier] -> State s (IO ()) -> Event -> s -> Next (IO s)
_handleKey key modifiers action event state = case event of
    EvKey k ms | k == key && ms == modifiers
        -> (Continue . pure . execState action) state
    _   -> Unchanged

handleResize :: (DisplayRegion -> State s ()) -> EventHandler s
handleResize action = mkEventHandler $ \event state -> case event of
    EvResize w h -> (Continue . execState (action (w, h))) state
    _            -> Unchanged

exitOn :: Key -> [Modifier] -> EventHandler s
exitOn key modifiers = mkEventHandler $ \event state -> case event of
    EvKey k ms | k == key && ms == modifiers -> Halt state
    _                                        -> Unchanged
