{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Event where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import Graphics.Vty as Vty

newtype EventHandler s = EventHandler { handle :: Event -> s -> Next s }

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

handleKey :: Key -> [Modifier] -> State s () -> EventHandler s
handleKey key modifiers action = EventHandler $ \event state -> case event of
    EvKey k ms | k == key && ms == modifiers -> (Continue . execState action) state
    _                                        -> Unchanged

handleResize :: (DisplayRegion -> State s ()) -> EventHandler s
handleResize action = EventHandler $ \event state -> case event of
    EvResize w h -> (Continue . execState (action (w, h))) state
    _            -> Unchanged

exitOn :: Key -> [Modifier] -> EventHandler s
exitOn key modifiers = EventHandler $ \event state -> case event of
    EvKey k ms | k == key && ms == modifiers -> Halt state
    _                                        -> Unchanged
