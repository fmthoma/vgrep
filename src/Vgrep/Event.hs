{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Event
    ( EventHandler ()
    , runEventHandler
    , mkEventHandler
    , mkEventHandlerIO

    , Next(..)

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
import Graphics.Vty as Vty

import Vgrep.Type

newtype EventHandler s = EventHandler
                         { runEventHandler :: Event -> s -> Vgrep (Next s) }

mkEventHandler :: (Event -> s -> Next s) -> EventHandler s
mkEventHandler f = EventHandler $ \e s -> pure (f e s)

mkEventHandlerIO :: (Event -> s -> Vgrep (Next s)) -> EventHandler s
mkEventHandlerIO = EventHandler

instance Monoid (EventHandler s) where
    mempty = EventHandler $ \_ _ -> pure Unchanged
    h1 `mappend` h2 = EventHandler $ \ev s ->
        liftA2 mappend (runEventHandler h1 ev s) (runEventHandler h2 ev s)

data Next s = Continue s
            | Resume (Vgrep s)
            | Halt s
            | Unchanged
            deriving (Functor)

instance Monoid (Next s) where
    mempty = Unchanged
    Unchanged `mappend` next = next
    next      `mappend` _    = next


handle :: (Event -> Bool) -> (s -> Vgrep (Next s)) -> EventHandler s
handle doesMatch action = mkEventHandlerIO $ \event state ->
    if doesMatch event then action state else pure Unchanged

handleResize :: (DisplayRegion -> State s ()) -> EventHandler s
handleResize action = mkEventHandler $ \event state -> case event of
    EvResize w h -> Continue (execState (action (w, h)) state)
    _otherwise   -> Unchanged


keyEvent :: Key -> [Modifier] -> Event -> Bool
keyEvent k ms = \case
    EvKey k' ms' | (k', ms') == (k, ms) -> True
    _otherwise                          -> False

keyCharEvent :: Char -> [Modifier] -> Event -> Bool
keyCharEvent c = keyEvent (KChar c)


continueIO :: StateT s Vgrep () -> s -> Vgrep (Next s)
continueIO action state = (fmap Continue . execStateT action) state

continue :: State s () -> s -> Vgrep (Next s)
continue action = continueIO (liftState action)

suspend :: StateT s Vgrep () -> s -> Vgrep (Next s)
suspend action = pure . Resume . execStateT action

halt :: s -> Vgrep (Next s)
halt state = pure (Halt state)
