{-# LANGUAGE DeriveFunctor #-}
module Vgrep.Event ( EventHandler ()
  {- CR/quchen -}  , mkEventHandler
  {- There is  -}  , mkEventHandlerIO
  {-  so much  -}
  {-   wasted  -}  , Next(..)
  {-   space   -}
  {-  here, I  -}  , handle
  {- can write -}  , handleKey
  {-   a full  -}  , handleKeyIO
  {-  sentence -}  , handleResize
  {-  into the -}  , exitOn
  {-    gaps   -}  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.State.Lift
import Data.Monoid
import Graphics.Vty as Vty

newtype EventHandler s = EventHandler
                         { handle :: Event -> s -> Next (IO s) }

mkEventHandler :: (Event -> s -> Next s) -> EventHandler s
mkEventHandler = EventHandler . fmap (fmap (fmap pure))
-- CR/quchen: f(f(f(x))) = (f.f.f) x
-- But since that's still unreadable, why not not use Reader,
-- mkEventHandler f = EventHandler (\_e _s -> pure f)

mkEventHandlerIO :: (Event -> s -> Next (IO s)) -> EventHandler s
mkEventHandlerIO = EventHandler

instance Monoid (EventHandler s) where
    mempty = EventHandler $ \_ _ -> Unchanged
    h1 `mappend` h2 = EventHandler $ \state event ->
        handle h1 state event <> handle h2 state event
        -- CR/quchen: Function monoid! :-)
        --            (f <> g) x = f x <> g x

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
    EvKey k ms | k == key && ms == modifiers
               -- CR/quchen: Sometimes I find comparing tuples more readable
               --            if multiple (==) are involved,
               --            (k,ms) == (key, modifiers)
        -> (Continue . execStateT action) state -- CR/quchen: (f . g) x = f (g x)
    _   -> Unchanged
    -- CR/quchen: name the fallthrough patterns

handleResize :: (DisplayRegion -> State s ()) -> EventHandler s
handleResize action = mkEventHandler $ \event state -> case event of
    EvResize w h -> (Continue . execState (action (w, h))) state -- CR/quchen: (f . g) x = f (g x)
    _            -> Unchanged
    -- CR/quchen: name the fallthrough patterns

exitOn :: Key -> [Modifier] -> EventHandler s
exitOn key modifiers = mkEventHandler $ \event state -> case event of
    EvKey k ms | k == key && ms == modifiers -> Halt state
    _                                        -> Unchanged
    -- CR/quchen: name the fallthrough patterns
