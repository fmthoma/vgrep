module Control.Monad.State.Extended
    ( module Control.Monad.State.Strict
    , liftState
    , whenS
    , unlessS
    ) where

import Control.Monad.State.Strict

liftState :: MonadState s m => State s a -> m a
liftState = state . runState

whenS :: MonadState s m => (s -> Bool) -> m () -> m ()
whenS predicate action = do
    condition <- fmap predicate get
    when condition action

unlessS :: MonadState s m => (s -> Bool) -> m () -> m ()
unlessS predicate = whenS (not . predicate)
