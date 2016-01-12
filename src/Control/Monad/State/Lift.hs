module Control.Monad.State.Lift where

import Control.Monad.State

liftState :: MonadState s m => State s a -> m a
liftState = state . runState
