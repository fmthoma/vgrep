module Control.Monad.State.Extended
    ( module Control.Monad.State
    , liftState
    ) where

import Control.Monad.State

liftState :: MonadState s m => State s a -> m a
liftState = state . runState
