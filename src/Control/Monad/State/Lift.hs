module Control.Monad.State.Lift where

import Control.Monad.Morph
import Control.Monad.State
import Control.Monad.State.Class

liftState :: Monad m => State s a -> StateT s m a
liftState = hoist generalize
