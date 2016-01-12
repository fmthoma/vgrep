-- CR/quchen: Related blogpost about adding stuff to third-party modules:
--            https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html

module Control.Monad.State.Lift where

import Control.Monad.Morph
import Control.Monad.State
import Control.Monad.State.Class

liftState :: Monad m => State s a -> StateT s m a
liftState = hoist generalize
