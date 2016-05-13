-- | The 'VgrepT' monad transformer allows reading from the 'Environment'
-- and changing the state of the 'Vgrep.App.App' or a 'Vgrep.Widget.Widget'.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Vgrep.Type
  ( -- * The 'VgrepT' monad transformer
    VgrepT ()
  , Vgrep

  , mkVgrepT
  , runVgrepT

  -- ** Modifying the environment
  , modifyEnvironment

  -- ** Utilities
  , vgrepBracket

  -- * Re-exports
  , lift
  , hoist
  , module Vgrep.Environment
) where

import qualified Control.Exception as E
import Control.Lens.Internal.Zoom
import Control.Lens.Zoom
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Extended

import Vgrep.Environment

-- | The 'VgrepT' monad transformer is parameterized over the state @s@ of
-- a 'Vgrep.Widget.Widget' or an 'Vgepr.App.App'.
newtype VgrepT s m a = VgrepT (StateT s (StateT Environment m) a)
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadIO )

-- | 'VgrepT' can read from the 'Environment'. Modifications to the
-- enviromnent are only possible globally (see 'modifyEnvironment'), the
-- 'local' environment is pure.
instance Monad m => MonadReader Environment (VgrepT s m) where
    ask = VgrepT (lift get)
    local f action = mkVgrepT $ \s env -> runVgrepT action s (f env)

instance Monad m => MonadState s (VgrepT s m) where
    get = VgrepT get
    put = VgrepT . put

instance MonadTrans (VgrepT s) where
    lift = VgrepT . lift . lift

instance MFunctor (VgrepT s) where
    hoist f (VgrepT action) = VgrepT (hoist (hoist f) action)

type instance Zoomed (VgrepT s m) = Focusing (StateT Environment m)

instance Monad m => Zoom (VgrepT s m) (VgrepT t m) s t where
    zoom l (VgrepT m) = VgrepT (zoom l m)

-- | Lift a monadic action to 'VgrepT'.
mkVgrepT
    :: Monad m
    => (s -> Environment -> m (a, s))
    -> VgrepT s m a
mkVgrepT action =
    let action' s env = fmap (, env) (action s env)
    in  VgrepT (StateT (StateT . action'))

-- | Pass an initial state and an 'Environment' and reduce a 'VgrepT'
-- action to an action in the base monad.
runVgrepT
    :: Monad m
    => VgrepT s m a
    -> s
    -> Environment
    -> m (a, s)
runVgrepT (VgrepT action) s env = do
    ((a, s'), _env') <- runStateT (runStateT action s) env
    pure (a, s')

type Vgrep s = VgrepT s Identity


-- | A version of 'E.bracket' where the action is lifted to 'VgrepT'.
vgrepBracket
    :: IO a
    -> (a -> IO c)
    -> (a -> VgrepT s IO b)
    -> VgrepT s IO b
vgrepBracket before after action = mkVgrepT $ \s env ->
    let baseAction a = runVgrepT (action a) s env
    in  E.bracket before after baseAction


-- | The 'Environment' of 'VgrepT' is not stateful, however it can be
-- modified globally. An example is resizing the application by changing
-- the display bounds.
modifyEnvironment :: Monad m => (Environment -> Environment) -> VgrepT s m ()
modifyEnvironment = VgrepT . lift . modify
