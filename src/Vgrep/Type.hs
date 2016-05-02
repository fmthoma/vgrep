{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Vgrep.Type
  ( VgrepT ()
  , Vgrep

  , mkVgrepT
  , runVgrepT
  , vgrepBracket

  , modifyEnvironment

  -- Re-exports
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

newtype VgrepT s m a = VgrepT (StateT s (StateT Environment m) a)
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadIO )

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

mkVgrepT
    :: Monad m
    => (s -> Environment -> m (a, s))
    -> VgrepT s m a
mkVgrepT action =
    let action' s env = fmap (, env) (action s env)
    in  VgrepT (StateT (StateT . action'))

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

vgrepBracket
    :: IO a
    -> (a -> IO c)
    -> (a -> VgrepT s IO b)
    -> VgrepT s IO b
vgrepBracket before after action = mkVgrepT $ \s env ->
    let baseAction a = runVgrepT (action a) s env
    in  E.bracket before after baseAction


modifyEnvironment :: Monad m => (Environment -> Environment) -> VgrepT s m ()
modifyEnvironment = VgrepT . lift . modify
