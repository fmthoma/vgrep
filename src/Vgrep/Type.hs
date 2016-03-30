{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Extended

import Vgrep.Environment

newtype VgrepT m a = VgrepT (StateT Environment m a)
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadTrans
                         , MFunctor
                         , MonadIO )

instance Monad m => MonadReader Environment (VgrepT m) where
    ask = VgrepT get
    local f action = mkVgrepT $ \env -> runVgrepT action (f env)

mkVgrepT :: Monad m
         => (Environment -> m (a, Environment))
         -> VgrepT m a
mkVgrepT = VgrepT . StateT

runVgrepT :: Monad m
          => VgrepT m a
          -> Environment -> m (a, Environment)
runVgrepT (VgrepT action) = runStateT action

type Vgrep = VgrepT Identity

vgrepBracket :: IO a
             -> (a -> IO c)
             -> (a -> VgrepT IO b)
             -> VgrepT IO b
vgrepBracket before after action = mkVgrepT $ \env ->
    let baseAction a = runVgrepT (action a) env
    in  E.bracket before after baseAction


modifyEnvironment :: Monad m => (Environment -> Environment) -> VgrepT m ()
modifyEnvironment = VgrepT . modify
