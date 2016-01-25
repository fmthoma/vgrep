{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vgrep.Type where

import qualified Control.Exception as E
import Control.Monad.Identity
import Control.Monad.Reader

import Vgrep.Environment

newtype VgrepT m a = VgrepT (ReaderT Environment m a)
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadReader Environment
                         , MonadIO )

mkVgrepT :: Monad m => (Environment -> m a) -> VgrepT m a
mkVgrepT action = VgrepT (ReaderT action)

runVgrepT :: Monad m => Environment -> VgrepT m a -> m a
runVgrepT env (VgrepT action) = runReaderT action env

type Vgrep = VgrepT Identity

mkVgrep :: (Environment -> a) -> Vgrep a
mkVgrep = mkVgrepT . fmap Identity

runVgrep :: Environment -> Vgrep a -> a
runVgrep env = runIdentity . runVgrepT env

bracket :: IO a
        -> (a -> IO c)
        -> (a -> VgrepT IO b)
        -> VgrepT IO b
bracket before after action = mkVgrepT $ \env ->
    let baseAction a = runVgrepT env (action a)
    in  E.bracket before after baseAction
