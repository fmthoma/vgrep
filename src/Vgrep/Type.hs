{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vgrep.Type where

import qualified Control.Exception as E
import Control.Monad.Reader

import Vgrep.Environment

newtype Vgrep a = Vgrep (ReaderT Environment IO a)
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadReader Environment
                         , MonadIO )

runVgrep :: Environment -> Vgrep a -> IO a
runVgrep env (Vgrep action) = runReaderT action env

bracket :: IO a
        -> (a -> IO c)
        -> (a -> Vgrep b)
        -> Vgrep b
bracket before after action = Vgrep . ReaderT $ \env ->
    let baseAction a = runVgrep env (action a)
    in  E.bracket before after baseAction
