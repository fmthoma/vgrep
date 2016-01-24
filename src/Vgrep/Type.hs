{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vgrep.Type where

import qualified Control.Exception as E
import Control.Monad.Reader

import Vgrep.Environment

newtype Vgrep a = Vgrep { runVgrep :: ReaderT Environment IO a }
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadReader Environment
                         , MonadIO )

bracket :: IO a
        -> (a -> IO c)
        -> (a -> Vgrep b)
        -> Vgrep b
bracket before after action = Vgrep . ReaderT $ \r ->
    let baseAction a = (runReaderT (runVgrep (action a)) r)
    in  E.bracket before after baseAction
