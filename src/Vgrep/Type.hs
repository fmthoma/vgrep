{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vgrep.Type where

import Control.Monad.Reader

import Vgrep.Environment

newtype Vgrep a = Vgrep { runVgrep :: ReaderT Environment IO a }
                deriving ( Functor
                         , Applicative
                         , Monad
                         , MonadReader Environment
                         , MonadIO )
