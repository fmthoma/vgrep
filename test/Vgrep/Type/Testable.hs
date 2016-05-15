{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Vgrep.Type.Testable
    ( monadicVgrep
    , module Vgrep.Type
    ) where

import Data.Functor.Identity
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Vgrep.Environment.Testable
import Vgrep.Type

instance Monad m => MonadState s (PropertyM (VgrepT s m)) where
    get = run get
    put = run . put

runVgrepForTest
    :: Vgrep s a
    -> (s, Environment)
    -> a
runVgrepForTest action (s, env) = fst (runIdentity (runVgrepT action s env))

monadicVgrep
    :: Arbitrary s
    => PropertyM (Vgrep s) a
    -> Gen Property
monadicVgrep testcase = do
    initialState <- arbitrary
    initialEnv   <- arbitrary
    pure (monadic (`runVgrepForTest` (initialState, initialEnv)) testcase)
