{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
module Test.Case (
      TestCase (..)
    , runTestCase
    , runTestCases

    , (~~)

    , testPropertyVgrep
    , monadicVgrep

    , module Vgrep.Type
    , TestTree ()
    ) where

import Control.Lens.Compat
import Control.Monad
import Data.Functor.Identity
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import Vgrep.Environment.Testable
import Vgrep.Type

data TestCase
    = forall s a prop. (Arbitrary s, Show s, Testable prop)
    => TestProperty
        { description :: TestName
        , testData    :: Gen (s, Environment)
        , testCase    :: PropertyM (Vgrep s) a
        , assertion   :: a -> PropertyM (Vgrep s) prop }
    | forall s a r. (Arbitrary s, Show s, Eq r, Show r)
    => TestInvariant
        { description :: TestName
        , testData    :: Gen (s, Environment)
        , testCase    :: PropertyM (Vgrep s) a
        , invariant   :: Getter s r }


runTestCase :: TestCase -> TestTree
runTestCase = \case
    TestProperty {..} -> testProperty description $ do
        (initialState, initialEnv) <- testData
        pure . monadic (`runVgrepForTest` (initialState, initialEnv)) . void $ do
            monitor (counterexample (show initialState))
            monitor (counterexample (show initialEnv))
            params <- testCase
            stop =<< assertion params
    TestInvariant {..} -> testProperty description $ do
        (initialState, initialEnv) <- testData
        pure . monadic (`runVgrepForTest` (initialState, initialEnv)) . void $ do
            monitor (counterexample (show initialState))
            monitor (counterexample (show initialEnv))
            invariantBefore <- use invariant
            void testCase
            invariantAfter <- use invariant
            stop (invariantAfter === invariantBefore)


runTestCases :: TestName -> [TestCase] -> TestTree
runTestCases name cases = testGroup name (map runTestCase cases)


instance Monad m => MonadState s (PropertyM (VgrepT s m)) where
    get = run get
    put = run . put

instance Monad m => MonadReader Environment (PropertyM (VgrepT s m)) where
    ask = run ask
    local f action = MkPropertyM $ \k -> fmap (local f) (unPropertyM action k)


runVgrepForTest
    :: Vgrep s a
    -> (s, Environment)
    -> a
runVgrepForTest action (s, env) = fst (runIdentity (runVgrepT action s env))

monadicVgrep
    :: (Arbitrary s, Testable a)
    => PropertyM (Vgrep s) a
    -> Gen Property
monadicVgrep testcase = do
    initialState <- arbitrary
    initialEnv   <- arbitrary
    pure (monadic (`runVgrepForTest` (initialState, initialEnv)) (fmap property testcase))

testPropertyVgrep
    :: (Arbitrary s, Testable a)
    => TestName
    -> PropertyM (Vgrep s) a
    -> TestTree
testPropertyVgrep name prop = testProperty name (monadicVgrep prop)

infix 4 ~~
(~~)
    :: (Eq a, Show a)
    => Getter s a
    -> Getter s a
    -> PropertyM (Vgrep s) Property
prop1 ~~ prop2 = do
    p1 <- use prop1
    p2 <- use prop2
    pure (p1 === p2)
