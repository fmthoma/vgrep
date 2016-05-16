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
    ) where

import Control.Lens
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import Vgrep.Environment.Testable
import Vgrep.Type

data TestCase
    = forall s prop. (Arbitrary s, Testable prop)
    => TestProperty
        { description :: TestName
        , testData    :: Gen (s, Environment)
        , testCase    :: PropertyM (Vgrep s) ()
        , assertion   :: PropertyM (Vgrep s) prop }
    | forall s a. (Arbitrary s, Eq a, Show a)
    => TestInvariant
        { description :: TestName
        , testData    :: Gen (s, Environment)
        , testCase    :: PropertyM (Vgrep s) ()
        , invariant   :: Getter s a }


runTestCase :: TestCase -> TestTree
runTestCase = \case
    TestProperty {..} -> testProperty description $ do
        (initialState, initialEnv) <- testData
        pure . monadic (`runVgrepForTest` (initialState, initialEnv)) $ do
            testCase
            stop =<< assertion
    TestInvariant {..} -> testProperty description $ do
        (initialState, initialEnv) <- testData
        pure . monadic (`runVgrepForTest` (initialState, initialEnv)) $ do
            before <- use invariant
            testCase
            after <- use invariant
            stop (after === before)


runTestCases :: TestName -> [TestCase] -> TestTree
runTestCases name cases = testGroup name (map runTestCase cases)


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

testPropertyVgrep
    :: Arbitrary s
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
