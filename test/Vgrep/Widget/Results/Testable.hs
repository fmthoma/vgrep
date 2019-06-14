{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
module Vgrep.Widget.Results.Testable
    ( module Vgrep.Widget.Results
    , module Vgrep.Widget.Results.Internal
    ) where

import qualified Data.List       as List
import qualified Data.Sequence   as Seq
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Test.QuickCheck

import Vgrep.Ansi
import Vgrep.Widget.Results          hiding
    ( fileName
    , lineNumber
    , lineReference
    )
import Vgrep.Widget.Results.Internal

instance Arbitrary Results where
    arbitrary = sized $ \n -> frequency
        [ (1, pure EmptyResults)
        , (n, generateResults) ]


generateResults :: Gen Results
generateResults = sized $ \n -> do
    streamOfResults <- arbitraryGrepResults
    numAs <- choose (0, n)
    numBs <- choose (0, n)
    numDs <- choose (0, n)
    numEs <- choose (0, n)
    let (as,  as') = splitAt numAs streamOfResults
        (bs,  bs') = splitAt numBs as'
        ([c], cs') = splitAt 1     bs'
        (ds,  ds') = splitAt numDs cs'
        (es,  _)   = splitAt numEs ds'
    pure $ Results
        (Seq.fromList as)
        (Seq.fromList bs)
        c
        (Seq.fromList ds)
        (Seq.fromList es)


arbitraryGrepResults :: Gen [FileLineReference]
arbitraryGrepResults = fmap concat . infiniteListOf $ do
    fileName <- arbitraryText
    lineReferences <- do
        matches <- listOf arbitraryFormattedText
        lineNumbers <- maybeLineNumbers (length matches)
        pure (zipWith LineReference lineNumbers matches)
    pure [ FileLineReference (File fileName) lineReference
             | lineReference <- lineReferences ]


arbitraryFormattedText :: Gen (Formatted attr)
arbitraryFormattedText = fmap bare arbitraryText

arbitraryText :: Gen Text
arbitraryText = fmap Text.pack arbitrary

ascendingListOf :: Ord a => Int -> Gen a -> Gen [a]
ascendingListOf len things = sorted (vectorOf len things)

maybeLineNumbers :: Int -> Gen [Maybe Int]
maybeLineNumbers len = arbitrary >>= \case
    Just () -> ascendingListOf len (fmap Just positiveNumber)
    Nothing -> vectorOf len (pure Nothing)

sorted :: (Functor f, Ord a) => f [a] -> f [a]
sorted = fmap List.sort

positiveNumber :: Gen Int
positiveNumber = arbitrary `suchThat` (> 0)
