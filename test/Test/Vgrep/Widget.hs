module Test.Vgrep.Widget (test) where

import Test.Tasty

import qualified Test.Vgrep.Widget.Pager as Pager
import qualified Test.Vgrep.Widget.Results as Results

test :: TestTree
test = testGroup "Widgets"
    [ Pager.test
    , Results.test ]
