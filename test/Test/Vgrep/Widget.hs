module Test.Vgrep.Widget (test) where

import Test.Tasty

import qualified Test.Vgrep.Widget.Pager as Pager

test :: TestTree
test = testGroup "Widgets"
    [ Pager.test ]
