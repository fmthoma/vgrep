import Test.Tasty

import qualified Test.Vgrep.Widget as Widget

main :: IO ()
main = defaultMain $
    testGroup "Unit tests" [ Widget.test ]
