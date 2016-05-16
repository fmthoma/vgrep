{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Pager.Internal (
    -- * Pager widget state
      Pager (..)

    -- * Auto-generated lenses
    , position
    , column
    , above
    , visible
    , highlighted
    ) where

import Control.Lens
import Data.Sequence
import Data.Set       (Set)
import Data.Text.Lazy (Text)


-- | Keeps track of the lines of text to display, the current scroll
-- positions, and the set of highlighted line numbers.
data Pager = Pager
    { _position    :: Int
    , _column      :: Int
    , _highlighted :: Set Int
    , _above       :: Seq Text
    , _visible     :: Seq Text }
    deriving (Eq, Show)

makeLenses ''Pager
