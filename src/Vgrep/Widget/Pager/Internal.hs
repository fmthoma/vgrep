{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Widget.Pager.Internal (
    -- * Pager widget state
      Pager (..)

    -- * Lenses
    , position
    -- ** Auto-generated lenses
    , column
    , above
    , visible
    , highlighted
    ) where

import Control.Lens
import Data.Sequence  (Seq)
import Data.Set       (Set)
import Data.Text.Lazy (Text)


-- | Keeps track of the lines of text to display, the current scroll
-- positions, and the set of highlighted line numbers.
data Pager = Pager
    { _column      :: Int
    , _highlighted :: Set Int
    , _above       :: Seq Text
    , _visible     :: Seq Text }
    deriving (Eq, Show)

makeLenses ''Pager

-- | The number of invisible lines above the screen
position :: Getter Pager Int
position = above . to length
