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

import Control.Lens.Compat
import Data.IntMap.Strict  (IntMap)
import Data.Sequence       (Seq)
import Data.Text           (Text)

import Vgrep.Ansi


-- | Keeps track of the lines of text to display, the current scroll
-- positions, and the set of highlighted line numbers.
data Pager = Pager
    { _column      :: Int
    -- ^ The current column offset for horizontal scrolling

    , _highlighted :: IntMap (Formatted Attr)
    -- ^ Set of line numbers that are highlighted (i.e. they contain matches)

    , _above       :: Seq Text
    -- ^ Zipper: Lines above the screen

    , _visible     :: Seq Text
    -- ^ Zipper: Lines on screen and below

    } deriving (Eq, Show)

makeLenses ''Pager

-- | The number of invisible lines above the screen
position :: Getter Pager Int
position = above . to length
