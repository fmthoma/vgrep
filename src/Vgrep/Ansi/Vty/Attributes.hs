module Vgrep.Ansi.Vty.Attributes
  ( Attr ()
  , combineStyles
  ) where

import Data.Bits               ((.|.))
import Graphics.Vty.Attributes (Attr (..), MaybeDefault (..), defAttr)

-- | Combines two 'Attr's. This differs from 'mappend' from the 'Monoid'
-- instance of 'Attr' in that 'Vty.Style's are combined rather than
-- overwritten.
combineStyles :: Attr -> Attr -> Attr
combineStyles l r = defAttr
    { attrStyle = case (attrStyle l, attrStyle r) of
        (SetTo l', SetTo r') -> SetTo (l' .|. r')
        (l', r')             -> l' <> r'
    , attrForeColor = attrForeColor l <> attrForeColor r
    , attrBackColor = attrBackColor l <> attrBackColor r
    }
