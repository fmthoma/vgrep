module Vgrep.Ansi
  ( renderAnsi
  , stripAnsi
  , parseAnsi
  , Vty.Attr ()
  , module Vgrep.Ansi.Type
  )where

import           Data.Bits    ((.|.))
import           Data.Text    (Text)
import qualified Graphics.Vty as Vty

import Vgrep.Ansi.Parser
import Vgrep.Ansi.Type


renderAnsi :: Formatted Vty.Attr -> Vty.Image
renderAnsi = \case
    Node attr ts -> Vty.horizCat (fmap (renderAnsi . fmap (combine attr)) ts)
    Leaf attr t  -> Vty.text' attr t

stripAnsi :: Formatted a -> Text
stripAnsi = \case
    Node _ ts -> foldMap stripAnsi ts
    Leaf _ t  -> t

-- | Combines two 'Vty.Attr's. This differs from 'mappend' from the 'Monoid'
-- instance of 'Vty.Attr' in that 'Vty.Style's are combined rather than
-- overwritten.
combine :: Vty.Attr -> Vty.Attr -> Vty.Attr
combine l r = Vty.Attr
    { Vty.attrStyle = case (Vty.attrStyle l, Vty.attrStyle r) of
        (Vty.SetTo l', Vty.SetTo r') -> Vty.SetTo (l' .|. r')
        (l', r')                     -> mappend l' r'
    , Vty.attrForeColor = mappend (Vty.attrForeColor l) (Vty.attrForeColor r)
    , Vty.attrBackColor = mappend (Vty.attrBackColor l) (Vty.attrBackColor r)
    }
