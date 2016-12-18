module Vgrep.Ansi
  ( Formatted ()
  , empty
  , bare
  , format
  , cat
  , mapText

  , renderAnsi
  , stripAnsi
  , parseAnsi

  , Vty.Attr ()
  )where

import           Data.Bits    ((.|.))
import           Data.Text    (Text)
import qualified Graphics.Vty as Vty

import Vgrep.Ansi.Parser
import Vgrep.Ansi.Type


renderAnsi :: Formatted Vty.Attr -> Vty.Image
renderAnsi = go mempty
  where
    go attr = \case
        Empty          -> Vty.emptyImage
        Text t         -> Vty.text' attr t
        Format attr' t -> go (combine attr attr') t
        Cat ts         -> Vty.horizCat (map (go attr) ts)

stripAnsi :: Formatted a -> Text
stripAnsi = \case
    Empty      -> mempty
    Text t     -> t
    Format _ t -> stripAnsi t
    Cat ts     -> foldMap stripAnsi ts

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
