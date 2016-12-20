-- | Utilities for printing ANSI formatted text.
module Vgrep.Ansi (
  -- * ANSI formatted text
    Formatted ()
  -- ** Smart constructors
  , empty
  , bare
  , format
  , cat
  -- ** Modifying text nodes
  , mapText
  , mapTextWithPos

  -- * Converting ANSI formatted text
  , renderAnsi
  , stripAnsi

  -- * Attributes
  , Vty.Attr ()
  , combineStyles
  )where

import           Data.Bits    ((.|.))
import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import qualified Graphics.Vty as Vty

import Vgrep.Ansi.Type


-- | Converts ANSI formatted text to an 'Vty.Image'. Nested formattings are
-- combined with 'combineStyles'. The given 'Vty.Attr' is used as style for the
-- root of the 'Formatted' tree.
--
-- >>> renderAnsi Vty.defAttr (bare "Text")
-- HorizText "Text"@(Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default},4,4)
--
renderAnsi :: Vty.Attr -> Formatted Vty.Attr -> Vty.Image
renderAnsi attr = \case
    Empty            -> Vty.emptyImage
    Text _ t         -> Vty.text' attr t
    Format _ attr' t -> renderAnsi (combineStyles attr attr') t
    Cat _ ts         -> Vty.horizCat (map (renderAnsi attr) ts)

-- | Strips away all formattings to plain 'Text'.
stripAnsi :: Formatted a -> Text
stripAnsi = \case
    Empty        -> mempty
    Text _ t     -> t
    Format _ _ t -> stripAnsi t
    Cat _ ts     -> foldMap stripAnsi ts

-- | Combines two 'Vty.Attr's. This differs from 'mappend' from the 'Monoid'
-- instance of 'Vty.Attr' in that 'Vty.Style's are combined rather than
-- overwritten.
combineStyles :: Vty.Attr -> Vty.Attr -> Vty.Attr
combineStyles l r = Vty.Attr
    { Vty.attrStyle = case (Vty.attrStyle l, Vty.attrStyle r) of
        (Vty.SetTo l', Vty.SetTo r') -> Vty.SetTo (l' .|. r')
        (l', r')                     -> l' <> r'
    , Vty.attrForeColor = Vty.attrForeColor l <> Vty.attrForeColor r
    , Vty.attrBackColor = Vty.attrBackColor l <> Vty.attrBackColor r
    }
