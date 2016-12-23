-- | Utilities for printing ANSI formatted text.
module Vgrep.Ansi (
  -- * ANSI formatted text
    AnsiFormatted
  , Formatted ()
  -- ** Smart constructors
  , emptyFormatted
  , bare
  , format
  , cat
  -- ** Modifying text nodes
  , mapText
  , mapTextWithPos
  , takeFormatted
  , dropFormatted
  , padFormatted

  -- * Converting ANSI formatted text
  , renderAnsi
  , stripAnsi
  ) where

import           Data.Text    (Text)
import qualified Graphics.Vty as Vty

import Vgrep.Ansi.Type
import Vgrep.Ansi.Vty.Attributes


-- | Converts ANSI formatted text to an 'Vty.Image'. Nested formattings are
-- combined with 'combineStyles'. The given 'Vty.Attr' is used as style for the
-- root of the 'Formatted' tree.
--
-- >>> renderAnsi Vty.defAttr (bare "Text")
-- HorizText "Text"@(Attr {attrStyle = Default, attrForeColor = Default, attrBackColor = Default},4,4)
--
renderAnsi :: Attr -> AnsiFormatted -> Vty.Image
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
