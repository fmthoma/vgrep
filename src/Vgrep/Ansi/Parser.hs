{-# LANGUAGE OverloadedStrings #-}
module Vgrep.Ansi.Parser
  ( parseAnsi
  , ansiFormatted
  , attrChange
  ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Graphics.Vty.Attributes (Attr)
import qualified Graphics.Vty.Attributes as Vty

import Vgrep.Ansi.Type


{- |
Directly parses ANSI formatted text using 'ansiFormatted'.

Parsing ANSI color codes:

>>> parseAnsi "Hello \ESC[31mWorld\ESC[m!"
Cat 12 [Text 6 "Hello ",Format 5 (Attr {attrStyle = KeepCurrent, attrForeColor = SetTo (ISOColor 1), attrBackColor = KeepCurrent}) (Text 5 "World"),Text 1 "!"]

More elaborate example with nested foreground and background colors:

>>> parseAnsi "\ESC[m\ESC[40mHello \ESC[31mWorld\ESC[39m!"
Cat 12 [Format 6 (Attr {attrStyle = KeepCurrent, attrForeColor = KeepCurrent, attrBackColor = SetTo (ISOColor 0)}) (Text 6 "Hello "),Format 5 (Attr {attrStyle = KeepCurrent, attrForeColor = SetTo (ISOColor 1), attrBackColor = SetTo (ISOColor 0)}) (Text 5 "World"),Format 1 (Attr {attrStyle = KeepCurrent, attrForeColor = KeepCurrent, attrBackColor = SetTo (ISOColor 0)}) (Text 1 "!")]

Some CSI sequences are ignored, since they are not supported by 'Vty':

>>> parseAnsi "\ESC[A\ESC[B\ESC[31mfoo\ESC[1K\ESC[mbar"
Cat 6 [Format 3 (Attr {attrStyle = KeepCurrent, attrForeColor = SetTo (ISOColor 1), attrBackColor = KeepCurrent}) (Text 3 "foo"),Text 3 "bar"]

Non-CSI sequences are not parsed, but included in the output:

>>> parseAnsi "\ESC]710;font\007foo\ESC[31mbar"
Cat 17 [Text 14 "\ESC]710;font\afoo",Format 3 (Attr {attrStyle = KeepCurrent, attrForeColor = SetTo (ISOColor 1), attrBackColor = KeepCurrent}) (Text 3 "bar")]

-}
parseAnsi :: Text -> AnsiFormatted
parseAnsi = either error id . parseOnly ansiFormatted
-- The use of 'error' ↑ is safe: 'ansiFormatted' does not fail.


-- | Parser for ANSI formatted text. Recognized escape sequences are the SGR
-- (Select Graphic Rendition) sequences (@\ESC[…m@) supported by 'Attr'.
-- Unsupported SGR sequences and other CSI escape sequences (@\ESC[…@) are
-- ignored. Other (non-CSI) escape sequences are not parsed, and included in the
-- output.
--
-- This parser does not fail, it will rather consume and return the remaining
-- input as unformatted text.
ansiFormatted :: Parser AnsiFormatted
ansiFormatted = go mempty
  where
    go :: Attr -> Parser AnsiFormatted
    go attr = endOfInput *> pure mempty
          <|> formattedText attr

    formattedText :: Attr -> Parser AnsiFormatted
    formattedText attr = do
        acs <- many attrChange
        let attr' = foldr ($) attr (reverse acs)
        t <- rawText
        rest <- go attr'
        pure (format attr' (bare t) <> rest)

    rawText :: Parser Text
    rawText = atLeastOneTill (== '\ESC') <|> endOfInput *> pure ""

    atLeastOneTill :: (Char -> Bool) -> Parser Text
    atLeastOneTill = liftA2 T.cons anyChar . takeTill


-- | Parser for ANSI CSI escape sequences. Recognized escape sequences are the
-- SGR (Select Graphic Rendition) sequences (@\ESC[…m@) supported by 'Attr'.
-- Unsupported SGR sequences and other CSI escape sequences (@\ESC[…@) are
-- ignored by returning 'id'.
--
-- This parser fails when encountering any other (non-CSI) escape sequence.
attrChange :: Parser (Attr -> Attr)
attrChange = fmap csiToAttrChange csi

csiEscape :: Parser Text
csiEscape = "\ESC["

csi :: Parser Csi
csi = csiEscape >> liftA2 Csi (decimal `sepBy` char ';') anyChar

data Csi = Csi [Int] Char

csiToAttrChange :: Csi -> Attr -> Attr
csiToAttrChange = \case
    Csi [] 'm' -> const mempty
    Csi is 'm' -> foldMap attrChangeFromCode is
    _otherwise -> id

attrChangeFromCode :: Int -> Attr -> Attr
attrChangeFromCode = \case
    0  -> const mempty
    1  -> withStyle Vty.bold
    3  -> withStyle Vty.standout
    4  -> withStyle Vty.underline
    5  -> withStyle Vty.blink
    6  -> withStyle Vty.blink
    7  -> withStyle Vty.reverseVideo
    21 -> withoutStyle Vty.bold
    22 -> withoutStyle Vty.bold
    23 -> withoutStyle Vty.standout
    24 -> withoutStyle Vty.underline
    25 -> withoutStyle Vty.blink
    27 -> withoutStyle Vty.reverseVideo
    i | i >= 30  && i <= 37   -> withForeColor (rawColor (i - 30))
      | i >= 40  && i <= 47   -> withBackColor (rawColor (i - 40))
      | i >= 90  && i <= 97   -> withForeColor (rawBrightColor (i - 90))
      | i >= 100 && i <= 107  -> withBackColor (rawBrightColor (i - 100))
    39 -> resetForeColor
    49 -> resetBackColor
    _  -> id
  where
    rawColor = \case
        0 -> Vty.black
        1 -> Vty.red
        2 -> Vty.green
        3 -> Vty.yellow
        4 -> Vty.blue
        5 -> Vty.magenta
        6 -> Vty.cyan
        _ -> Vty.white
    rawBrightColor = \case
        0 -> Vty.brightBlack
        1 -> Vty.brightRed
        2 -> Vty.brightGreen
        3 -> Vty.brightYellow
        4 -> Vty.brightBlue
        5 -> Vty.brightMagenta
        6 -> Vty.brightCyan
        _ -> Vty.brightWhite
    withStyle = flip Vty.withStyle
    withForeColor = flip Vty.withForeColor
    withBackColor = flip Vty.withBackColor
    withoutStyle style attr = case Vty.attrStyle attr of
        Vty.SetTo oldStyle | oldStyle `Vty.hasStyle` style
                   -> attr { Vty.attrStyle = Vty.SetTo (oldStyle .&. complement style) }
        _otherwise -> attr
    resetForeColor attr = attr { Vty.attrForeColor = Vty.KeepCurrent }
    resetBackColor attr = attr { Vty.attrBackColor = Vty.KeepCurrent }
