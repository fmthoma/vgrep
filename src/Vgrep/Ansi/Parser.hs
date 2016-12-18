{-# LANGUAGE OverloadedStrings #-}
module Vgrep.Ansi.Parser
  ( parseAnsi
  , ansiFormatted
  , attrChange
  , module Vgrep.Ansi.Type
  ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Monoid
import           Data.Text               (Text)
import           Graphics.Vty.Attributes (Attr)
import qualified Graphics.Vty.Attributes as Vty

import Vgrep.Ansi.Type


parseAnsi :: Text -> Formatted Attr
parseAnsi = either error id . parseOnly ansiFormatted


ansiFormatted :: Parser (Formatted Attr)
ansiFormatted = go mempty
  where
    go attr = endOfInput *> pure mempty
          <|> formattedText attr
          <|> unformattedText attr
    formattedText attr = do
        ac <- attrChange
        let attr' = ac attr
        t  <- rawText
        rest <- go attr'
        pure (Format attr' (Text t) <> rest)
    unformattedText attr = do
        t <- rawText
        rest <- go attr
        pure (Format attr (Text t) <> rest)
    rawText = takeTill (== '\ESC') <|> takeText


attrChange :: Parser (Attr -> Attr)
attrChange = fmap csiToAttrChange csi

csiEscape :: Parser Text
csiEscape = "\ESC["

csi :: Parser Csi
csi = csiEscape >> liftA2 Csi (decimal `sepBy` char ';') anyChar

data Csi = Csi [Int] Char

csiToAttrChange :: Csi -> (Attr -> Attr)
csiToAttrChange = \case
    Csi [] 'm' -> const mempty
    Csi is 'm' -> foldMap attrChangeFromCode is
    _otherwise -> id

attrChangeFromCode :: Int -> (Attr -> Attr)
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
