{-# LANGUAGE FlexibleContexts #-}
module Vgrep.Text (
    -- * Utilities for rendering 'Text'
    -- | Tabs and other characters below ASCII 32 cause problems in
    -- "Graphics.Vty", so we expand them to readable characters, e.g. @\\r@
    -- to @^13@. Tabs are expanded toh the configured 'tabWidth'.
      expandForDisplay
    , expandLineForDisplay
    , expandFormattedLine
    ) where

import           Control.Lens.Compat
import           Control.Monad.Reader.Class
import           Data.Char
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import Vgrep.Ansi
import Vgrep.Environment


-- | Expand a list of lines
expandForDisplay
    :: (Functor f, MonadReader Environment m)
    => f Text -> m (f Text)
expandForDisplay inputLines = do
    tw <- tabWidth
    pure (fmap (expandText tw) inputLines)

-- | Expand a single line
expandLineForDisplay :: MonadReader Environment m => Text -> m Text
expandLineForDisplay inputLine = do
    tw <- tabWidth
    pure (expandText tw inputLine)

-- | Expand an ANSI formatted line
expandFormattedLine :: MonadReader Environment m => Formatted a -> m (Formatted a)
expandFormattedLine inputLine = do
    tw <- tabWidth
    pure (mapTextWithPos (expandTextAt tw . Position) inputLine)


newtype TabWidth = TabWidth Int
newtype Position = Position Int

tabWidth :: MonadReader Environment m => m TabWidth
tabWidth = view (config . tabstop . to TabWidth)

expandText :: TabWidth -> Text -> Text
expandText tw = expandTextAt tw (Position 0)

expandTextAt :: TabWidth -> Position -> Text -> Text
expandTextAt tw pos =
    T.pack . expandSpecialChars . expandTabs tw pos . T.unpack

expandTabs :: TabWidth -> Position -> String -> String
expandTabs (TabWidth tw) (Position p) = go p
  where go pos (c:cs)
            | c == '\t' = let shift = tw - (pos `mod` tw)
                          in  replicate shift ' ' ++ go (pos + shift) cs
            | otherwise = c : go (pos + 1) cs
        go _ [] = []

expandSpecialChars :: String -> String
expandSpecialChars = \case
    c:cs | ord c < 32  -> ['^', chr (ord c + 64)] ++ expandSpecialChars cs
         | otherwise   -> c : expandSpecialChars cs
    []                 -> []
