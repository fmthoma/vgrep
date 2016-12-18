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
    tabWidth <- view (config . tabstop)
    pure (fmap (expandText tabWidth 0) inputLines)

-- | Expand a single line
expandLineForDisplay :: MonadReader Environment m => Text -> m Text
expandLineForDisplay inputLine = do
    tabWidth <- view (config . tabstop)
    pure (expandText tabWidth 0 inputLine)

-- | Expand an ANSI formatted line
expandFormattedLine :: MonadReader Environment m => Formatted a -> m (Formatted a)
expandFormattedLine inputLine = do
    tabWidth <- view (config . tabstop)
    pure (mapTextWithPos (expandText tabWidth) inputLine)

expandText :: Int -> Int -> Text -> Text
expandText tabWidth pos =
    T.pack . expandSpecialChars . expandTabs tabWidth pos . T.unpack

expandTabs :: Int -> Int -> String -> String
expandTabs tabWidth = go
  where go pos (c:cs)
            | c == '\t' = let shift = tabWidth - (pos `mod` tabWidth)
                          in  replicate shift ' ' ++ go (pos + shift) cs
            | otherwise = c : go (pos + 1) cs
        go _ [] = []

expandSpecialChars :: String -> String
expandSpecialChars = \case
    c:cs | ord c < 32  -> ['^', chr (ord c + 64)] ++ expandSpecialChars cs
         | otherwise   -> c : expandSpecialChars cs
    []                 -> []
