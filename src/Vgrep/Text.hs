{-# LANGUAGE FlexibleContexts #-}
module Vgrep.Text (
    -- * Utilities for rendering 'Text'
    -- | Tabs and other characters below ASCII 32 cause problems in
    -- "Graphics.Vty", so we expand them to readable characters, e.g. @\\r@
    -- to @^13@. Tabs are expanded toh the configured 'tabWidth'.
      expandForDisplay
    , expandLineForDisplay
    ) where

import           Control.Lens.Compat
import           Control.Monad.Reader.Class
import           Data.Char
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import Vgrep.Environment


-- | Expand a list of lines
expandForDisplay
    :: (Functor f, MonadReader Environment m)
    => f Text -> m (f Text)
expandForDisplay inputLines = do
    tabWidth <- view (config . tabstop)
    pure (fmap (expandText tabWidth) inputLines)

-- | Expand a single line
expandLineForDisplay :: MonadReader Environment m => Text -> m Text
expandLineForDisplay inputLine = do
    tabWidth <- view (config . tabstop)
    pure (expandText tabWidth inputLine)

expandText :: Int -> Text -> Text
expandText tabWidth =
    T.pack . expandSpecialChars . expandTabs tabWidth . T.unpack

expandTabs :: Int -> String -> String
expandTabs tabWidth = go 0
  where go pos (c:cs)
            | c == '\t' = let shift = tabWidth - (pos `mod` tabWidth)
                          in  replicate shift ' ' ++ go (pos + shift) cs
            | otherwise = c : go (pos + 1) cs
        go _ [] = []

expandSpecialChars :: String -> String
expandSpecialChars = \case
    cs@('\ESC':'[':_)  -> cs -- Keep escape sequences, to be parsed by ANSI parser
    c:cs | ord c < 32  -> ['^', chr (ord c + 64)] ++ expandSpecialChars cs
         | otherwise   -> c : expandSpecialChars cs
    []                 -> []
