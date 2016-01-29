module Vgrep.Text ( expandForDisplay ) where

import Data.Char
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


expandForDisplay :: Text -> Text
expandForDisplay = T.pack . expandSpecialChars . expandTabs . T.unpack

expandTabs :: String -> String
expandTabs = go 0
  where go pos (c:cs)
            | c == '\t' = let shift = 8 - (pos `mod` 8)
                          in  replicate shift ' ' ++ go (pos + shift) cs
            | otherwise = c : go (pos + 1) cs
        go _ [] = []

expandSpecialChars :: String -> String
expandSpecialChars = \case
    c:cs | ord c < 32 -> ['^', chr (ord c + 64)] ++ expandSpecialChars cs
         | otherwise  -> c : expandSpecialChars cs
    []                -> []
