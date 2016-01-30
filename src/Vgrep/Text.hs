module Vgrep.Text ( expandForDisplay ) where

import Control.Lens
import Data.Char
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Vgrep.Environment
import Vgrep.Type


expandForDisplay :: Monad m => [Text] -> VgrepT m [Text]
expandForDisplay inputLines = do
    tabWidth <- view (config . tabstop)
    pure (map (expandLineForDisplay tabWidth) inputLines)

expandLineForDisplay :: Int -> Text -> Text
expandLineForDisplay tabWidth =
    (T.pack . expandSpecialChars . expandTabs tabWidth . T.unpack)

expandTabs :: Int -> String -> String
expandTabs tabWidth = go 0
  where go pos (c:cs)
            | c == '\t' = let shift = tabWidth - (pos `mod` tabWidth)
                          in  replicate shift ' ' ++ go (pos + shift) cs
            | otherwise = c : go (pos + 1) cs
        go _ [] = []

expandSpecialChars :: String -> String
expandSpecialChars = \case
    c:cs | ord c < 32 -> ['^', chr (ord c + 64)] ++ expandSpecialChars cs
         | otherwise  -> c : expandSpecialChars cs
    []                -> []
