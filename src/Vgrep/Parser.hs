module Vgrep.Parser
    ( parseGrepOutput
    ) where

import Control.Applicative
import Data.Attoparsec.Text.Lazy
import Data.Maybe
import Data.Text.Lazy

parseGrepOutput :: [Text] -> [(Text, Maybe Int, Text)]
parseGrepOutput = catMaybes . fmap parseLine

parseLine :: Text -> Maybe (Text, Maybe Int, Text)
parseLine line = maybeResult (parse lineParser line)

lineParser :: Parser (Text, Maybe Int, Text)
lineParser = do
    file       <- many (notChar ':') <* char ':'
    lineNumber <- optional (decimal <* char ':')
    result     <- takeLazyText
    return (pack file, lineNumber, result)
