module Vgrep.Parser
    ( parseGrepOutput
    ) where

import Control.Applicative
import Data.Attoparsec.Text.Lazy
import Data.Maybe
import Data.Text.Lazy

import Vgrep.Widget.Results.Buffer (File(..), FileLineReference)

parseGrepOutput :: [Text] -> [FileLineReference]
parseGrepOutput = catMaybes . fmap parseLine

parseLine :: Text -> Maybe FileLineReference
parseLine line = maybeResult (parse lineParser line)

lineParser :: Parser FileLineReference
lineParser = do
    file       <- many (notChar ':') <* char ':'
    lineNumber <- optional (decimal <* char ':')
    result     <- takeLazyText
    return (File (pack file), (lineNumber, result))
