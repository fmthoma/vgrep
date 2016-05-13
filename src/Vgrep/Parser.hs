module Vgrep.Parser (
    -- * Parsing @grep@ output
      parseGrepOutput
    , parseLine

    -- ** Re-export
    , FileLineReference
    ) where

import Control.Applicative
import Data.Attoparsec.Text.Lazy
import Data.Maybe
import Data.Text.Lazy

import Vgrep.Results


-- | Parses lines of 'Text', skipping lines that are not valid @grep@
-- output.
parseGrepOutput :: [Text] -> [FileLineReference]
parseGrepOutput = catMaybes . fmap parseLine

-- | Parses a line of @grep@ output. Returns 'Nothing' if the line cannot
-- be parsed.
--
-- The output should consist of a file name, line number and the content,
-- separated by colons:
--
-- >>> parseLine "path/to/file:123:foobar"
-- Just (FileLineReference {getFile = File {getFileName = "path/to/file"}, getLineReference = LineReference {getLineNumber = Just 123, getLineText = "foobar"}})
--
-- Omitting the line number still produces valid output:
--
-- >>> parseLine "path/to/file:foobar"
-- Just (FileLineReference {getFile = File {getFileName = "path/to/file"}, getLineReference = LineReference {getLineNumber = Nothing, getLineText = "foobar"}})
--
-- However, an file name must be present:
--
-- >>> parseLine "foobar"
-- Nothing
parseLine :: Text -> Maybe FileLineReference
parseLine line = maybeResult (parse lineParser line)

lineParser :: Parser FileLineReference
lineParser = do
    file       <- manyTill anyChar (char ':')
    lineNumber <- optional (decimal <* char ':')
    result     <- takeLazyText
    pure FileLineReference
        { getFile = File
            { getFileName = pack file }
        , getLineReference = LineReference
            { getLineNumber = lineNumber
            , getLineText   = result } }
