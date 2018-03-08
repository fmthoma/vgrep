module Vgrep.Parser (
    -- * Parsing @grep@ output
      parseGrepOutput
    , parseLine

    -- ** Re-export
    , FileLineReference
    ) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Maybe
import Data.Text            hiding (takeWhile)
import Prelude              hiding (takeWhile)

import Vgrep.Ansi        (stripAnsi)
import Vgrep.Ansi.Parser (attrChange, parseAnsi)
import Vgrep.Results     (File (..), FileLineReference (..), LineReference (..))


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
-- Just (FileLineReference {_file = File {_fileName = "path/to/file"}, _lineReference = LineReference {_lineNumber = Just 123, _lineText = Text 6 "foobar"}})
--
-- Omitting the line number still produces valid output:
--
-- >>> parseLine "path/to/file:foobar"
-- Just (FileLineReference {_file = File {_fileName = "path/to/file"}, _lineReference = LineReference {_lineNumber = Nothing, _lineText = Text 6 "foobar"}})
--
-- However, an file name must be present:
--
-- >>> parseLine "foobar"
-- Nothing
--
-- ANSI escape codes in the line text are parsed correctly:
--
-- >>> parseLine "path/to/file:foo\ESC[31mbar\ESC[mbaz"
-- Just (FileLineReference {_file = File {_fileName = "path/to/file"}, _lineReference = LineReference {_lineNumber = Nothing, _lineText = Cat 9 [Text 3 "foo",Format 3 (Attr {attrStyle = KeepCurrent, attrForeColor = SetTo (ISOColor 1), attrBackColor = KeepCurrent, attrURL = KeepCurrent}) (Text 3 "bar"),Text 3 "baz"]}})
--
parseLine :: Text -> Maybe FileLineReference
parseLine line = case parseOnly lineParser line of
    Left  _      -> Nothing
    Right result -> Just result

lineParser :: Parser FileLineReference
lineParser = do
    file       <- takeWhile (/= ':') <* char ':'
    lineNumber <- optional (skipMany attrChange *> decimal <* skipMany attrChange <* char ':')
    result     <- takeText
    pure FileLineReference
        { _file = File
            { _fileName = stripAnsi (parseAnsi file) }
        , _lineReference = LineReference
            { _lineNumber = lineNumber
            , _lineText   = parseAnsi result } }
