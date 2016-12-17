module Vgrep.Results
    ( File (..)
    , LineReference (..)
    , FileLineReference (..)
    ) where

import Data.Text (Text)

import Vgrep.Ansi (Formatted, Attr)


newtype File = File
    { getFileName :: Text
    } deriving (Eq, Show)

data LineReference = LineReference
    { getLineNumber :: Maybe Int
    , getLineText   :: Formatted Attr
    } deriving (Eq, Show)

data FileLineReference = FileLineReference
    { getFile          :: File
    , getLineReference :: LineReference
    } deriving (Eq, Show)
