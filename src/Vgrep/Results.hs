module Vgrep.Results
    ( File (..)
    , LineReference (..)
    , FileLineReference (..)
    ) where

import Data.Text (Text)


newtype File = File
    { getFileName :: Text
    } deriving (Eq, Show)

data LineReference = LineReference
    { getLineNumber :: Maybe Int
    , getLineText   :: Text
    } deriving (Eq, Show)

data FileLineReference = FileLineReference
    { getFile          :: File
    , getLineReference :: LineReference
    } deriving (Eq, Show)
