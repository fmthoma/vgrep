{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Results
    ( File (..)
    , fileName

    , LineReference (..)
    , lineNumber
    , lineText

    , FileLineReference (..)
    , file
    , lineReference
    ) where

import Lens.Micro.Platform
import Data.Text       (Text)

import Vgrep.Ansi (AnsiFormatted)


newtype File = File
    { _fileName :: Text
    } deriving (Eq, Show)

makeLenses ''File

data LineReference = LineReference
    { _lineNumber :: Maybe Int
    , _lineText   :: AnsiFormatted
    } deriving (Eq, Show)

makeLenses ''LineReference

data FileLineReference = FileLineReference
    { _file          :: File
    , _lineReference :: LineReference
    } deriving (Eq, Show)

makeLenses ''FileLineReference
