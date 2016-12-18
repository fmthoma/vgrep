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

import Data.Text (Text)
import Control.Lens.TH

import Vgrep.Ansi (Formatted, Attr)


newtype File = File
    { _fileName :: Text
    } deriving (Eq, Show)

makeLenses ''File

data LineReference = LineReference
    { _lineNumber :: Maybe Int
    , _lineText   :: Formatted Attr
    } deriving (Eq, Show)

makeLenses ''LineReference

data FileLineReference = FileLineReference
    { _file          :: File
    , _lineReference :: LineReference
    } deriving (Eq, Show)

makeLenses ''FileLineReference
