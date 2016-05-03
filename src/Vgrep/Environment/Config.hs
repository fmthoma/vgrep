{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Environment.Config where

import Control.Lens
import Data.Maybe
import Graphics.Vty.Image
import System.Environment

data Config = Config
    { _colors  :: Colors
    , _tabstop :: Int
    , _editor  :: String }

data Colors = Colors
    { _lineNumbers   :: Attr
    , _lineNumbersHl :: Attr
    , _normal        :: Attr
    , _normalHl      :: Attr
    , _fileHeaders   :: Attr
    , _selected      :: Attr }

makeLenses ''Config
makeLenses ''Colors

defaultConfig :: IO Config
defaultConfig = do
    defaultEditor <- lookupEnv "EDITOR"
    pure Config
        { _colors = Colors
            { _lineNumbers   = defAttr `withForeColor` blue
            , _lineNumbersHl = defAttr `withForeColor` blue
                                       `withStyle` bold
            , _normal        = defAttr
            , _normalHl      = defAttr `withStyle` bold
            , _fileHeaders   = defAttr `withBackColor` green
            , _selected      = defAttr `withStyle` standout }
        , _tabstop = 8
        , _editor = fromMaybe "vi" defaultEditor }
