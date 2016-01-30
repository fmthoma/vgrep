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
    { _lineNumbers :: Attr
    , _fileHeaders :: Attr
    , _highlight   :: Attr
    , _normal      :: Attr }

makeLenses ''Config
makeLenses ''Colors

defaultConfig :: IO Config
defaultConfig = do
    defaultEditor <- lookupEnv "EDITOR"
    pure $ Config
        { _colors = Colors
            { _lineNumbers = defAttr `withForeColor` blue
            , _fileHeaders = defAttr `withBackColor` green
            , _highlight   = defAttr `withStyle` standout
            , _normal      = defAttr }
        , _tabstop = 8
        , _editor = fromMaybe "vi" defaultEditor }
