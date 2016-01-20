{-# LANGUAGE TemplateHaskell #-}
module Vgrep.Config where

import Control.Lens
import Data.Maybe
import Graphics.Vty.Image
import System.Environment

data Config = Config
    { colors :: Colors
    , editor :: String }

data Colors = Colors
    { lineNumber :: Attr
    , fileHeader :: Attr
    , highlight  :: Attr
    , normal     :: Attr }

makeLenses ''Config
makeLenses ''Colors

defaultConfig :: IO Config
defaultConfig = do
    defaultEditor <- lookupEnv "EDITOR"
    pure $ Config
        { colors = Colors
            { lineNumber = defAttr `withForeColor` brightBlack
                                   `withBackColor` black
            , fileHeader = defAttr `withBackColor` green
            , highlight  = defAttr `withStyle` standout
            , normal     = defAttr }
        , editor = fromMaybe "vi" defaultEditor }
