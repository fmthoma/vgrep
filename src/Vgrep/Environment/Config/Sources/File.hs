{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Example config file for 'Vgrep.Environment.Config.defaultConfig':
--
-- > {
-- >   "colors": {
-- >     "lineNumbers" : {
-- >       "foreColor": "blue"
-- >     },
-- >     "lineNumbersHl": {
-- >       "foreColor": "blue",
-- >       "style": "bold"
-- >     },
-- >     "normal": {},
-- >     "normalHl": {
-- >       "style": "bold"
-- >     },
-- >     "fileHeaders": {
-- >       "backColor": "green"
-- >     },
-- >     "selected": {
-- >       "style": "standout"
-- >     }
-- >   },
-- >   "tabstop": 8,
-- >   "editor": "vi"
-- > }
--
-- The JSON keys correspond to the lenses in "Vgrep.Environment.Config", the
-- values for 'Vty.Color' and 'Vty.Style' can be obtained from the corresponding
-- predefined constants in "Graphics.Vty.Attributes".
module Vgrep.Environment.Config.Sources.File
    ( configFromFile
    ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy    as BS
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (unpack)
import qualified Graphics.Vty.Attributes as Vty
import           System.Directory
import           System.IO

import Vgrep.Environment.Config.Monoid


-- | Reads the configuration from a JSON file, by default @~/.vgrep/config@.
--
-- When the config file does not exist, no error is raised. When the config file
-- cannot be parsed, however, ar warning is written to stderr.
configFromFile :: MonadIO io => io ConfigMonoid
configFromFile = liftIO $ do
    configDir <- getAppUserDataDirectory "vgrep"
    let configFile = configDir <> "/config"
    doesFileExist configFile >>= \case
        False -> hPutStrLn stderr configFile >> pure mempty
        True  -> fmap eitherDecode' (BS.readFile configFile) >>= \case
            Right config -> pure config
            Left err     -> do
                hPutStrLn stderr $
                    "Could not parse config file " ++ configFile ++ ":"
                    ++ "\n  " ++ err
                    ++ "\nFalling back to default config."
                pure mempty


instance FromJSON ConfigMonoid where
    parseJSON = withObject "ConfigMonoid" $ \o -> do
        _mcolors  <- o .:? "colors" .!= mempty
        _mtabstop <- fmap First (o .:? "tabstop")
        _meditor  <- fmap First (o .:? "editor")
        pure ConfigMonoid{..}

instance FromJSON ColorsMonoid where
    parseJSON = withObject "ColorsMonoid" $ \o -> do
        _mlineNumbers   <- fmap First (o .:? "lineNumbers")
        _mlineNumbersHl <- fmap First (o .:? "lineNumbersHl")
        _mnormal        <- fmap First (o .:? "normal")
        _mnormalHl      <- fmap First (o .:? "normalHl")
        _mfileHeaders   <- fmap First (o .:? "fileHeaders")
        _mselected      <- fmap First (o .:? "selected")
        pure ColorsMonoid{..}

instance FromJSON Vty.Attr where
    parseJSON = fmap attrToVty . parseJSON


data Attr = Attr
    { foreColor :: Maybe Color
    , backColor :: Maybe Color
    , style     :: Maybe Style
    }

instance FromJSON Attr where
    parseJSON = withObject "Attr" $ \o -> do
        foreColor <- o .:? "foreColor"
        backColor <- o .:? "backColor"
        style     <- o .:? "style"
        pure Attr{..}

attrToVty :: Attr -> Vty.Attr
attrToVty Attr{..} = foldAttrs
    [ fmap (flip Vty.withForeColor . colorToVty) foreColor
    , fmap (flip Vty.withBackColor . colorToVty) backColor
    , fmap (flip Vty.withStyle     . styleToVty) style ]
  where
    foldAttrs = foldr ($) Vty.defAttr . catMaybes


data Color
    = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | BrightBlack
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite

instance FromJSON Color where
    parseJSON = withText "Color" $ \case
        "black"         -> pure Black
        "red"           -> pure Red
        "green"         -> pure Green
        "yellow"        -> pure Yellow
        "blue"          -> pure Blue
        "magenta"       -> pure Magenta
        "cyan"          -> pure Cyan
        "white"         -> pure White
        "brightBlack"   -> pure BrightBlack
        "brightRed"     -> pure BrightRed
        "brightGreen"   -> pure BrightGreen
        "brightYellow"  -> pure BrightYellow
        "brightBlue"    -> pure BrightBlue
        "brightMagenta" -> pure BrightMagenta
        "brightCyan"    -> pure BrightCyan
        "brightWhite"   -> pure BrightWhite
        s               -> fail ("Unknown Color: " <> unpack s)

colorToVty :: Color -> Vty.Color
colorToVty = \case
        Black         -> Vty.black
        Red           -> Vty.red
        Green         -> Vty.green
        Yellow        -> Vty.yellow
        Blue          -> Vty.blue
        Magenta       -> Vty.magenta
        Cyan          -> Vty.cyan
        White         -> Vty.white
        BrightBlack   -> Vty.brightBlack
        BrightRed     -> Vty.brightRed
        BrightGreen   -> Vty.brightGreen
        BrightYellow  -> Vty.brightYellow
        BrightBlue    -> Vty.brightBlue
        BrightMagenta -> Vty.brightMagenta
        BrightCyan    -> Vty.brightCyan
        BrightWhite   -> Vty.brightWhite


data Style
    = Standout
    | Underline
    | ReverseVideo
    | Blink
    | Dim
    | Bold

instance FromJSON Style where
    parseJSON = withText "Style" $ \case
        "standout"     -> pure Standout
        "underline"    -> pure Underline
        "reverseVideo" -> pure ReverseVideo
        "blink"        -> pure Blink
        "dim"          -> pure Dim
        "bold"         -> pure Bold
        s              -> fail ("Unknown Style: " <> unpack s)

styleToVty :: Style -> Vty.Style
styleToVty = \case
    Standout     -> Vty.standout
    Underline    -> Vty.underline
    ReverseVideo -> Vty.reverseVideo
    Blink        -> Vty.blink
    Dim          -> Vty.dim
    Bold         -> Vty.bold
