{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Vgrep.Environment.Config.Sources.File
    ( configFromFile
    , Attr
    , Color
    , Style
    ) where

import           Control.Monad.IO.Class
import           Data.Aeson              (withObject, withText)
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (unpack)
import           Data.Yaml.Aeson
import qualified Graphics.Vty.Attributes as Vty
import           System.Directory
import           System.IO

import Vgrep.Environment.Config.Monoid

-- $setup
-- >>> import Data.List (isInfixOf)


{- |
Reads the configuration from a JSON or YAML file. The file should be
located in one of the following places:

* @~\/.vgrep\/config.yaml@,
* @~\/.vgrep\/config.yml@,
* @~\/.vgrep\/config.json@ or
* @~\/.vgrep\/config@.

When none of these files exist, no error is raised. When a file exists, but
cannot be parsed, a warning is written to stderr.

Supported formats are JSON and YAML. The example YAML config given in the
project directory (@config.yaml.example@) is equivalent to the default
config:

>>> import qualified Vgrep.Environment.Config as C
>>> Right config <- decodeFileEither "config.yaml.example" :: IO (Either ParseException ConfigMonoid)
>>> C.fromConfigMonoid config == C.defaultConfig
True

Example YAML config file for 'Vgrep.Environment.Config.defaultConfig':

> colors:
>   line-numbers:
>     fore-color: blue
>   line-numbers-hl:
>     fore-color: blue
>     style: bold
>   normal:
>   normal-hl:
>     style: bold
>   file-headers:
>     back-color: green
>   selected:
>     style: standout
> tabstop: 8
> editor: "vi"

Example JSON file for the same config:

> {
>   "colors": {
>     "line-numbers" : {
>       "fore-color": "blue"
>     },
>     "line-numbers-hl": {
>       "fore-color": "blue",
>       "style": "bold"
>     },
>     "normal": {},
>     "normal-hl": {
>       "style": "bold"
>     },
>     "file-headers": {
>       "back-color": "green"
>     },
>     "selected": {
>       "style": "standout"
>     }
>   },
>   "tabstop": 8,
>   "editor": "vi"
> }

The JSON/YAML keys correspond to the lenses in "Vgrep.Environment.Config",
the values for 'Vty.Color' and 'Vty.Style' can be obtained from the
corresponding predefined constants in "Graphics.Vty.Attributes".
-}
configFromFile :: MonadIO io => io ConfigMonoid
configFromFile = liftIO $ do
    configDir <- getAppUserDataDirectory "vgrep"
    let configFiles = map (configDir </>)
            [ "config.yaml"
            , "config.yml"
            , "config.json"
            , "config" ]
    findExistingFile configFiles >>= \case
        Nothing         -> pure mempty
        Just configFile -> decodeFileEither configFile >>= \case
            Right config -> pure config
            Left err     -> do
                hPutStrLn stderr $
                    "Could not parse config file " ++ configFile ++ ":"
                    ++ "\n" ++ prettyPrintParseException err
                    ++ "\nFalling back to default config."
                pure mempty
  where
    findExistingFile :: [FilePath] -> IO (Maybe FilePath)
    findExistingFile = \case
        [] -> pure Nothing
        f : fs -> do
            exists <- doesFileExist f
            if exists then pure (Just f) else findExistingFile fs

    (</>) :: FilePath -> FilePath -> FilePath
    dir </> file = dir <> "/" <> file


instance FromJSON ConfigMonoid where
    parseJSON = withObject "ConfigMonoid" $ \o -> do
        _mcolors  <- o .:? "colors" .!= mempty
        _mtabstop <- fmap First (o .:? "tabstop")
        _meditor  <- fmap First (o .:? "editor")
        pure ConfigMonoid{..}

instance FromJSON ColorsMonoid where
    parseJSON = withObject "ColorsMonoid" $ \o -> do
        _mlineNumbers   <- fmap First (o .:? "line-numbers")
        _mlineNumbersHl <- fmap First (o .:? "line-numbers-hl")
        _mnormal        <- fmap First (o .:? "normal")
        _mnormalHl      <- fmap First (o .:? "normal-hl")
        _mfileHeaders   <- fmap First (o .:? "file-headers")
        _mselected      <- fmap First (o .:? "selected")
        pure ColorsMonoid{..}

instance FromJSON Vty.Attr where
    parseJSON = fmap attrToVty . parseJSON


{- |
A JSON-parsable data type for 'Vty.Attr'.

JSON example:

>>> decodeEither "{\"fore-color\": \"black\", \"style\": \"standout\"}" :: Either String Attr
Right (Attr {foreColor = Just Black, backColor = Nothing, style = Just Standout})

JSON example without quotes:
>>> decodeEither "{fore-color: black, style: standout}" :: Either String Attr
Right (Attr {foreColor = Just Black, backColor = Nothing, style = Just Standout})

YAML example:

>>> :{
>>> decodeEither
>>>   $  "fore-color: \"blue\"\n"
>>>   <> "back-color: \"bright-blue\"\n"
>>>   <> "style: \"reverse-video\"\n"
>>>   :: Either String Attr
>>> :}
Right (Attr {foreColor = Just Blue, backColor = Just BrightBlue, style = Just ReverseVideo})

YAML example without quotes:

>>> :{
>>> decodeEither
>>>   $  "fore-color: blue\n"
>>>   <> "back-color: bright-blue\n"
>>>   <> "style: reverse-video\n"
>>>   :: Either String Attr
>>> :}
Right (Attr {foreColor = Just Blue, backColor = Just BrightBlue, style = Just ReverseVideo})
-}
data Attr = Attr
    { foreColor :: Maybe Color
    , backColor :: Maybe Color
    , style     :: Maybe Style
    }
    deriving (Eq, Show)

instance FromJSON Attr where
    parseJSON = withObject "Attr" $ \o -> do
        foreColor <- o .:? "fore-color"
        backColor <- o .:? "back-color"
        style     <- o .:? "style"
        pure Attr{..}

attrToVty :: Attr -> Vty.Attr
attrToVty Attr{..} = foldAttrs
    [ fmap (flip Vty.withForeColor . colorToVty) foreColor
    , fmap (flip Vty.withBackColor . colorToVty) backColor
    , fmap (flip Vty.withStyle     . styleToVty) style ]
  where
    foldAttrs = foldr ($) Vty.defAttr . catMaybes


{- |
A JSON-parsable data type for 'Vty.Color'.

>>> decodeEither "[\"black\",\"red\",\"bright-black\"]" :: Either String [Color]
Right [Black,Red,BrightBlack]

Also works without quotes:

>>> decodeEither "[black,red,bright-black]" :: Either String [Color]
Right [Black,Red,BrightBlack]

Fails with error message if the 'Color' cannot be parsed:

>>> let Left err = decodeEither "foo" :: Either String Color
>>> "Unknown Color: foo" `isInfixOf` err
True
-}
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
    deriving (Eq, Show)

instance FromJSON Color where
    parseJSON = withText "Color" $ \case
        "black"          -> pure Black
        "red"            -> pure Red
        "green"          -> pure Green
        "yellow"         -> pure Yellow
        "blue"           -> pure Blue
        "magenta"        -> pure Magenta
        "cyan"           -> pure Cyan
        "white"          -> pure White
        "bright-black"   -> pure BrightBlack
        "bright-red"     -> pure BrightRed
        "bright-green"   -> pure BrightGreen
        "bright-yellow"  -> pure BrightYellow
        "bright-blue"    -> pure BrightBlue
        "bright-magenta" -> pure BrightMagenta
        "bright-cyan"    -> pure BrightCyan
        "bright-white"   -> pure BrightWhite
        s                -> fail ("Unknown Color: " <> unpack s)

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


{- |
A JSON-parsable data type for 'Vty.Style'.

>>> decodeEither "[\"standout\", \"underline\", \"bold\"]" :: Either String [Style]
Right [Standout,Underline,Bold]

Also works without quotes:

>>> decodeEither "[standout, underline, bold]" :: Either String [Style]
Right [Standout,Underline,Bold]

Fails with error message if the 'Style' cannot be parsed:

>>> let Left err = decodeEither "foo" :: Either String Style
>>> "Unknown Style: foo" `isInfixOf` err
True
-}
data Style
    = Standout
    | Underline
    | ReverseVideo
    | Blink
    | Dim
    | Bold
    deriving (Eq, Show)

instance FromJSON Style where
    parseJSON = withText "Style" $ \case
        "standout"      -> pure Standout
        "underline"     -> pure Underline
        "reverse-video" -> pure ReverseVideo
        "blink"         -> pure Blink
        "dim"           -> pure Dim
        "bold"          -> pure Bold
        s               -> fail ("Unknown Style: " <> unpack s)

styleToVty :: Style -> Vty.Style
styleToVty = \case
    Standout     -> Vty.standout
    Underline    -> Vty.underline
    ReverseVideo -> Vty.reverseVideo
    Blink        -> Vty.blink
    Dim          -> Vty.dim
    Bold         -> Vty.bold
